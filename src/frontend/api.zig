const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");
const semantics = @import("semantics.zig");
const abi_builder = @import("../backend/abi_builder.zig");

pub const DiagnosticKind = enum { semantic };

pub const Diagnostic = struct {
    kind: DiagnosticKind,
    message: []const u8,
};

pub const AnalysisResult = struct {
    tree: ast.Tree,
    diagnostics: []const Diagnostic,
    allocator: std.mem.Allocator,
    diagnostics_owned: bool,
    metadata: semantics.Metadata,
    abis: []ContractAbi,

    pub fn deinit(self: *AnalysisResult) void {
        if (self.diagnostics_owned) {
            for (self.diagnostics) |diag| {
                self.allocator.free(diag.message);
            }
            self.allocator.free(self.diagnostics);
        }
        self.metadata.deinit(self.allocator);
        for (self.abis) |abi| {
            self.allocator.free(abi.name);
            self.allocator.free(abi.json);
        }
        if (self.abis.len != 0) self.allocator.free(self.abis);
        self.tree.deinit();
    }
};

pub const ContractAbi = struct {
    name: []u8,
    json: []u8,
};

pub fn analyzeSource(allocator: std.mem.Allocator, source: []const u8) !AnalysisResult {
    var tree = try parser.parseModule(allocator, source);
    errdefer tree.deinit();

    var diagnostics: []const Diagnostic = &[_]Diagnostic{};
    var owns_diagnostics = false;
    var metadata = semantics.Metadata{};
    var abi_list: std.ArrayList(ContractAbi) = .empty;
    var abi_cleanup = true;
    defer if (abi_cleanup) {
        for (abi_list.items) |abi| {
            allocator.free(abi.name);
            allocator.free(abi.json);
        }
        abi_list.deinit();
    };

    if (semantics.analyze(allocator, &tree, &metadata)) |_| {} else |err| switch (err) {
        error.OutOfMemory => return err,
        else => {
            const message = try std.fmt.allocPrint(allocator, "semantic error: {s}", .{semantics.describeError(err)});
            var diag_slice = try allocator.alloc(Diagnostic, 1);
            diag_slice[0] = .{ .kind = .semantic, .message = message };
            diagnostics = diag_slice;
            owns_diagnostics = true;
        },
    }

    const module = tree.getModule();
    var abi_builder_inst = abi_builder.AbiBuilder.init(allocator);
    for (module.contracts) |contract| {
        const abi_json = abi_builder_inst.buildContract(contract) catch |err| switch (err) {
            abi_builder.AbiBuilderError.OutOfMemory => return err,
            else => continue,
        };
        const name_copy = allocator.dupe(u8, contract.name) catch |dup_err| {
            allocator.free(abi_json);
            return dup_err;
        };
        abi_list.append(.{ .name = name_copy, .json = abi_json }) catch |append_err| {
            allocator.free(name_copy);
            allocator.free(abi_json);
            return append_err;
        };
    }

    const abi_slice = abi_list.toOwnedSlice() catch |err| switch (err) {
        error.OutOfMemory => return err,
    };
    abi_cleanup = false;

    return AnalysisResult{
        .tree = tree,
        .diagnostics = diagnostics,
        .allocator = allocator,
        .diagnostics_owned = owns_diagnostics,
        .metadata = metadata,
        .abis = abi_slice,
    };
}

const testing = std.testing;

test "analyze source with valid module" {
    const src =
        "contract A {\n" ++
        "    state balance: u64;\n" ++
        "    fn get() -> u64 {\n" ++
        "        return 0;\n" ++
        "    }\n" ++
        "}\n" ++
        "contract B {\n" ++
        "    state count: u32;\n" ++
        "}\n";

    var result = try analyzeSource(testing.allocator, src);
    defer result.deinit();
    try testing.expectEqual(@as(usize, 0), result.diagnostics.len);
    try testing.expectEqual(@as(usize, 2), result.abis.len);
    try testing.expect(std.mem.eql(u8, "A", result.abis[0].name));
    try testing.expect(std.mem.eql(u8, "B", result.abis[1].name));
}

test "analyze source captures semantic error" {
    const src =
        "contract A {\n" ++
        "    state balance: u64;\n" ++
        "    state balance: u64;\n" ++
        "}\n";

    var result = try analyzeSource(testing.allocator, src);
    defer result.deinit();
    try testing.expectEqual(@as(usize, 1), result.diagnostics.len);
    try testing.expect(std.mem.indexOf(u8, result.diagnostics[0].message, "duplicate state") != null);
    try testing.expectEqual(@as(usize, 0), result.abis.len);
}

test "analysis result exposes metadata" {
    const src =
        "contract Ledger {\n" ++
        "    pub state balance: u64;\n" ++
        "    table deposits: Map<Address, u64>;\n" ++
        "    pub event Transfer(from: Address, to: Address);\n" ++
        "    pub fn credit(account: Address, amount: u64) view {\n" ++
        "        let mut working = amount;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "}\n";

    var result = try analyzeSource(testing.allocator, src);
    defer result.deinit();

    try testing.expect(result.metadata.owned);
    try testing.expectEqual(@as(usize, 0), result.diagnostics.len);
    try testing.expectEqual(@as(usize, 1), result.metadata.contracts.len);

    const contract = result.metadata.contracts[0];
    try testing.expect(std.mem.eql(u8, "Ledger", contract.name));

    try testing.expectEqual(@as(usize, 1), contract.states.len);
    const state_meta = contract.states[0];
    try testing.expect(std.mem.eql(u8, "balance", state_meta.name));
    try testing.expect(std.mem.eql(u8, "u64", state_meta.type_name));
    try testing.expect(state_meta.is_public);

    try testing.expectEqual(@as(usize, 1), contract.tables.len);
    const table_meta = contract.tables[0];
    try testing.expect(std.mem.eql(u8, "deposits", table_meta.name));
    try testing.expect(std.mem.eql(u8, "Map<Address, u64>", table_meta.type_name));
    try testing.expect(!table_meta.is_public);

    try testing.expectEqual(@as(usize, 1), contract.events.len);
    const event_meta = contract.events[0];
    try testing.expect(std.mem.eql(u8, "Transfer", event_meta.name));
    try testing.expect(event_meta.is_public);
    try testing.expectEqual(@as(usize, 2), event_meta.fields.len);
    try testing.expect(std.mem.eql(u8, "from", event_meta.fields[0].name));
    try testing.expect(std.mem.eql(u8, "Address", event_meta.fields[0].type_name));
    try testing.expect(std.mem.eql(u8, "to", event_meta.fields[1].name));
    try testing.expect(std.mem.eql(u8, "Address", event_meta.fields[1].type_name));

    try testing.expectEqual(@as(usize, 1), contract.functions.len);
    const fn_meta = contract.functions[0];
    try testing.expect(std.mem.eql(u8, "credit", fn_meta.name));
    try testing.expect(std.mem.eql(u8, "void", fn_meta.return_type));
    try testing.expect(fn_meta.is_public);
    try testing.expect(fn_meta.view);
    try testing.expect(!fn_meta.payable);
    try testing.expectEqual(@as(usize, 2), fn_meta.params.len);
    try testing.expect(std.mem.eql(u8, "account", fn_meta.params[0].name));
    try testing.expect(std.mem.eql(u8, "Address", fn_meta.params[0].type_name));
    try testing.expect(!fn_meta.params[0].is_const);
    try testing.expect(!fn_meta.params[0].is_mutable);
    try testing.expect(std.mem.eql(u8, "amount", fn_meta.params[1].name));
    try testing.expect(std.mem.eql(u8, "u64", fn_meta.params[1].type_name));

    try testing.expectEqual(@as(usize, 1), result.abis.len);
    const abi_doc = result.abis[0];
    try testing.expect(std.mem.eql(u8, "Ledger", abi_doc.name));
    try testing.expect(std.mem.indexOf(u8, abi_doc.json, "\"signature\":\"Transfer(Address,Address)\"") != null);
    try testing.expect(std.mem.indexOf(u8, abi_doc.json, "\"topic\":\"0x") != null);
}
