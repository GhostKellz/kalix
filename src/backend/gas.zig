const std = @import("std");
const ir = @import("ir.zig");
const parser = @import("../frontend/parser.zig");
const lowering = @import("lowering.zig");
const zvm = @import("zvm");
const zvm_opcode = zvm.Opcode;
const zvm_gas = zvm.gas_costs;

pub const StorageProfile = struct {
    state_loads: u32 = 0,
    state_stores: u32 = 0,
    table_loads: u32 = 0,
    table_stores: u32 = 0,
};

pub const FunctionReport = struct {
    name: []u8,
    label: u32,
    gas: u64,
    storage: StorageProfile,
};

pub const GasReport = struct {
    total: u64 = 0,
    storage: StorageProfile = .{},
    functions: []FunctionReport = &[_]FunctionReport{},

    pub fn deinit(self: *GasReport, allocator: std.mem.Allocator) void {
        for (self.functions) |fn_report| allocator.free(fn_report.name);
        if (self.functions.len != 0) allocator.free(self.functions);
        self.functions = &[_]FunctionReport{};
    }
};

pub const GasAnalyzer = struct {
    pub fn analyze(instructions: []const ir.IR) GasReport {
        var report = GasReport{};
        for (instructions) |instr| {
            report.total += instructionCost(instr);
            switch (instr) {
                .load_state => report.storage.state_loads += 1,
                .store_state => report.storage.state_stores += 1,
                .load_table => report.storage.table_loads += 1,
                .store_table => report.storage.table_stores += 1,
                else => {},
            }
        }
        return report;
    }

    pub fn analyzeFunctions(
        allocator: std.mem.Allocator,
        instructions: []const ir.IR,
        functions: []const lowering.FunctionEntry,
    ) std.mem.Allocator.Error![]FunctionReport {
        var reports = try allocator.alloc(FunctionReport, functions.len);
        var filled: usize = 0;
        errdefer {
            for (reports[0..filled]) |report_item| allocator.free(report_item.name);
            allocator.free(reports);
        }

        for (functions, 0..) |fn_entry, idx| {
            const span = instructions[fn_entry.start_index..fn_entry.end_index];
            const fn_report = analyze(span);
            reports[idx] = .{
                .name = try allocator.dupe(u8, fn_entry.decl.name),
                .label = fn_entry.label,
                .gas = fn_report.total,
                .storage = fn_report.storage,
            };
            filled = idx + 1;
        }

        return reports;
    }
};

fn instructionCost(instr: ir.IR) u64 {
    return switch (instr) {
        .label => 0,
        .add => opcodeCost(zvm_opcode.ADD),
        .sub => opcodeCost(zvm_opcode.SUB),
        .mul => opcodeCost(zvm_opcode.MUL),
        .div => opcodeCost(zvm_opcode.DIV),
        .mod_ => opcodeCost(zvm_opcode.MOD),
        .eq => opcodeCost(zvm_opcode.EQ),
        .lt => opcodeCost(zvm_opcode.LT),
        .gt => opcodeCost(zvm_opcode.GT),
        .ne => boolComparisonCost(zvm_opcode.EQ),
        .lte => boolComparisonCost(zvm_opcode.GT),
        .gte => boolComparisonCost(zvm_opcode.LT),
        .load_local => push_cost + opcodeCost(zvm_opcode.TLOAD),
        .store_local => push_cost + opcodeCost(zvm_opcode.TSTORE),
        .load_state => push_cost + opcodeCost(zvm_opcode.SLOAD),
        .store_state => push_cost + opcodeCost(zvm_opcode.SSTORE),
        .load_table => tableSequenceCost(opcodeCost(zvm_opcode.SLOAD)),
        .store_table => tableSequenceCost(opcodeCost(zvm_opcode.SSTORE)),
        .push => push_cost,
        .jump => jumpCost(false),
        .jump_if => jumpCost(true),
        .ret => opcodeCost(zvm_opcode.HALT),
        else => unknown_cost,
    };
}

inline fn opcodeCost(op: zvm_opcode) u64 {
    return zvm_gas.getOpcodeCost(op);
}

const push_cost = opcodeCost(zvm_opcode.PUSH1);
const swap_cost = opcodeCost(zvm_opcode.SWAP1);
const table_hash_cost = opcodeCost(zvm_opcode.TABLEHASH);
const invert_bool_cost = push_cost + opcodeCost(zvm_opcode.XOR);
// Mirrors emitTableHashSequence: PUSH slot, SWAP1 to bring key on top, TABLEHASH.
const table_sequence_base_cost = push_cost + swap_cost + table_hash_cost;
const jump_base_cost = opcodeCost(zvm_opcode.JUMP);
const jump_if_base_cost = opcodeCost(zvm_opcode.JUMPI);
const unknown_cost: u64 = 25;

comptime {
    std.debug.assert(push_cost != 0);
    std.debug.assert(opcodeCost(zvm_opcode.SSTORE) != 0);
}

fn boolComparisonCost(base: zvm_opcode) u64 {
    return opcodeCost(base) + invert_bool_cost;
}

fn tableSequenceCost(terminal_cost: u64) u64 {
    return table_sequence_base_cost + terminal_cost;
}

fn jumpCost(is_conditional: bool) u64 {
    return push_cost + (if (is_conditional) jump_if_base_cost else jump_base_cost);
}

const testing = std.testing;

test "gas analyzer aggregates instruction costs" {
    const source =
        "contract Treasury {\n" ++
        "    state balance: u64;\n" ++
        "    fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    const report = GasAnalyzer.analyze(builder.instructions.items);
    try testing.expectEqual(@as(u64, 518), report.total);
    try testing.expectEqual(@as(u32, 1), report.storage.state_loads);
    try testing.expectEqual(@as(u32, 1), report.storage.state_stores);
    try testing.expectEqual(@as(u32, 0), report.storage.table_loads);
    try testing.expectEqual(@as(u32, 0), report.storage.table_stores);
}

test "gas analyzer reports per-function metrics" {
    const source =
        "contract Stats {\n" ++
        "    state counter: u64;\n" ++
        "    fn set(value: u64) {\n" ++
        "        state.counter = value;\n" ++
        "    }\n" ++
        "    fn get() {\n" ++
        "        let current = state.counter;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    const reports = try GasAnalyzer.analyzeFunctions(testing.allocator, builder.instructions.items, lowering.getFunctions(&builder));
    defer {
        for (reports) |report| testing.allocator.free(report.name);
        testing.allocator.free(reports);
    }

    try testing.expectEqual(@as(usize, 2), reports.len);
    try testing.expect(std.mem.eql(u8, reports[0].name, "set"));
    try testing.expect(std.mem.eql(u8, reports[1].name, "get"));
    try testing.expect(reports[0].storage.state_stores >= 1);
    try testing.expect(reports[1].storage.state_loads >= 1);
}
