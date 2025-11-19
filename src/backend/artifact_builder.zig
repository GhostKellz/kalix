const std = @import("std");
const ast = @import("../frontend/ast.zig");
const parser = @import("../frontend/parser.zig");
const lowering = @import("lowering.zig");
const codegen = @import("codegen.zig");
const gas = @import("gas.zig");
const abi_builder = @import("abi_builder.zig");
const zvm = @import("zvm");
const zvm_opcode = zvm.Opcode;

pub const BuildError = lowering.LoweringError || codegen.CodeGenError || abi_builder.AbiBuilderError || error{ OutOfMemory, MissingFunctionLabel, DuplicateSelector };

pub const ContractArtifact = struct {
    name: []u8,
    container: zvm.BytecodeContainer,
    gas: gas.GasReport,

    pub fn deinit(self: *ContractArtifact, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        self.gas.deinit(allocator);
        self.container.deinit(allocator);
    }
};

const FunctionDispatch = struct {
    selector: [4]u8,
    label: u32,
    param_count: usize,
};

const DispatcherLayout = struct {
    header_len: usize,
    compare_len: usize,
    revert_offset: usize,
    revert_len: usize,
    stub_offsets: []usize,
    total_len: usize,
};

pub fn buildModule(allocator: std.mem.Allocator, module: *const ast.Module) BuildError![]ContractArtifact {
    var artifacts = std.ArrayListUnmanaged(ContractArtifact){};
    errdefer {
        for (artifacts.items) |*artifact| {
            artifact.deinit(allocator);
        }
        artifacts.deinit(allocator);
    }

    for (module.contracts) |contract| {
        var artifact = try buildContract(allocator, contract);
        errdefer artifact.deinit(allocator);
        try artifacts.append(allocator, artifact);
    }

    return artifacts.toOwnedSlice(allocator);
}

pub fn buildContract(allocator: std.mem.Allocator, contract: ast.Contract) BuildError!ContractArtifact {
    var builder = lowering.IRBuilder.init(allocator);
    defer builder.deinit();

    var module_view = ast.Module{ .contracts = &[_]ast.Contract{contract} };
    try builder.lowerModule(&module_view);

    const instructions = builder.instructions.items;

    var dispatch_entries = std.ArrayListUnmanaged(FunctionDispatch){};
    defer dispatch_entries.deinit(allocator);
    try collectDispatchEntries(allocator, lowering.getFunctions(&builder), &dispatch_entries);

    const layout = try computeDispatcherLayout(allocator, dispatch_entries.items);
    defer allocator.free(layout.stub_offsets);

    var generator = codegen.CodeGen.init(allocator);
    defer generator.deinit();
    generator.setAddressBase(layout.total_len);
    const body_code = try generator.emit(instructions, lowering.getFunctions(&builder));
    var gas_report = generator.takeGasReport();
    errdefer gas_report.deinit(allocator);

    var function_offsets = try allocator.alloc(usize, dispatch_entries.items.len);
    defer allocator.free(function_offsets);
    for (dispatch_entries.items, 0..) |entry, idx| {
        const offset = generator.getLabelOffset(entry.label) orelse return BuildError.MissingFunctionLabel;
        function_offsets[idx] = offset;
    }

    const dispatcher = try emitDispatcher(
        allocator,
        dispatch_entries.items,
        layout,
        function_offsets,
    );
    defer allocator.free(dispatcher);

    var final_code = std.ArrayListUnmanaged(u8){};
    defer final_code.deinit(allocator);
    try final_code.appendSlice(allocator, dispatcher);
    try final_code.appendSlice(allocator, body_code);
    const bytecode = try final_code.toOwnedSlice(allocator);
    defer allocator.free(bytecode);

    var abi = abi_builder.AbiBuilder.init(allocator);
    const abi_json = try abi.buildContract(contract);
    defer allocator.free(abi_json);

    var container = try zvm.BytecodeContainer.create(allocator, bytecode, abi_json, .zvm_native);
    errdefer container.deinit(allocator);

    const name_copy = try allocator.dupe(u8, contract.name);

    return ContractArtifact{
        .name = name_copy,
        .container = container,
        .gas = gas_report,
    };
}

pub fn serializeArtifact(allocator: std.mem.Allocator, artifact: *const ContractArtifact) error{OutOfMemory}![]u8 {
    return try artifact.container.serialize(allocator);
}

const testing = std.testing;

fn buildArtifactsForSource(source: []const u8) ![]ContractArtifact {
    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    const module = tree.getModule();
    return try buildModule(testing.allocator, module);
}

const dispatch_header_len: usize = 21;
const dispatch_compare_len: usize = 16;
const dispatch_revert_len: usize = 5;
const dispatch_param_step_len: usize = 10;
const dispatch_stub_footer_len: usize = 6;

fn collectDispatchEntries(
    allocator: std.mem.Allocator,
    functions: []const lowering.FunctionEntry,
    out: *std.ArrayListUnmanaged(FunctionDispatch),
) BuildError!void {
    var seen = std.AutoHashMap(u32, u8).init(allocator);
    defer seen.deinit();

    for (functions) |entry| {
        if (!entry.decl.qualifiers.is_public) continue;
        const signature = try abi_builder.makeSignature(allocator, entry.decl);
        defer allocator.free(signature);

        const selector = zvm.KalixLoader.computeSelector(signature);
        const selector_value = std.mem.readInt(u32, selector[0..], .big);
        if (seen.contains(selector_value)) return BuildError.DuplicateSelector;
        seen.put(selector_value, 0) catch return BuildError.OutOfMemory;
        try out.append(allocator, .{
            .selector = selector,
            .label = entry.label,
            .param_count = entry.decl.params.len,
        });
    }
}

fn computeDispatcherLayout(allocator: std.mem.Allocator, entries: []const FunctionDispatch) !DispatcherLayout {
    const header_len = dispatch_header_len;
    const compare_len = dispatch_compare_len * entries.len;
    const revert_offset = header_len + compare_len;
    const revert_len = dispatch_revert_len;

    var stub_offsets = try allocator.alloc(usize, entries.len);
    var cursor = revert_offset + revert_len;
    for (entries, 0..) |entry, idx| {
        stub_offsets[idx] = cursor;
        cursor += stubLength(entry);
    }

    return DispatcherLayout{
        .header_len = header_len,
        .compare_len = compare_len,
        .revert_offset = revert_offset,
        .revert_len = revert_len,
        .stub_offsets = stub_offsets,
        .total_len = cursor,
    };
}

fn stubLength(entry: FunctionDispatch) usize {
    return (entry.param_count * dispatch_param_step_len) + dispatch_stub_footer_len;
}

fn emitDispatcher(
    allocator: std.mem.Allocator,
    entries: []const FunctionDispatch,
    layout: DispatcherLayout,
    function_offsets: []const usize,
) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    errdefer buffer.deinit(allocator);

    try appendOpcode(&buffer, allocator, .CALLDATASIZE);
    try appendPush1(&buffer, allocator, 0x04);
    try appendOpcode(&buffer, allocator, .LT);
    try appendPush4(&buffer, allocator, @intCast(layout.revert_offset));
    try appendOpcode(&buffer, allocator, .SWAP1);
    try appendOpcode(&buffer, allocator, .JUMPI);

    try appendPush1(&buffer, allocator, 0x00);
    try appendOpcode(&buffer, allocator, .CALLDATALOAD);
    try appendPush1(&buffer, allocator, 0xE0);
    try appendOpcode(&buffer, allocator, .SHR);
    try appendPush1(&buffer, allocator, 0x00);
    try appendOpcode(&buffer, allocator, .SWAP1);
    try appendOpcode(&buffer, allocator, .MSTORE);

    for (entries, 0..) |entry, idx| {
        try appendPush1(&buffer, allocator, 0x00);
        try appendOpcode(&buffer, allocator, .MLOAD);

        const selector_value = std.mem.readInt(u32, entry.selector[0..], .big);
        try appendPush4(&buffer, allocator, selector_value);
        try appendOpcode(&buffer, allocator, .EQ);

        try appendPush4(&buffer, allocator, @intCast(layout.stub_offsets[idx]));
        try appendOpcode(&buffer, allocator, .SWAP1);
        try appendOpcode(&buffer, allocator, .JUMPI);
    }

    try appendPush1(&buffer, allocator, 0x00);
    try appendPush1(&buffer, allocator, 0x00);
    try appendOpcode(&buffer, allocator, .REVERT);

    for (entries, 0..) |entry, idx| {
        for (0..entry.param_count) |param_idx| {
            const param_index: u32 = @intCast(param_idx);
            const calldata_offset: u32 = 4 + param_index * 32;
            try appendPush4(&buffer, allocator, calldata_offset);
            try appendOpcode(&buffer, allocator, .CALLDATALOAD);
            const slot: u16 = @intCast(param_idx);
            try appendPush2(&buffer, allocator, slot);
            try appendOpcode(&buffer, allocator, .TSTORE);
        }

        const fn_address = layout.total_len + function_offsets[idx];
        try appendPush4(&buffer, allocator, @intCast(fn_address));
        try appendOpcode(&buffer, allocator, .JUMP);
    }

    return buffer.toOwnedSlice(allocator);
}

fn appendOpcode(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, opcode: zvm_opcode) !void {
    try buffer.append(allocator, @intFromEnum(opcode));
}

fn appendPush1(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, value: u8) !void {
    try buffer.append(allocator, @intFromEnum(zvm_opcode.PUSH1));
    try buffer.append(allocator, value);
}

fn appendPush2(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, value: u16) !void {
    try buffer.append(allocator, @intFromEnum(zvm_opcode.PUSH2));
    var bytes: [2]u8 = undefined;
    std.mem.writeInt(u16, &bytes, value, .big);
    try buffer.appendSlice(allocator, bytes[0..]);
}

fn appendPush4(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, value: u32) !void {
    try buffer.append(allocator, @intFromEnum(zvm_opcode.PUSH4));
    var bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &bytes, value, .big);
    try buffer.appendSlice(allocator, bytes[0..]);
}

test "artifact builder creates container with abi" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const artifacts = try buildArtifactsForSource(source);
    defer {
        for (artifacts) |*artifact| artifact.deinit(testing.allocator);
        testing.allocator.free(artifacts);
    }

    try testing.expectEqual(@as(usize, 1), artifacts.len);
    const artifact = artifacts[0];
    try testing.expect(std.mem.eql(u8, "Treasury", artifact.name));
    try testing.expect(artifact.container.code.len > 0);
    try testing.expect(artifact.container.abi.len > 0);
    try testing.expectEqual(zvm.BytecodeContainer.Target.zvm_native, artifact.container.target);
    try testing.expect(artifact.gas.total > 0);
    try testing.expectEqual(@as(u32, 1), artifact.gas.storage.state_stores);
    try testing.expectEqual(@as(u32, 1), artifact.gas.storage.state_loads);
    try testing.expectEqual(@as(usize, 1), artifact.gas.functions.len);
    try testing.expect(std.mem.eql(u8, artifact.gas.functions[0].name, "deposit"));
}

test "artifact container loads through kalix loader" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const artifacts = try buildArtifactsForSource(source);
    defer {
        for (artifacts) |*artifact| artifact.deinit(testing.allocator);
        testing.allocator.free(artifacts);
    }

    var artifact = artifacts[0];
    const serialized = try serializeArtifact(testing.allocator, &artifact);
    defer testing.allocator.free(serialized);

    var loader = zvm.runtime.KalixLoader.init(testing.allocator);
    var loaded_container = try loader.loadContract(serialized);
    defer loaded_container.deinit(testing.allocator);

    try testing.expectEqual(artifact.container.code_size, loaded_container.code_size);
    try testing.expect(loaded_container.abi.len > 0);
}

test "artifact dispatcher routes public calls" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const artifacts = try buildArtifactsForSource(source);
    defer {
        for (artifacts) |*artifact| artifact.deinit(testing.allocator);
        testing.allocator.free(artifacts);
    }

    var artifact = artifacts[0];

    var loader = zvm.runtime.KalixLoader.init(testing.allocator);

    var state = zvm.JournaledState.init(testing.allocator);
    defer state.deinit();
    var tstorage = zvm.TransientStorageImpl.init(testing.allocator);
    defer tstorage.deinit();

    var storage = state.asStorage();
    const addr = zvm.Address.zero();
    storage.store(addr, zvm.U256.zero(), zvm.U256.fromU64(10));

    const selector = zvm.KalixLoader.computeSelector("deposit(u64)");
    var calldata = std.ArrayListUnmanaged(u8){};
    defer calldata.deinit(testing.allocator);

    var param_bytes: [32]u8 = [_]u8{0} ** 32;
    std.mem.writeInt(u64, param_bytes[24..32], 5, .big);
    try calldata.appendSlice(testing.allocator, param_bytes[0..]);

    const result = try loader.executeFunction(
        &artifact.container,
        selector,
        calldata.items,
        200_000,
        storage,
        tstorage.asTransientStorage(),
        null,
    );

    try testing.expect(result.success);
    const updated = storage.load(addr, zvm.U256.zero());
    try testing.expectEqual(@as(u64, 15), updated.toU64());
}

test "artifact dispatcher rejects unknown selectors" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        state.balance = state.balance + amount;\n" ++
        "    }\n" ++
        "}\n";

    const artifacts = try buildArtifactsForSource(source);
    defer {
        for (artifacts) |*artifact| artifact.deinit(testing.allocator);
        testing.allocator.free(artifacts);
    }

    var artifact = artifacts[0];

    var loader = zvm.runtime.KalixLoader.init(testing.allocator);
    var state = zvm.JournaledState.init(testing.allocator);
    defer state.deinit();
    var tstorage = zvm.TransientStorageImpl.init(testing.allocator);
    defer tstorage.deinit();

    const selector = zvm.KalixLoader.computeSelector("missing()");
    var calldata = std.ArrayListUnmanaged(u8){};
    defer calldata.deinit(testing.allocator);

    const result = try loader.executeFunction(
        &artifact.container,
        selector,
        calldata.items,
        200_000,
        state.asStorage(),
        tstorage.asTransientStorage(),
        null,
    );

    try testing.expect(!result.success);
}

test "artifact dispatcher handles multiple functions" {
    const source =
        "contract Ops {\n" ++
        "    pub state total: u64;\n" ++
        "    pub fn set(value: u64) {\n" ++
        "        state.total = value;\n" ++
        "    }\n" ++
        "    pub fn add(delta: u64) {\n" ++
        "        state.total = state.total + delta;\n" ++
        "    }\n" ++
        "}\n";

    const artifacts = try buildArtifactsForSource(source);
    defer {
        for (artifacts) |*artifact| artifact.deinit(testing.allocator);
        testing.allocator.free(artifacts);
    }

    var artifact = artifacts[0];

    var loader = zvm.runtime.KalixLoader.init(testing.allocator);
    var state = zvm.JournaledState.init(testing.allocator);
    defer state.deinit();
    var tstorage = zvm.TransientStorageImpl.init(testing.allocator);
    defer tstorage.deinit();

    var storage = state.asStorage();
    const addr = zvm.Address.zero();

    const set_selector = zvm.KalixLoader.computeSelector("set(u64)");
    const add_selector = zvm.KalixLoader.computeSelector("add(u64)");

    var calldata = std.ArrayListUnmanaged(u8){};
    defer calldata.deinit(testing.allocator);

    var buf: [32]u8 = [_]u8{0} ** 32;

    std.mem.writeInt(u64, buf[24..32], 5, .big);
    try calldata.appendSlice(testing.allocator, buf[0..]);
    const set_result = try loader.executeFunction(
        &artifact.container,
        set_selector,
        calldata.items,
        200_000,
        storage,
        tstorage.asTransientStorage(),
        null,
    );
    try testing.expect(set_result.success);
    calldata.clearRetainingCapacity();

    @memset(&buf, 0);
    std.mem.writeInt(u64, buf[24..32], 3, .big);
    try calldata.appendSlice(testing.allocator, buf[0..]);
    const add_result = try loader.executeFunction(
        &artifact.container,
        add_selector,
        calldata.items,
        200_000,
        storage,
        tstorage.asTransientStorage(),
        null,
    );
    try testing.expect(add_result.success);

    const updated = storage.load(addr, zvm.U256.zero());
    try testing.expectEqual(@as(u64, 8), updated.toU64());
    try testing.expectEqual(@as(usize, 2), artifact.gas.functions.len);
}

test "artifact builder rejects duplicate selectors" {
    const source =
        "contract Collide {\n" ++
        "    pub fn ping() { }\n" ++
        "    pub fn ping() { }\n" ++
        "}\n";

    const result = buildArtifactsForSource(source);
    try testing.expectError(BuildError.DuplicateSelector, result);
}
