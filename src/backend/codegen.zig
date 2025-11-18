const std = @import("std");
const ir = @import("ir.zig");
const zvm = @import("zvm");
const zvm_opcode = zvm.Opcode;
const gas = @import("gas.zig");

const GasAnalyzer = gas.GasAnalyzer;
const LabelEntry = struct {
    label: u32,
    offset: usize,
};

pub const CodeGenError = error{
    OutOfMemory,
    UnsupportedInstruction,
    UnsupportedReturnValue,
};

pub const Opcode = enum(u8) {
    add = @intFromEnum(zvm_opcode.ADD),
    sub = @intFromEnum(zvm_opcode.SUB),
    mul = @intFromEnum(zvm_opcode.MUL),
    div = @intFromEnum(zvm_opcode.DIV),
    mod_ = @intFromEnum(zvm_opcode.MOD),
    eq = @intFromEnum(zvm_opcode.EQ),
    lt = @intFromEnum(zvm_opcode.LT),
    gt = @intFromEnum(zvm_opcode.GT),

    load_local = @intFromEnum(zvm_opcode.TLOAD),
    store_local = @intFromEnum(zvm_opcode.TSTORE),
    load_state = @intFromEnum(zvm_opcode.SLOAD),
    store_state = @intFromEnum(zvm_opcode.SSTORE),
    jump = @intFromEnum(zvm_opcode.JUMP),
    jump_if = @intFromEnum(zvm_opcode.JUMPI),

    ret = @intFromEnum(zvm_opcode.HALT),
};

pub const table_hash_opcode: u8 = @intFromEnum(zvm_opcode.TABLEHASH);

pub const CodeGen = struct {
    allocator: std.mem.Allocator,
    bytecode: std.ArrayListUnmanaged(u8) = .{},
    last_gas: u64 = 0,
    storage_profile: gas.StorageProfile = .{},
    label_cache: std.ArrayListUnmanaged(LabelEntry) = .{},
    address_base: usize = 0,

    pub fn init(allocator: std.mem.Allocator) CodeGen {
        return .{ .allocator = allocator, .bytecode = .{}, .last_gas = 0, .label_cache = .{}, .address_base = 0 };
    }

    pub fn deinit(self: *CodeGen) void {
        self.bytecode.deinit(self.allocator);
        self.label_cache.deinit(self.allocator);
    }

    pub fn emit(self: *CodeGen, instructions: []const ir.IR) CodeGenError![]const u8 {
        self.bytecode.clearRetainingCapacity();
        self.label_cache.clearRetainingCapacity();
        var label_offsets = std.AutoHashMap(u32, usize).init(self.allocator);
        defer label_offsets.deinit();

        const gas_report = GasAnalyzer.analyze(instructions);
        self.last_gas = gas_report.total;
        self.storage_profile = gas_report.storage;

        var offset: usize = 0;
        for (instructions) |instr| {
            switch (instr) {
                .label => |label| {
                    label_offsets.put(label.name, offset) catch return CodeGenError.OutOfMemory;
                    try self.recordLabel(label.name, offset);
                },
                else => offset += instructionSize(instr) catch return CodeGenError.UnsupportedInstruction,
            }
        }

        for (instructions) |instr| {
            try self.emitInstruction(instr, &label_offsets);
        }
        return self.bytecode.items;
    }

    pub fn setAddressBase(self: *CodeGen, base: usize) void {
        self.address_base = base;
    }

    pub fn getLabelOffset(self: *const CodeGen, label: u32) ?usize {
        for (self.label_cache.items) |entry| {
            if (entry.label == label) return entry.offset;
        }
        return null;
    }

    fn emitInstruction(self: *CodeGen, instr: ir.IR, label_offsets: *std.AutoHashMap(u32, usize)) CodeGenError!void {
        switch (instr) {
            .label => {},
            .add => try self.appendOpcode(.add),
            .sub => try self.appendOpcode(.sub),
            .mul => try self.appendOpcode(.mul),
            .div => try self.appendOpcode(.div),
            .mod_ => try self.appendOpcode(.mod_),
            .eq => try self.appendOpcode(.eq),
            .lt => try self.appendOpcode(.lt),
            .gt => try self.appendOpcode(.gt),
            .ne => try self.emitNe(),
            .lte => try self.emitLte(),
            .gte => try self.emitGte(),
            .load_local => |op| try self.emitLoadLocal(op.slot),
            .store_local => |op| try self.emitStoreLocal(op.slot),
            .load_state => |op| try self.emitLoadState(op.slot),
            .store_state => |op| try self.emitStoreState(op.slot),
            .load_table => |op| try self.emitLoadTable(op),
            .store_table => |op| try self.emitStoreTable(op),
            .jump => |op| try self.emitJump(label_offsets, op.target),
            .jump_if => |op| try self.emitJumpIf(label_offsets, op.target),
            .push => |op| try self.emitPush(op.value),
            .ret => |op| {
                if (op.value != null) return CodeGenError.UnsupportedReturnValue;
                try self.appendOpcode(.ret);
            },
            else => return CodeGenError.UnsupportedInstruction,
        }
    }

    fn emitLoadLocal(self: *CodeGen, slot: u16) CodeGenError!void {
        try self.emitPushU16(slot);
        try self.appendOpcode(.load_local);
    }

    fn emitStoreLocal(self: *CodeGen, slot: u16) CodeGenError!void {
        try self.emitPushU16(slot);
        try self.appendOpcode(.store_local);
    }

    fn emitLoadState(self: *CodeGen, slot: u64) CodeGenError!void {
        try self.emitPushU64(slot);
        try self.appendOpcode(.load_state);
    }

    fn emitStoreState(self: *CodeGen, slot: u64) CodeGenError!void {
        try self.emitPushU64(slot);
        try self.appendOpcode(.store_state);
    }

    fn emitLoadTable(self: *CodeGen, op: ir.TableLoad) CodeGenError!void {
        try self.emitTableHashSequence(op.slot);
        try self.appendOpcode(.load_state);
    }

    fn emitStoreTable(self: *CodeGen, op: ir.TableStore) CodeGenError!void {
        try self.emitTableHashSequence(op.slot);
        try self.appendOpcode(.store_state);
    }

    fn emitJump(self: *CodeGen, label_offsets: *std.AutoHashMap(u32, usize), target: u32) CodeGenError!void {
        const offset = label_offsets.get(target) orelse return CodeGenError.UnsupportedInstruction;
        const adjusted = offset + self.address_base;
        if (adjusted > std.math.maxInt(u32)) return CodeGenError.UnsupportedInstruction;
        try self.emitPushU32(@intCast(adjusted));
        try self.appendOpcode(.jump);
    }

    fn emitJumpIf(self: *CodeGen, label_offsets: *std.AutoHashMap(u32, usize), target: u32) CodeGenError!void {
        const offset = label_offsets.get(target) orelse return CodeGenError.UnsupportedInstruction;
        const adjusted = offset + self.address_base;
        if (adjusted > std.math.maxInt(u32)) return CodeGenError.UnsupportedInstruction;
        try self.emitPushU32(@intCast(adjusted));
        try self.appendOpcode(.jump_if);
    }

    fn appendOpcode(self: *CodeGen, opcode: Opcode) CodeGenError!void {
        self.bytecode.append(self.allocator, @intFromEnum(opcode)) catch return CodeGenError.OutOfMemory;
    }

    fn appendRawOpcode(self: *CodeGen, opcode: u8) CodeGenError!void {
        self.bytecode.append(self.allocator, opcode) catch return CodeGenError.OutOfMemory;
    }

    fn recordLabel(self: *CodeGen, label: u32, offset: usize) !void {
        for (self.label_cache.items) |*entry| {
            if (entry.label == label) {
                entry.offset = offset;
                return;
            }
        }
        try self.label_cache.append(self.allocator, .{ .label = label, .offset = offset });
    }

    fn emitPush(self: *CodeGen, reg: ir.Register) CodeGenError!void {
        if (reg != .constant) return CodeGenError.UnsupportedInstruction;
        const value = reg.constant;
        var buf: [32]u8 = undefined;
        const value_len = encodeConstant(value, &buf);
        const spec = pushSpec(value_len);

        try self.appendRawOpcode(@intFromEnum(spec.opcode));
        const offset = buf.len - spec.size;
        try self.bytecode.appendSlice(self.allocator, buf[offset..]);
    }

    fn emitPushConstant(self: *CodeGen, value: ir.U256) CodeGenError!void {
        const reg = ir.Register{ .constant = value };
        try self.emitPush(reg);
    }

    fn emitInvertBool(self: *CodeGen) CodeGenError!void {
        try self.emitPushConstant(@as(ir.U256, 1));
        try self.appendRawOpcode(@intFromEnum(zvm_opcode.XOR));
    }

    fn emitNe(self: *CodeGen) CodeGenError!void {
        try self.appendOpcode(.eq);
        try self.emitInvertBool();
    }

    fn emitLte(self: *CodeGen) CodeGenError!void {
        try self.appendOpcode(.gt);
        try self.emitInvertBool();
    }

    fn emitGte(self: *CodeGen) CodeGenError!void {
        try self.appendOpcode(.lt);
        try self.emitInvertBool();
    }

    fn emitPushU16(self: *CodeGen, value: u16) CodeGenError!void {
        try self.appendRawOpcode(@intFromEnum(zvm_opcode.PUSH2));
        var buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &buf, value, .big);
        try self.bytecode.appendSlice(self.allocator, &buf);
    }

    fn emitPushU32(self: *CodeGen, value: u32) CodeGenError!void {
        try self.appendRawOpcode(@intFromEnum(zvm_opcode.PUSH4));
        var buf: [4]u8 = undefined;
        std.mem.writeInt(u32, &buf, value, .big);
        try self.bytecode.appendSlice(self.allocator, &buf);
    }

    fn emitPushU64(self: *CodeGen, value: u64) CodeGenError!void {
        try self.appendRawOpcode(@intFromEnum(zvm_opcode.PUSH8));
        var buf: [8]u8 = undefined;
        std.mem.writeInt(u64, &buf, value, .big);
        try self.bytecode.appendSlice(self.allocator, &buf);
    }

    fn emitTableHashSequence(self: *CodeGen, slot: u64) CodeGenError!void {
        // Stack prior to this call is expected to have the table key on top.
        try self.emitPushU64(slot);
        try self.appendRawOpcode(@intFromEnum(zvm_opcode.SWAP1));
        try self.appendRawOpcode(table_hash_opcode);
    }

    pub fn gasUsed(self: *CodeGen) u64 {
        return self.last_gas;
    }

    pub fn storageProfile(self: *const CodeGen) gas.StorageProfile {
        return self.storage_profile;
    }

    fn instructionSize(instr: ir.IR) CodeGenError!usize {
        const push_u16: usize = 1 + 2;
        const push_u32: usize = 1 + 4;
        const push_u64: usize = 1 + 8;

        return switch (instr) {
            .label => 0,
            .add, .sub, .mul, .div, .mod_, .eq, .lt, .gt, .ret => 1,
            .ne, .lte, .gte => boolComparisonSize,
            .load_local, .store_local => push_u16 + 1,
            .load_state, .store_state => push_u64 + 1,
            .load_table, .store_table => table_hash_instruction_size + 1,
            .jump, .jump_if => push_u32 + 1,
            .push => |op| pushEncodedSize(op.value.constant),
            else => CodeGenError.UnsupportedInstruction,
        };
    }
};

const boolComparisonSize: usize = 1 + pushEncodedSize(@as(ir.U256, 1)) + 1;
const table_hash_instruction_size: usize = pushEncodedSize(@as(ir.U256, 0xFFFFFFFFFFFFFFFF)) + 2;

fn encodeConstant(value: ir.U256, buf: *[32]u8) usize {
    @memset(buf, 0);
    var tmp = value;
    var index: usize = buf.len;
    while (tmp != 0 and index > 0) : (tmp >>= 8) {
        index -= 1;
        buf.*[index] = @intCast(tmp & 0xFF);
    }
    if (value == 0) {
        buf.*[buf.len - 1] = 0;
        return 1;
    }
    return buf.len - index;
}

const PushSpec = struct {
    opcode: zvm_opcode,
    size: usize,
};

fn pushSpec(len: usize) PushSpec {
    if (len <= 1) return .{ .opcode = .PUSH1, .size = 1 };
    if (len <= 2) return .{ .opcode = .PUSH2, .size = 2 };
    if (len <= 4) return .{ .opcode = .PUSH4, .size = 4 };
    if (len <= 8) return .{ .opcode = .PUSH8, .size = 8 };
    if (len <= 16) return .{ .opcode = .PUSH16, .size = 16 };
    return .{ .opcode = .PUSH32, .size = 32 };
}

fn pushEncodedSize(value: ir.U256) usize {
    var buf: [32]u8 = undefined;
    const len = encodeConstant(value, &buf);
    const spec = pushSpec(len);
    return 1 + spec.size;
}

const testing = std.testing;
const lowering = @import("lowering.zig");
const parser = @import("../frontend/parser.zig");

// [Truncated for brevity; rest of original file continues]

test "codegen emits bytecode for deposit" {
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

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();

    const bytes = try generator.emit(builder.instructions.items);
    try testing.expectEqual(@as(usize, 34), bytes.len);
    try testing.expect(bytes[0] == @intFromEnum(zvm_opcode.PUSH2));
    try testing.expect(bytes[3] == @intFromEnum(Opcode.load_local));
    try testing.expect(bytes[bytes.len - 1] == @intFromEnum(Opcode.ret));
    try testing.expectEqual(@as(u64, 518), generator.gasUsed());
    const profile = generator.storageProfile();
    try testing.expectEqual(@as(u32, 1), profile.state_loads);
    try testing.expectEqual(@as(u32, 1), profile.state_stores);
    try testing.expectEqual(@as(u32, 0), profile.table_loads);
    try testing.expectEqual(@as(u32, 0), profile.table_stores);
}

test "codegen encodes jumps with label offsets" {
    const instructions = [_]ir.IR{
        ir.IR{ .label = .{ .name = 0 } },
        ir.IR{ .jump = .{ .target = 1 } },
        ir.IR{ .label = .{ .name = 1 } },
        ir.IR{ .ret = .{ .value = null } },
    };

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();

    const bytes = try generator.emit(&instructions);
    try testing.expectEqual(@as(usize, 7), bytes.len);
    try testing.expect(bytes[0] == @intFromEnum(zvm_opcode.PUSH4));
    const offset = std.mem.readInt(u32, bytes[1..5], .big);
    try testing.expectEqual(@as(u32, 6), offset);
    try testing.expect(bytes[5] == @intFromEnum(Opcode.jump));
    try testing.expect(bytes[6] == @intFromEnum(Opcode.ret));
}

test "codegen emits composite comparisons" {
    const instructions = [_]ir.IR{
        ir.IR{ .ne = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 1 }, .right = .{ .temp = 2 } } },
        ir.IR{ .lte = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 3 }, .right = .{ .temp = 4 } } },
        ir.IR{ .gte = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 5 }, .right = .{ .temp = 6 } } },
    };

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();

    const bytes = try generator.emit(&instructions);
    try testing.expectEqual(@as(usize, 12), bytes.len);

    try testing.expect(bytes[0] == @intFromEnum(zvm_opcode.EQ));
    try testing.expect(bytes[1] == @intFromEnum(zvm_opcode.PUSH1));
    try testing.expect(bytes[2] == 0x01);
    try testing.expect(bytes[3] == @intFromEnum(zvm_opcode.XOR));

    try testing.expect(bytes[4] == @intFromEnum(zvm_opcode.GT));
    try testing.expect(bytes[5] == @intFromEnum(zvm_opcode.PUSH1));
    try testing.expect(bytes[6] == 0x01);
    try testing.expect(bytes[7] == @intFromEnum(zvm_opcode.XOR));

    try testing.expect(bytes[8] == @intFromEnum(zvm_opcode.LT));
    try testing.expect(bytes[9] == @intFromEnum(zvm_opcode.PUSH1));
    try testing.expect(bytes[10] == 0x01);
    try testing.expect(bytes[11] == @intFromEnum(zvm_opcode.XOR));
}

test "codegen emits table hash sequence" {
    const instructions = [_]ir.IR{
        ir.IR{ .load_table = .{ .dest = .{ .temp = 0 }, .slot = 7, .key = .{ .temp = 1 } } },
        ir.IR{ .store_table = .{ .slot = 7, .key = .{ .temp = 2 }, .value = .{ .temp = 3 } } },
    };

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();

    const bytes = try generator.emit(&instructions);
    try testing.expectEqual(@as(usize, 24), bytes.len);

    try testing.expect(bytes[0] == @intFromEnum(zvm_opcode.PUSH8));
    const load_slot = std.mem.readInt(u64, bytes[1..9], .big);
    try testing.expectEqual(@as(u64, 7), load_slot);
    try testing.expect(bytes[9] == @intFromEnum(zvm_opcode.SWAP1));
    try testing.expect(bytes[10] == table_hash_opcode);
    try testing.expect(bytes[11] == @intFromEnum(zvm_opcode.SLOAD));

    try testing.expect(bytes[12] == @intFromEnum(zvm_opcode.PUSH8));
    const store_slot = std.mem.readInt(u64, bytes[13..21], .big);
    try testing.expectEqual(@as(u64, 7), store_slot);
    try testing.expect(bytes[21] == @intFromEnum(zvm_opcode.SWAP1));
    try testing.expect(bytes[22] == table_hash_opcode);
    try testing.expect(bytes[23] == @intFromEnum(zvm_opcode.SSTORE));

    try testing.expectEqual(@as(u64, 272), generator.gasUsed());
    const profile = generator.storageProfile();
    try testing.expectEqual(@as(u32, 0), profile.state_loads);
    try testing.expectEqual(@as(u32, 0), profile.state_stores);
    try testing.expectEqual(@as(u32, 1), profile.table_loads);
    try testing.expectEqual(@as(u32, 2), profile.table_stores);
}

test "codegen integrates composite comparisons from source" {
    const source =
        "contract Logic {\n" ++
        "    fn compare(a: u64, b: u64) {\n" ++
        "        let ne_val = a != b;\n" ++
        "        let lte_val = a <= b;\n" ++
        "        let gte_val = a >= b;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytes = try generator.emit(builder.instructions.items);

    var xor_count: usize = 0;
    for (bytes) |byte| {
        if (byte == @intFromEnum(zvm_opcode.XOR)) xor_count += 1;
    }

    try testing.expectEqual(@as(usize, 3), xor_count);
}

test "codegen integrates table hashing from source" {
    const source =
        "contract Tables {\n" ++
        "    table balances: Map<Address, u64>;\n" ++
        "    fn touch(key: u64, value: u64) {\n" ++
        "        state.balances[key] = value;\n" ++
        "        let fetched = state.balances[key];\n" ++
        "        state.balances[key] = fetched;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytes = try generator.emit(builder.instructions.items);

    var hash_count: usize = 0;
    var sload_count: usize = 0;
    var sstore_count: usize = 0;
    for (bytes) |byte| {
        switch (byte) {
            table_hash_opcode => hash_count += 1,
            @intFromEnum(zvm_opcode.SLOAD) => sload_count += 1,
            @intFromEnum(zvm_opcode.SSTORE) => sstore_count += 1,
            else => {},
        }
    }

    try testing.expectEqual(@as(usize, 3), hash_count);
    try testing.expectEqual(@as(usize, 1), sload_count);
    try testing.expectEqual(@as(usize, 2), sstore_count);
}

test "codegen emits branching control flow" {
    const source =
        "contract Flow {\n" ++
        "    fn branch(flag: bool, counter: u64) {\n" ++
        "        if flag {\n" ++
        "            let mut value = counter;\n" ++
        "            value = value + 1;\n" ++
        "        } else {\n" ++
        "            let mut value = counter;\n" ++
        "            value = value - 1;\n" ++
        "        }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytes = try generator.emit(builder.instructions.items);

    var jumpi_count: usize = 0;
    var jump_count: usize = 0;
    for (bytes) |byte| {
        if (byte == @intFromEnum(zvm_opcode.JUMPI)) jumpi_count += 1;
        if (byte == @intFromEnum(zvm_opcode.JUMP)) jump_count += 1;
    }

    try testing.expectEqual(@as(usize, 1), jumpi_count);
    try testing.expect(jump_count >= 2);
}

test "codegen emits while loop backedge" {
    const source =
        "contract Loop {\n" ++
        "    fn spin(mut counter: u64) {\n" ++
        "        while counter {\n" ++
        "            counter = counter - 1;\n" ++
        "        }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var generator = CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytes = try generator.emit(builder.instructions.items);

    var jumpi_count: usize = 0;
    var backjump_count: usize = 0;
    for (bytes, 0..) |byte, idx| {
        if (byte == @intFromEnum(zvm_opcode.JUMPI)) jumpi_count += 1;
        if (byte == @intFromEnum(zvm_opcode.JUMP)) {
            if (idx >= 5 and bytes[idx - 5] == @intFromEnum(zvm_opcode.PUSH4)) {
                backjump_count += 1;
            }
        }
    }

    try testing.expectEqual(@as(usize, 1), jumpi_count);
    try testing.expect(backjump_count >= 1);
}
