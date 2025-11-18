const std = @import("std");
const ir = @import("ir.zig");
const codegen = @import("codegen.zig");
const zvm = @import("zvm");
const zvm_opcode = zvm.Opcode;

pub const HarnessError = error{
    OutOfMemory,
    BytecodeTruncated,
    UnexpectedOpcode,
    StackUnderflow,
    MissingLocal,
    DivisionByZero,
    ExpectedReturn,
    InvalidJumpTarget,
    InvalidImmediate,
};

const BinaryOp = enum {
    add,
    sub,
    mul,
    div,
    mod,
    eq,
    lt,
    gt,
    bit_and,
    bit_xor,
};

pub const Harness = struct {
    allocator: std.mem.Allocator,
    stack: std.ArrayListUnmanaged(ir.U256) = .{},
    locals: std.AutoHashMap(u16, ir.U256),
    storage: std.AutoHashMap(ir.U256, ir.U256),

    pub fn init(allocator: std.mem.Allocator) Harness {
        return .{
            .allocator = allocator,
            .stack = .{},
            .locals = std.AutoHashMap(u16, ir.U256).init(allocator),
            .storage = std.AutoHashMap(ir.U256, ir.U256).init(allocator),
        };
    }

    pub fn deinit(self: *Harness) void {
        self.stack.deinit(self.allocator);
        self.locals.deinit();
        self.storage.deinit();
    }

    pub fn setLocal(self: *Harness, slot: u16, value: ir.U256) HarnessError!void {
        self.locals.put(slot, value) catch return HarnessError.OutOfMemory;
    }

    pub fn getLocal(self: *Harness, slot: u16) ?ir.U256 {
        if (self.locals.get(slot)) |ptr| return ptr.*;
        return null;
    }

    pub fn setStorage(self: *Harness, slot: ir.U256, value: ir.U256) HarnessError!void {
        self.storage.put(slot, value) catch return HarnessError.OutOfMemory;
    }

    pub fn getStorage(self: *Harness, slot: ir.U256) ir.U256 {
        if (self.storage.get(slot)) |ptr| return ptr.*;
        return @as(ir.U256, 0);
    }

    pub fn run(self: *Harness, bytecode: []const u8) HarnessError!void {
        self.stack.clearRetainingCapacity();
        var pc: usize = 0;

        while (pc < bytecode.len) {
            const opcode_byte = bytecode[pc];
            pc += 1;

            const opcode = std.meta.intToEnum(zvm_opcode, opcode_byte) catch return HarnessError.UnexpectedOpcode;

            switch (opcode) {
                .HALT => return,

                .PUSH1 => try self.push(try readPushValue(bytecode, &pc, 1)),
                .PUSH2 => try self.push(try readPushValue(bytecode, &pc, 2)),
                .PUSH4 => try self.push(try readPushValue(bytecode, &pc, 4)),
                .PUSH8 => try self.push(try readPushValue(bytecode, &pc, 8)),
                .PUSH16 => try self.push(try readPushValue(bytecode, &pc, 16)),
                .PUSH32 => try self.push(try readPushValue(bytecode, &pc, 32)),

                .ADD => try self.binaryOp(.add),
                .SUB => try self.binaryOp(.sub),
                .MUL => try self.binaryOp(.mul),
                .DIV => try self.binaryOp(.div),
                .MOD => try self.binaryOp(.mod),
                .EQ => try self.binaryOp(.eq),
                .LT => try self.binaryOp(.lt),
                .GT => try self.binaryOp(.gt),
                .AND => try self.binaryOp(.bit_and),
                .XOR => try self.binaryOp(.bit_xor),

                .TLOAD => {
                    const slot_value = try self.pop();
                    const slot = try u256ToU16(slot_value);
                    const local_ptr = self.locals.get(slot) orelse return HarnessError.MissingLocal;
                    try self.push(local_ptr.*);
                },
                .TSTORE => {
                    const slot_value = try self.pop();
                    const slot = try u256ToU16(slot_value);
                    const value = try self.pop();
                    self.locals.put(slot, value) catch return HarnessError.OutOfMemory;
                },

                .SLOAD => {
                    const slot_value = try self.pop();
                    const value = self.getStorage(slot_value);
                    try self.push(value);
                },
                .SSTORE => {
                    const slot_value = try self.pop();
                    const value = try self.pop();
                    self.storage.put(slot_value, value) catch return HarnessError.OutOfMemory;
                },

                .JUMP => {
                    const dest_value = try self.pop();
                    const dest = try u256ToUsize(dest_value);
                    if (dest >= bytecode.len) return HarnessError.InvalidJumpTarget;
                    pc = dest;
                    continue;
                },

                .JUMPI => {
                    const dest_value = try self.pop();
                    const condition = try self.pop();
                    if (condition != 0) {
                        const dest = try u256ToUsize(dest_value);
                        if (dest >= bytecode.len) return HarnessError.InvalidJumpTarget;
                        pc = dest;
                        continue;
                    }
                },

                .TABLEHASH => {
                    const key = try self.pop();
                    const slot = try self.pop();
                    const hashed = computeTableHash(slot, key);
                    try self.push(hashed);
                },

                else => return HarnessError.UnexpectedOpcode,
            }
        }

        return HarnessError.ExpectedReturn;
    }

    fn push(self: *Harness, value: ir.U256) HarnessError!void {
        self.stack.append(self.allocator, value) catch return HarnessError.OutOfMemory;
    }

    fn pop(self: *Harness) HarnessError!ir.U256 {
        if (self.stack.items.len == 0) return HarnessError.StackUnderflow;
        const value = self.stack.items[self.stack.items.len - 1];
        self.stack.items.len -= 1;
        return value;
    }

    fn binaryOp(self: *Harness, op: BinaryOp) HarnessError!void {
        const right = try self.pop();
        const left = try self.pop();
        const result = switch (op) {
            .add => left +% right,
            .sub => left -% right,
            .mul => left *% right,
            .div => blk: {
                if (right == 0) return HarnessError.DivisionByZero;
                break :blk left / right;
            },
            .mod => blk: {
                if (right == 0) return HarnessError.DivisionByZero;
                break :blk left % right;
            },
            .eq => if (left == right) 1 else 0,
            .lt => if (left < right) 1 else 0,
            .gt => if (left > right) 1 else 0,
            .bit_and => left & right,
            .bit_xor => left ^ right,
        };
        try self.push(result);
    }
};

fn readImmediate(comptime T: type, bytecode: []const u8, index: *usize) HarnessError!T {
    const size = @sizeOf(T);
    if (index.* + size > bytecode.len) return HarnessError.BytecodeTruncated;
    const value = std.mem.readInt(T, bytecode[index.* .. index.* + size], .little);
    index.* += size;
    return value;
}

fn readPushValue(bytecode: []const u8, pc: *usize, size: usize) HarnessError!ir.U256 {
    if (pc.* + size > bytecode.len) return HarnessError.BytecodeTruncated;
    var result: ir.U256 = 0;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        result <<= 8;
        result |= @as(ir.U256, bytecode[pc.* + i]);
    }
    pc.* += size;
    return result;
}

fn u256ToU16(value: ir.U256) HarnessError!u16 {
    const max = @as(ir.U256, std.math.maxInt(u16));
    if (value > max) return HarnessError.InvalidImmediate;
    return @intCast(value);
}

fn u256ToUsize(value: ir.U256) HarnessError!usize {
    const max = @as(ir.U256, std.math.maxInt(usize));
    if (value > max) return HarnessError.InvalidImmediate;
    return @intCast(value);
}

fn computeTableHash(slot: ir.U256, key: ir.U256) ir.U256 {
    var slot_bytes: [32]u8 = undefined;
    var key_bytes: [32]u8 = undefined;
    encodeU256(slot, &slot_bytes);
    encodeU256(key, &key_bytes);

    var hasher = std.crypto.hash.sha3.Keccak256.init(.{});
    hasher.update(&slot_bytes);
    hasher.update(&key_bytes);

    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    return decodeU256(hash);
}

fn encodeU256(value: ir.U256, buffer: *[32]u8) void {
    var tmp = value;
    var index: usize = buffer.len;
    @memset(buffer, 0);
    while (tmp != 0 and index > 0) : (tmp >>= 8) {
        index -= 1;
        buffer.*[index] = @as(u8, @intCast(tmp & 0xFF));
    }
}

fn decodeU256(bytes: [32]u8) ir.U256 {
    var value: ir.U256 = 0;
    for (bytes) |byte| {
        value <<= 8;
        value |= @as(ir.U256, byte);
    }
    return value;
}

const testing = std.testing;
const parser = @import("../frontend/parser.zig");
const lowering = @import("lowering.zig");

const TestError = error{MissingLocal};

test "harness executes deposit bytecode and updates storage" {
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

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytecode = try generator.emit(builder.instructions.items);

    var harness = Harness.init(testing.allocator);
    defer harness.deinit();
    try harness.setLocal(0, @as(ir.U256, 5));
    try harness.setStorage(@as(ir.U256, 0), @as(ir.U256, 10));

    try harness.run(bytecode);

    try testing.expectEqual(@as(ir.U256, 15), harness.getStorage(@as(ir.U256, 0)));

    const stored_total = harness.getLocal(1) orelse return TestError.MissingLocal;
    try testing.expectEqual(@as(ir.U256, 5), stored_total);
}

test "harness executes conditional branch" {
    const instructions = [_]ir.IR{
        ir.IR{ .label = .{ .name = 0 } },
        ir.IR{ .load_local = .{ .dest = .{ .temp = 0 }, .slot = 0 } },
        ir.IR{ .jump_if = .{ .target = 2, .condition = .{ .temp = 0 } } },
        ir.IR{ .load_local = .{ .dest = .{ .temp = 1 }, .slot = 2 } },
        ir.IR{ .store_local = .{ .slot = 3, .value = .{ .temp = 1 } } },
        ir.IR{ .jump = .{ .target = 3 } },
        ir.IR{ .label = .{ .name = 2 } },
        ir.IR{ .load_local = .{ .dest = .{ .temp = 1 }, .slot = 1 } },
        ir.IR{ .store_local = .{ .slot = 3, .value = .{ .temp = 1 } } },
        ir.IR{ .label = .{ .name = 3 } },
        ir.IR{ .load_local = .{ .dest = .{ .temp = 2 }, .slot = 3 } },
        ir.IR{ .store_state = .{ .slot = 0, .value = .{ .temp = 2 } } },
        ir.IR{ .ret = .{ .value = null } },
    };

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytecode = try generator.emit(&instructions);

    var harness_true = Harness.init(testing.allocator);
    defer harness_true.deinit();
    try harness_true.setLocal(0, @as(ir.U256, 1));
    try harness_true.setLocal(1, @as(ir.U256, 42));
    try harness_true.setLocal(2, @as(ir.U256, 7));
    try harness_true.setLocal(3, @as(ir.U256, 0));
    try harness_true.run(bytecode);
    try testing.expectEqual(@as(ir.U256, 42), harness_true.getStorage(@as(ir.U256, 0)));

    var harness_false = Harness.init(testing.allocator);
    defer harness_false.deinit();
    try harness_false.setLocal(0, @as(ir.U256, 0));
    try harness_false.setLocal(1, @as(ir.U256, 99));
    try harness_false.setLocal(2, @as(ir.U256, 11));
    try harness_false.setLocal(3, @as(ir.U256, 0));
    try harness_false.run(bytecode);
    try testing.expectEqual(@as(ir.U256, 11), harness_false.getStorage(@as(ir.U256, 0)));
}

test "harness executes table store and load" {
    const store_program = [_]ir.IR{
        ir.IR{ .load_local = .{ .dest = .{ .temp = 0 }, .slot = 0 } },
        ir.IR{ .load_local = .{ .dest = .{ .temp = 1 }, .slot = 1 } },
        ir.IR{ .store_table = .{ .slot = 0, .key = .{ .temp = 1 }, .value = .{ .temp = 0 } } },
        ir.IR{ .ret = .{ .value = null } },
    };

    const load_program = [_]ir.IR{
        ir.IR{ .load_local = .{ .dest = .{ .temp = 0 }, .slot = 1 } },
        ir.IR{ .load_table = .{ .slot = 0, .dest = .{ .temp = 1 }, .key = .{ .temp = 0 } } },
        ir.IR{ .store_local = .{ .slot = 2, .value = .{ .temp = 1 } } },
        ir.IR{ .ret = .{ .value = null } },
    };

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();

    var harness = Harness.init(testing.allocator);
    defer harness.deinit();

    const store_bytes = try generator.emit(&store_program);
    try harness.setLocal(0, @as(ir.U256, 42));
    try harness.setLocal(1, @as(ir.U256, 7));
    try harness.run(store_bytes);

    const expected_slot = computeTableHash(@as(ir.U256, 0), @as(ir.U256, 7));
    try testing.expectEqual(@as(ir.U256, 42), harness.getStorage(expected_slot));

    const load_bytes = try generator.emit(&load_program);
    try harness.setLocal(1, @as(ir.U256, 7));
    try harness.run(load_bytes);

    const stored = harness.getLocal(2) orelse return TestError.MissingLocal;
    try testing.expectEqual(@as(ir.U256, 42), stored);
}

test "harness executes comparison ops" {
    const program = [_]ir.IR{
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .eq = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 1 }, .right = .{ .temp = 2 } } },
        ir.IR{ .store_local = .{ .slot = 0, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 7) } } },
        ir.IR{ .eq = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 3 }, .right = .{ .temp = 4 } } },
        ir.IR{ .store_local = .{ .slot = 1, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 4) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 7) } } },
        ir.IR{ .lt = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 5 }, .right = .{ .temp = 6 } } },
        ir.IR{ .store_local = .{ .slot = 2, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 9) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 7) } } },
        ir.IR{ .gt = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 7 }, .right = .{ .temp = 8 } } },
        ir.IR{ .store_local = .{ .slot = 3, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 7) } } },
        ir.IR{ .ne = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 9 }, .right = .{ .temp = 10 } } },
        ir.IR{ .store_local = .{ .slot = 4, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 5) } } },
        ir.IR{ .lte = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 11 }, .right = .{ .temp = 12 } } },
        ir.IR{ .store_local = .{ .slot = 5, .value = .{ .temp = 0 } } },

        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 4) } } },
        ir.IR{ .push = .{ .value = ir.Register{ .constant = @as(ir.U256, 6) } } },
        ir.IR{ .gte = .{ .dest = .{ .temp = 0 }, .left = .{ .temp = 13 }, .right = .{ .temp = 14 } } },
        ir.IR{ .store_local = .{ .slot = 6, .value = .{ .temp = 0 } } },

        ir.IR{ .ret = .{ .value = null } },
    };

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytecode = try generator.emit(&program);

    var harness = Harness.init(testing.allocator);
    defer harness.deinit();
    try harness.run(bytecode);

    const eq_true = harness.getLocal(0) orelse return TestError.MissingLocal;
    const eq_false = harness.getLocal(1) orelse return TestError.MissingLocal;
    const lt_true = harness.getLocal(2) orelse return TestError.MissingLocal;
    const gt_true = harness.getLocal(3) orelse return TestError.MissingLocal;
    const ne_true = harness.getLocal(4) orelse return TestError.MissingLocal;
    const lte_true = harness.getLocal(5) orelse return TestError.MissingLocal;
    const gte_false = harness.getLocal(6) orelse return TestError.MissingLocal;

    try testing.expectEqual(@as(ir.U256, 1), eq_true);
    try testing.expectEqual(@as(ir.U256, 0), eq_false);
    try testing.expectEqual(@as(ir.U256, 1), lt_true);
    try testing.expectEqual(@as(ir.U256, 1), gt_true);
    try testing.expectEqual(@as(ir.U256, 1), ne_true);
    try testing.expectEqual(@as(ir.U256, 1), lte_true);
    try testing.expectEqual(@as(ir.U256, 0), gte_false);
}

test "harness returns zero for missing table entry" {
    const load_program = [_]ir.IR{
        ir.IR{ .load_local = .{ .dest = .{ .temp = 0 }, .slot = 1 } },
        ir.IR{ .load_table = .{ .slot = 0, .dest = .{ .temp = 1 }, .key = .{ .temp = 0 } } },
        ir.IR{ .store_local = .{ .slot = 2, .value = .{ .temp = 1 } } },
        ir.IR{ .ret = .{ .value = null } },
    };

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();

    var harness = Harness.init(testing.allocator);
    defer harness.deinit();

    const load_bytes = try generator.emit(&load_program);
    try harness.setLocal(1, @as(ir.U256, 99));
    try harness.run(load_bytes);

    const loaded = harness.getLocal(2) orelse return TestError.MissingLocal;
    try testing.expectEqual(@as(ir.U256, 0), loaded);
}

test "harness skips while loop when condition false" {
    const source =
        "contract Control {\n" ++
        "    fn spin(flag: bool) {\n" ++
        "        while flag { }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    var builder = lowering.IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var generator = codegen.CodeGen.init(testing.allocator);
    defer generator.deinit();
    const bytecode = try generator.emit(builder.instructions.items);

    var harness = Harness.init(testing.allocator);
    defer harness.deinit();
    try harness.setLocal(0, @as(ir.U256, 0));

    try harness.run(bytecode);
}
