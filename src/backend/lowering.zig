const std = @import("std");
const ast = @import("../frontend/ast.zig");
const token = @import("../frontend/token.zig");
const parser = @import("../frontend/parser.zig");
const ir = @import("ir.zig");
const state_mapper_mod = @import("state_mapper.zig");

const StateMapper = state_mapper_mod.StateMapper;
const StateMapperError = state_mapper_mod.StateMapperError;

pub const LoweringError = error{
    OutOfMemory,
    DuplicateState,
    DuplicateLocal,
    UnknownLocal,
    UnknownState,
    UnknownTable,
    UnsupportedConstruct,
    NegativeLiteralUnsupported,
};

pub const FunctionEntry = struct {
    decl: ast.FnDecl,
    label: u32,
    start_index: usize,
    end_index: usize,
};

const TableAccess = struct {
    slot: u64,
    key: ir.Register,
};

pub const IRBuilder = struct {
    allocator: std.mem.Allocator,
    instructions: std.ArrayListUnmanaged(ir.IR) = .{},
    mapper: StateMapper,
    next_temp: u16 = 0,
    next_label: u32 = 0,
    functions: std.ArrayListUnmanaged(FunctionEntry) = .{},

    pub fn init(allocator: std.mem.Allocator) IRBuilder {
        return .{
            .allocator = allocator,
            .instructions = .{},
            .mapper = StateMapper.init(allocator),
            .next_temp = 0,
            .next_label = 0,
            .functions = .{},
        };
    }

    pub fn deinit(self: *IRBuilder) void {
        self.instructions.deinit(self.allocator);
        self.mapper.deinit();
        self.functions.deinit(self.allocator);
    }

    pub fn reset(self: *IRBuilder) void {
        self.instructions.clearRetainingCapacity();
        self.mapper.deinit();
        self.mapper = StateMapper.init(self.allocator);
        self.next_temp = 0;
        self.next_label = 0;
        self.functions.clearRetainingCapacity();
    }

    pub fn lowerModule(self: *IRBuilder, module: *ast.Module) LoweringError!void {
        for (module.contracts) |contract| {
            try self.lowerContract(contract);
        }
    }

    fn lowerContract(self: *IRBuilder, contract: ast.Contract) LoweringError!void {
        self.mapper.deinit();
        self.mapper = StateMapper.init(self.allocator);
        self.functions.clearRetainingCapacity();

        for (contract.items) |item| {
            switch (item) {
                .state => |state_decl| try self.assignStateSlot(state_decl.name),
                .table => |table_decl| try self.assignTableSlot(table_decl.name),
                else => {},
            }
        }

        for (contract.items) |item| {
            switch (item) {
                .function => |fn_decl| try self.lowerFunction(fn_decl),
                else => {},
            }
        }
    }

    fn assignStateSlot(self: *IRBuilder, name: []const u8) LoweringError!void {
        _ = self.mapper.assignState(name) catch |err| switch (err) {
            StateMapperError.DuplicateState => return LoweringError.DuplicateState,
            StateMapperError.OutOfMemory => return LoweringError.OutOfMemory,
        };
    }

    fn assignTableSlot(self: *IRBuilder, name: []const u8) LoweringError!void {
        _ = self.mapper.assignTable(name) catch |err| switch (err) {
            StateMapperError.DuplicateState => return LoweringError.DuplicateState,
            StateMapperError.OutOfMemory => return LoweringError.OutOfMemory,
        };
    }

    fn lowerFunction(self: *IRBuilder, fn_decl: ast.FnDecl) LoweringError!void {
        self.next_temp = 0;

        var ctx = FunctionContext.init(self);
        defer ctx.deinit();

        for (fn_decl.params) |param| {
            _ = try ctx.declareLocal(param.name);
        }

        const label_id = self.next_label;
        self.next_label += 1;
        const start_index = self.instructions.items.len;
        try self.emit(.{ .label = .{ .name = label_id } });

        try self.lowerBlock(&ctx, fn_decl.body);

        if (!ctx.has_explicit_return) {
            try self.emit(.{ .ret = .{ .value = null } });
        }

        const end_index = self.instructions.items.len;
        try self.functions.append(self.allocator, .{
            .decl = fn_decl,
            .label = label_id,
            .start_index = start_index,
            .end_index = end_index,
        });
    }

    fn lowerBlock(self: *IRBuilder, ctx: *FunctionContext, block: ast.Block) LoweringError!void {
        for (block.statements) |stmt| {
            if (ctx.has_explicit_return) break;
            try self.lowerStatement(ctx, stmt);
        }
    }

    fn lowerStatement(self: *IRBuilder, ctx: *FunctionContext, stmt: ast.Statement) LoweringError!void {
        switch (stmt) {
            .let_binding => |let_stmt| try self.lowerLet(ctx, let_stmt),
            .return_stmt => |ret_stmt| try self.lowerReturn(ctx, ret_stmt),
            .expression => |expr| {
                _ = try self.lowerExpr(ctx, expr);
            },
            .block => |blk| try self.lowerBlock(ctx, blk),
            .if_stmt => |if_stmt| try self.lowerIf(ctx, if_stmt),
            .while_stmt => |while_stmt| try self.lowerWhile(ctx, while_stmt),
        }
    }

    fn lowerLet(self: *IRBuilder, ctx: *FunctionContext, let_stmt: ast.LetStmt) LoweringError!void {
        const slot = try ctx.declareLocal(let_stmt.name);
        const value_reg = try self.lowerExpr(ctx, let_stmt.rhs);
        try self.emit(.{ .store_local = .{ .slot = slot, .value = value_reg } });
    }

    fn lowerReturn(self: *IRBuilder, ctx: *FunctionContext, ret_stmt: ast.ReturnStmt) LoweringError!void {
        var value_reg: ?ir.Register = null;
        if (ret_stmt.value) |expr| {
            value_reg = try self.lowerExpr(ctx, expr);
        }
        try self.emit(.{ .ret = .{ .value = value_reg } });
        ctx.has_explicit_return = true;
    }

    fn lowerIf(self: *IRBuilder, ctx: *FunctionContext, if_stmt: ast.IfStmt) LoweringError!void {
        const cond_reg = try self.lowerExpr(ctx, if_stmt.condition);
        const then_label = self.allocLabel();
        const end_label = self.allocLabel();
        const else_label = if (if_stmt.else_block != null) self.allocLabel() else end_label;

        try self.emit(.{ .jump_if = .{ .target = then_label, .condition = cond_reg } });
        try self.emit(.{ .jump = .{ .target = else_label } });

        try self.emit(.{ .label = .{ .name = then_label } });
        const original_flag = ctx.has_explicit_return;
        ctx.has_explicit_return = false;
        try self.lowerBlock(ctx, if_stmt.then_block);
        const then_returned = ctx.has_explicit_return;
        ctx.has_explicit_return = original_flag;
        try self.emit(.{ .jump = .{ .target = end_label } });

        if (if_stmt.else_block) |else_blk| {
            try self.emit(.{ .label = .{ .name = else_label } });
            ctx.has_explicit_return = false;
            try self.lowerBlock(ctx, else_blk);
            const else_returned = ctx.has_explicit_return;
            ctx.has_explicit_return = original_flag or (then_returned and else_returned);
        } else {
            if (else_label != end_label) {
                try self.emit(.{ .label = .{ .name = else_label } });
            }
            ctx.has_explicit_return = original_flag;
        }

        try self.emit(.{ .label = .{ .name = end_label } });
    }

    fn lowerWhile(self: *IRBuilder, ctx: *FunctionContext, while_stmt: ast.WhileStmt) LoweringError!void {
        const condition_label = self.allocLabel();
        const body_label = self.allocLabel();
        const end_label = self.allocLabel();

        try self.emit(.{ .label = .{ .name = condition_label } });
        const cond_reg = try self.lowerExpr(ctx, while_stmt.condition);
        try self.emit(.{ .jump_if = .{ .target = body_label, .condition = cond_reg } });
        try self.emit(.{ .jump = .{ .target = end_label } });

        try self.emit(.{ .label = .{ .name = body_label } });
        const original_flag = ctx.has_explicit_return;
        ctx.has_explicit_return = false;
        try self.lowerBlock(ctx, while_stmt.body);
        const body_returned = ctx.has_explicit_return;
        ctx.has_explicit_return = original_flag;

        if (!body_returned) {
            try self.emit(.{ .jump = .{ .target = condition_label } });
        }

        try self.emit(.{ .label = .{ .name = end_label } });
    }

    fn lowerExpr(self: *IRBuilder, ctx: *FunctionContext, expr: *ast.Expr) LoweringError!ir.Register {
        return switch (expr.*) {
            .identifier => |ident| try ctx.loadIdentifier(ident.name),
            .integer_literal => |lit| try self.lowerIntegerLiteral(lit.value),
            .bool_literal => |lit| try self.lowerBoolLiteral(lit.value),
            .binary => |binary| try self.lowerBinary(ctx, binary),
            .unary => |unary| try self.lowerUnary(ctx, unary),
            .assignment => |assignment| try self.lowerAssignment(ctx, assignment),
            .member => |member| try self.lowerMember(member),
            .index => |index_expr| try self.lowerIndex(ctx, index_expr),
            else => LoweringError.UnsupportedConstruct,
        };
    }

    fn lowerAssignment(self: *IRBuilder, ctx: *FunctionContext, assignment: ast.AssignmentExpr) LoweringError!ir.Register {
        const value_reg = try self.lowerExpr(ctx, assignment.value);
        switch (assignment.target.*) {
            .identifier => |ident| {
                const slot = try ctx.getLocal(ident.name);
                try self.emit(.{ .store_local = .{ .slot = slot, .value = value_reg } });
                return value_reg;
            },
            .member => |member| {
                const slot = try self.resolveStateSlot(member.name);
                try self.emit(.{ .store_state = .{ .slot = slot, .value = value_reg } });
                return value_reg;
            },
            .index => |index_expr| {
                const table_access = try self.prepareTableAccess(ctx, index_expr);
                try self.emit(.{ .store_table = .{ .slot = table_access.slot, .key = table_access.key, .value = value_reg } });
                return value_reg;
            },
            else => return LoweringError.UnsupportedConstruct,
        }
    }

    fn lowerMember(self: *IRBuilder, member: ast.MemberExpr) LoweringError!ir.Register {
        switch (member.base.*) {
            .identifier => |ident| {
                if (!std.mem.eql(u8, ident.name, "state")) {
                    return LoweringError.UnsupportedConstruct;
                }
                const slot = try self.resolveStateSlot(member.name);
                const dest = self.allocTemp();
                try self.emit(.{ .load_state = .{ .dest = dest, .slot = slot } });
                return dest;
            },
            else => return LoweringError.UnsupportedConstruct,
        }
    }

    fn lowerIndex(self: *IRBuilder, ctx: *FunctionContext, index_expr: ast.IndexExpr) LoweringError!ir.Register {
        const access = try self.prepareTableAccess(ctx, index_expr);
        const dest = self.allocTemp();
        try self.emit(.{ .load_table = .{ .dest = dest, .slot = access.slot, .key = access.key } });
        return dest;
    }

    fn prepareTableAccess(self: *IRBuilder, ctx: *FunctionContext, index_expr: ast.IndexExpr) LoweringError!TableAccess {
        switch (index_expr.target.*) {
            .member => |member| {
                switch (member.base.*) {
                    .identifier => |ident| {
                        if (!std.mem.eql(u8, ident.name, "state")) {
                            return LoweringError.UnsupportedConstruct;
                        }
                        const slot = try self.resolveTableSlot(member.name);
                        const key = try self.lowerExpr(ctx, index_expr.index);
                        return .{ .slot = slot, .key = key };
                    },
                    else => return LoweringError.UnsupportedConstruct,
                }
            },
            else => return LoweringError.UnsupportedConstruct,
        }
    }

    fn lowerBinary(self: *IRBuilder, ctx: *FunctionContext, binary: ast.BinaryExpr) LoweringError!ir.Register {
        const left = try self.lowerExpr(ctx, binary.left);
        const right = try self.lowerExpr(ctx, binary.right);
        const dest = self.allocTemp();

        const instruction = switch (binary.op) {
            .plus => ir.IR{ .add = .{ .dest = dest, .left = left, .right = right } },
            .minus => ir.IR{ .sub = .{ .dest = dest, .left = left, .right = right } },
            .star => ir.IR{ .mul = .{ .dest = dest, .left = left, .right = right } },
            .slash => ir.IR{ .div = .{ .dest = dest, .left = left, .right = right } },
            .percent => ir.IR{ .mod_ = .{ .dest = dest, .left = left, .right = right } },
            .and_and => ir.IR{ .and_ = .{ .dest = dest, .left = left, .right = right } },
            .or_or => ir.IR{ .or_ = .{ .dest = dest, .left = left, .right = right } },
            .equal_equal => ir.IR{ .eq = .{ .dest = dest, .left = left, .right = right } },
            .less => ir.IR{ .lt = .{ .dest = dest, .left = left, .right = right } },
            .greater => ir.IR{ .gt = .{ .dest = dest, .left = left, .right = right } },
            .bang_equal => ir.IR{ .ne = .{ .dest = dest, .left = left, .right = right } },
            .less_equal => ir.IR{ .lte = .{ .dest = dest, .left = left, .right = right } },
            .greater_equal => ir.IR{ .gte = .{ .dest = dest, .left = left, .right = right } },
            else => return LoweringError.UnsupportedConstruct,
        };

        try self.emit(instruction);
        return dest;
    }

    fn lowerUnary(self: *IRBuilder, ctx: *FunctionContext, unary: ast.UnaryExpr) LoweringError!ir.Register {
        switch (unary.op) {
            token.TokenKind.bang => {
                const value = try self.lowerExpr(ctx, unary.operand);
                const dest = self.allocTemp();
                const zero = ir.Register{ .constant = @as(ir.U256, 0) };
                try self.emit(.{ .eq = .{ .dest = dest, .left = value, .right = zero } });
                return dest;
            },
            else => return LoweringError.UnsupportedConstruct,
        }
    }

    fn lowerIntegerLiteral(self: *IRBuilder, value: i128) LoweringError!ir.Register {
        _ = self;
        if (value < 0) return LoweringError.NegativeLiteralUnsupported;
        const unsigned = std.math.cast(u128, value) orelse return LoweringError.UnsupportedConstruct;
        const constant = std.math.cast(ir.U256, unsigned) orelse return LoweringError.UnsupportedConstruct;
        return .{ .constant = constant };
    }

    fn lowerBoolLiteral(self: *IRBuilder, value: bool) LoweringError!ir.Register {
        _ = self;
        const as_u8: u8 = if (value) 1 else 0;
        const constant = std.math.cast(ir.U256, as_u8) orelse unreachable;
        return .{ .constant = constant };
    }

    fn resolveStateSlot(self: *IRBuilder, name: []const u8) LoweringError!u64 {
        if (self.mapper.getStateSlot(name)) |slot| return slot;
        return LoweringError.UnknownState;
    }

    fn resolveTableSlot(self: *IRBuilder, name: []const u8) LoweringError!u64 {
        if (self.mapper.getTableSlot(name)) |slot| return slot;
        return LoweringError.UnknownTable;
    }

    fn emit(self: *IRBuilder, instruction: ir.IR) LoweringError!void {
        self.instructions.append(self.allocator, instruction) catch return LoweringError.OutOfMemory;
    }

    fn allocTemp(self: *IRBuilder) ir.Register {
        const idx = self.next_temp;
        self.next_temp += 1;
        return .{ .temp = idx };
    }

    fn allocLabel(self: *IRBuilder) u32 {
        const current = self.next_label;
        self.next_label += 1;
        return current;
    }
};

pub fn getFunctions(builder: *IRBuilder) []const FunctionEntry {
    return builder.functions.items;
}

const FunctionContext = struct {
    builder: *IRBuilder,
    allocator: std.mem.Allocator,
    locals: std.StringHashMapUnmanaged(u16) = .{},
    next_local: u16 = 0,
    has_explicit_return: bool = false,

    fn init(builder: *IRBuilder) FunctionContext {
        return .{
            .builder = builder,
            .allocator = builder.allocator,
            .locals = .{},
            .next_local = 0,
            .has_explicit_return = false,
        };
    }

    fn deinit(self: *FunctionContext) void {
        self.locals.deinit(self.allocator);
    }

    fn declareLocal(self: *FunctionContext, name: []const u8) LoweringError!u16 {
        if (self.locals.contains(name)) return LoweringError.DuplicateLocal;
        const slot = self.next_local;
        self.next_local += 1;
        self.locals.put(self.allocator, name, slot) catch return LoweringError.OutOfMemory;
        return slot;
    }

    fn getLocal(self: *FunctionContext, name: []const u8) LoweringError!u16 {
        if (self.locals.get(name)) |slot| return slot;
        return LoweringError.UnknownLocal;
    }

    fn loadIdentifier(self: *FunctionContext, name: []const u8) LoweringError!ir.Register {
        if (std.mem.eql(u8, name, "state")) return LoweringError.UnsupportedConstruct;
        const slot = try self.getLocal(name);
        const dest = self.builder.allocTemp();
        try self.builder.emit(.{ .load_local = .{ .dest = dest, .slot = slot } });
        return dest;
    }
};

const testing = std.testing;
const TestError = error{ UnexpectedInstruction, UnsupportedFixture };
const RenderError = std.mem.Allocator.Error || TestError;

fn parseSource(source: []const u8) !ast.Tree {
    return try parser.parseModule(testing.allocator, source);
}

fn appendSlice(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, data: []const u8) RenderError!void {
    try buffer.appendSlice(allocator, data);
}

fn appendDecimal(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, value: anytype) RenderError!void {
    var temp: [64]u8 = undefined;
    const rendered = std.fmt.bufPrint(&temp, "{d}", .{value}) catch unreachable;
    for (rendered) |byte| {
        try buffer.append(allocator, byte);
    }
}

fn writeRegister(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, reg: ir.Register) RenderError!void {
    switch (reg) {
        .stack => |idx| {
            try appendSlice(buffer, allocator, "{\"stack\":");
            try appendDecimal(buffer, allocator, idx);
            try appendSlice(buffer, allocator, "}");
        },
        .temp => |idx| {
            try appendSlice(buffer, allocator, "{\"temp\":");
            try appendDecimal(buffer, allocator, idx);
            try appendSlice(buffer, allocator, "}");
        },
        .constant => |value| {
            var hex_buf: [64]u8 = undefined;
            const hex_rendered = std.fmt.bufPrint(&hex_buf, "{X}", .{value}) catch unreachable;
            try appendSlice(buffer, allocator, "{\"constant\":\"0x");
            for (hex_rendered) |byte| {
                try buffer.append(allocator, byte);
            }
            try appendSlice(buffer, allocator, "\"}");
        },
    }
}

fn writeBinary(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, name: []const u8, op: ir.BinaryOp) RenderError!void {
    try appendSlice(buffer, allocator, "{\"op\":\"");
    try appendSlice(buffer, allocator, name);
    try appendSlice(buffer, allocator, "\",\"dest\":");
    try writeRegister(buffer, allocator, op.dest);
    try appendSlice(buffer, allocator, ",\"left\":");
    try writeRegister(buffer, allocator, op.left);
    try appendSlice(buffer, allocator, ",\"right\":");
    try writeRegister(buffer, allocator, op.right);
    try appendSlice(buffer, allocator, "}");
}

fn writeInstruction(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, instr: ir.IR) RenderError!void {
    switch (instr) {
        .label => |label| {
            try appendSlice(buffer, allocator, "{\"op\":\"label\",\"label\":");
            try appendDecimal(buffer, allocator, label.name);
            try appendSlice(buffer, allocator, "}");
        },
        .load_local => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"load_local\",\"dest\":");
            try writeRegister(buffer, allocator, op.dest);
            try appendSlice(buffer, allocator, ",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, "}");
        },
        .store_local => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"store_local\",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, ",\"value\":");
            try writeRegister(buffer, allocator, op.value);
            try appendSlice(buffer, allocator, "}");
        },
        .load_state => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"load_state\",\"dest\":");
            try writeRegister(buffer, allocator, op.dest);
            try appendSlice(buffer, allocator, ",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, "}");
        },
        .store_state => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"store_state\",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, ",\"value\":");
            try writeRegister(buffer, allocator, op.value);
            try appendSlice(buffer, allocator, "}");
        },
        .load_table => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"load_table\",\"dest\":");
            try writeRegister(buffer, allocator, op.dest);
            try appendSlice(buffer, allocator, ",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, ",\"key\":");
            try writeRegister(buffer, allocator, op.key);
            try appendSlice(buffer, allocator, "}");
        },
        .store_table => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"store_table\",\"slot\":");
            try appendDecimal(buffer, allocator, op.slot);
            try appendSlice(buffer, allocator, ",\"key\":");
            try writeRegister(buffer, allocator, op.key);
            try appendSlice(buffer, allocator, ",\"value\":");
            try writeRegister(buffer, allocator, op.value);
            try appendSlice(buffer, allocator, "}");
        },
        .add => |op| try writeBinary(buffer, allocator, "add", op),
        .sub => |op| try writeBinary(buffer, allocator, "sub", op),
        .mul => |op| try writeBinary(buffer, allocator, "mul", op),
        .div => |op| try writeBinary(buffer, allocator, "div", op),
        .mod_ => |op| try writeBinary(buffer, allocator, "mod", op),
        .and_ => |op| try writeBinary(buffer, allocator, "and", op),
        .or_ => |op| try writeBinary(buffer, allocator, "or", op),
        .eq => |op| try writeBinary(buffer, allocator, "eq", op),
        .lt => |op| try writeBinary(buffer, allocator, "lt", op),
        .gt => |op| try writeBinary(buffer, allocator, "gt", op),
        .ne => |op| try writeBinary(buffer, allocator, "ne", op),
        .lte => |op| try writeBinary(buffer, allocator, "lte", op),
        .gte => |op| try writeBinary(buffer, allocator, "gte", op),
        .jump => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"jump\",\"target\":");
            try appendDecimal(buffer, allocator, op.target);
            try appendSlice(buffer, allocator, "}");
        },
        .jump_if => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"jump_if\",\"target\":");
            try appendDecimal(buffer, allocator, op.target);
            try appendSlice(buffer, allocator, ",\"condition\":");
            try writeRegister(buffer, allocator, op.condition);
            try appendSlice(buffer, allocator, "}");
        },
        .push => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"push\",\"value\":");
            try writeRegister(buffer, allocator, op.value);
            try appendSlice(buffer, allocator, "}");
        },
        .ret => |op| {
            try appendSlice(buffer, allocator, "{\"op\":\"ret\",\"value\":");
            if (op.value) |val| {
                try writeRegister(buffer, allocator, val);
            } else {
                try appendSlice(buffer, allocator, "null");
            }
            try appendSlice(buffer, allocator, "}");
        },
        else => return TestError.UnsupportedFixture,
    }
}

fn formatInstructions(allocator: std.mem.Allocator, instructions: []const ir.IR) RenderError![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    errdefer buffer.deinit(allocator);

    try appendSlice(&buffer, allocator, "[\n");
    for (instructions, 0..) |instr, idx| {
        try appendSlice(&buffer, allocator, "  ");
        try writeInstruction(&buffer, allocator, instr);
        if (idx + 1 != instructions.len) {
            try appendSlice(&buffer, allocator, ",\n");
        } else {
            try appendSlice(&buffer, allocator, "\n");
        }
    }
    try appendSlice(&buffer, allocator, "]");

    return buffer.toOwnedSlice(allocator);
}

pub fn renderControlFlowFixture(allocator: std.mem.Allocator) ![]u8 {
    const source =
        "contract Complex {\n" ++
        "    state balance: u64;\n" ++
        "    table records: Map<Address, u64>;\n" ++
        "    event Notified(flag: bool);\n" ++
        "    fn execute(flag: bool, key: Address, amount: u64, threshold: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        if flag && total > threshold {\n" ++
        "            state.balance = state.balance + total;\n" ++
        "        } else if !flag && total != threshold {\n" ++
        "            state.balance = state.balance - total;\n" ++
        "        } else {\n" ++
        "            total = threshold;\n" ++
        "        }\n" ++
        "        while total > threshold {\n" ++
        "            state.records[key] = total;\n" ++
        "            total = total - 1;\n" ++
        "        }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(allocator, source);
    defer tree.deinit();

    var builder = IRBuilder.init(allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    return try formatInstructions(allocator, builder.instructions.items);
}

pub fn verifyControlFlowFixture(allocator: std.mem.Allocator) !void {
    const rendered = try renderControlFlowFixture(allocator);
    defer allocator.free(rendered);

    const fixture = std.fs.cwd().readFileAlloc(
        "src/backend/testdata/lowering/control_flow.json",
        allocator,
        @enumFromInt(std.math.maxInt(usize)),
    ) catch |err| switch (err) {
        error.FileNotFound => {
            try std.fs.cwd().makePath("src/backend/testdata/lowering");
            var file = try std.fs.cwd().createFile(
                "src/backend/testdata/lowering/control_flow.json",
                .{ .truncate = true },
            );
            defer file.close();
            try file.writeAll(rendered);
            return;
        },
        else => return err,
    };
    defer allocator.free(fixture);

    const actual = std.mem.trim(u8, rendered, " \n\r\t");
    const expected = std.mem.trim(u8, fixture, " \n\r\t");
    try testing.expect(std.mem.eql(u8, expected, actual));
}

test "lower simple deposit function" {
    const source =
        "contract Treasury {\n" ++
        "    state balance: u64;\n" ++
        "    fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();

    try builder.lowerModule(tree.getModule());

    const instructions = builder.instructions.items;
    try testing.expectEqual(@as(usize, 8), instructions.len);

    switch (instructions[0]) {
        .label => |label| try testing.expectEqual(@as(u32, 0), label.name),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[1]) {
        .load_local => |op| try testing.expectEqual(@as(u16, 0), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[2]) {
        .store_local => |op| try testing.expectEqual(@as(u16, 1), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[3]) {
        .load_state => |op| try testing.expectEqual(@as(u64, 0), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[4]) {
        .load_local => |op| try testing.expectEqual(@as(u16, 1), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[5]) {
        .add => |op| {
            switch (op.dest) {
                .temp => |idx| try testing.expectEqual(@as(u16, 3), idx),
                else => return TestError.UnexpectedInstruction,
            }
            switch (op.left) {
                .temp => |idx| try testing.expectEqual(@as(u16, 1), idx),
                else => return TestError.UnexpectedInstruction,
            }
            switch (op.right) {
                .temp => |idx| try testing.expectEqual(@as(u16, 2), idx),
                else => return TestError.UnexpectedInstruction,
            }
        },
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[6]) {
        .store_state => |op| try testing.expectEqual(@as(u64, 0), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[7]) {
        .ret => |op| try testing.expect(op.value == null),
        else => return TestError.UnexpectedInstruction,
    }
}

test "lower while loop" {
    const source =
        "contract Control {\n" ++
        "    fn spin(flag: bool) {\n" ++
        "        while flag { }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();

    try builder.lowerModule(tree.getModule());

    const instructions = builder.instructions.items;
    try testing.expect(instructions.len >= 8);

    switch (instructions[0]) {
        .label => |label| try testing.expectEqual(@as(u32, 0), label.name),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[1]) {
        .label => |label| try testing.expectEqual(@as(u32, 1), label.name),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[2]) {
        .load_local => |op| try testing.expectEqual(@as(u16, 0), op.slot),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[3]) {
        .jump_if => |op| try testing.expectEqual(@as(u32, 2), op.target),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[4]) {
        .jump => |op| try testing.expectEqual(@as(u32, 3), op.target),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[5]) {
        .label => |label| try testing.expectEqual(@as(u32, 2), label.name),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[6]) {
        .jump => |op| try testing.expectEqual(@as(u32, 1), op.target),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[7]) {
        .label => |label| try testing.expectEqual(@as(u32, 3), label.name),
        else => return TestError.UnexpectedInstruction,
    }

    switch (instructions[instructions.len - 1]) {
        .ret => |op| try testing.expect(op.value == null),
        else => return TestError.UnexpectedInstruction,
    }
}

test "lower assignment to local" {
    const source =
        "contract C {\n" ++
        "    state counter: u64;\n" ++
        "    fn set(value: u64) {\n" ++
        "        let current = value;\n" ++
        "        current = current + 1;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();

    try builder.lowerModule(tree.getModule());

    const instructions = builder.instructions.items;
    try testing.expect(instructions.len >= 7);

    var found_reassignment = false;
    for (instructions) |instr| {
        switch (instr) {
            .store_local => |op| {
                if (op.slot == 1) found_reassignment = true;
            },
            else => {},
        }
    }
    try testing.expect(found_reassignment);
}

test "lower comparisons" {
    const source =
        "contract Logic {\n" ++
        "    fn compare(a: u64, b: u64) {\n" ++
        "        let eq_val = a == b;\n" ++
        "        let lt_val = a < b;\n" ++
        "        let gt_val = a > b;\n" ++
        "        let ne_val = a != b;\n" ++
        "        let lte_val = a <= b;\n" ++
        "        let gte_val = a >= b;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();

    try builder.lowerModule(tree.getModule());

    var saw_eq = false;
    var saw_lt = false;
    var saw_gt = false;
    var saw_ne = false;
    var saw_lte = false;
    var saw_gte = false;

    for (builder.instructions.items) |instr| {
        switch (instr) {
            .eq => saw_eq = true,
            .lt => saw_lt = true,
            .gt => saw_gt = true,
            .ne => saw_ne = true,
            .lte => saw_lte = true,
            .gte => saw_gte = true,
            else => {},
        }
    }

    try testing.expect(saw_eq);
    try testing.expect(saw_lt);
    try testing.expect(saw_gt);
    try testing.expect(saw_ne);
    try testing.expect(saw_lte);
    try testing.expect(saw_gte);
}

test "lower table assignments and loads" {
    const source =
        "contract Tables {\n" ++
        "    table balances: Map<Address, u64>;\n" ++
        "    fn sync(key: u64, value: u64) {\n" ++
        "        state.balances[key] = value;\n" ++
        "        let current = state.balances[key];\n" ++
        "        state.balances[key] = current;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var load_count: usize = 0;
    var store_count: usize = 0;

    for (builder.instructions.items) |instr| {
        switch (instr) {
            .load_table => |op| {
                load_count += 1;
                try testing.expectEqual(@as(u64, 0), op.slot);
            },
            .store_table => |op| {
                store_count += 1;
                try testing.expectEqual(@as(u64, 0), op.slot);
            },
            else => {},
        }
    }

    try testing.expectEqual(@as(usize, 1), load_count);
    try testing.expectEqual(@as(usize, 2), store_count);
}

test "lower logical control flow" {
    const source =
        "contract Logic {\n" ++
        "    fn branch(flag: bool, ready: bool) {\n" ++
        "        let mut guard = 0;\n" ++
        "        if flag && ready {\n" ++
        "            guard = guard + 1;\n" ++
        "        } else if !flag || ready {\n" ++
        "            guard = guard + 2;\n" ++
        "        }\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();

    var builder = IRBuilder.init(testing.allocator);
    defer builder.deinit();
    try builder.lowerModule(tree.getModule());

    var saw_and = false;
    var saw_or = false;
    for (builder.instructions.items) |instr| {
        switch (instr) {
            .and_ => saw_and = true,
            .or_ => saw_or = true,
            else => {},
        }
    }

    try testing.expect(saw_and);
    try testing.expect(saw_or);
}
