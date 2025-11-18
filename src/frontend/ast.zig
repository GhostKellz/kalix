const std = @import("std");
const token = @import("token.zig");

pub const Tree = struct {
    arena: std.heap.ArenaAllocator,
    module: ?*Module,

    pub fn init(gpa: std.mem.Allocator) Tree {
        return .{
            .arena = std.heap.ArenaAllocator.init(gpa),
            .module = null,
        };
    }

    pub fn allocator(self: *Tree) std.mem.Allocator {
        return self.arena.allocator();
    }

    pub fn setModule(self: *Tree, module_ptr: *Module) void {
        self.module = module_ptr;
    }

    pub fn getModule(self: *Tree) *Module {
        return self.module.?;
    }

    pub fn deinit(self: *Tree) void {
        self.arena.deinit();
    }

    pub fn createExpr(self: *Tree, expr: Expr) !*Expr {
        const ptr = try self.allocator().create(Expr);
        ptr.* = expr;
        return ptr;
    }

    pub fn createStatements(self: *Tree, list: []Statement) ![]Statement {
        const dst = try self.allocator().alloc(Statement, list.len);
        std.mem.copyForwards(Statement, dst, list);
        return dst;
    }

    pub fn createExprList(self: *Tree, list: []*Expr) ![]*Expr {
        const dst = try self.allocator().alloc(*Expr, list.len);
        std.mem.copyForwards(*Expr, dst, list);
        return dst;
    }

    pub fn createContractItems(self: *Tree, list: []ContractItem) ![]ContractItem {
        const dst = try self.allocator().alloc(ContractItem, list.len);
        std.mem.copyForwards(ContractItem, dst, list);
        return dst;
    }

    pub fn createContracts(self: *Tree, list: []Contract) ![]Contract {
        const dst = try self.allocator().alloc(Contract, list.len);
        std.mem.copyForwards(Contract, dst, list);
        return dst;
    }

    pub fn createParams(self: *Tree, list: []Param) ![]Param {
        const dst = try self.allocator().alloc(Param, list.len);
        std.mem.copyForwards(Param, dst, list);
        return dst;
    }
};

pub const Module = struct {
    contracts: []const Contract,
};

pub const Contract = struct {
    name: []const u8,
    items: []const ContractItem,
};

pub const ContractItem = union(enum) {
    state: StateDecl,
    table: TableDecl,
    event: EventDecl,
    function: FnDecl,
};

pub const StateDecl = struct {
    name: []const u8,
    ty: TypeExpr,
    is_public: bool,
};

pub const TableDecl = struct {
    name: []const u8,
    ty: TypeExpr,
    is_public: bool,
};

pub const EventDecl = struct {
    name: []const u8,
    params: []const Param,
    is_public: bool,
};

pub const FnDecl = struct {
    name: []const u8,
    params: []const Param,
    return_type: ?TypeExpr,
    qualifiers: Qualifiers,
    body: Block,
};

pub const Qualifiers = struct {
    is_public: bool,
    view: bool,
    payable: bool,
};

pub const Param = struct {
    name: []const u8,
    ty: TypeExpr,
    is_mutable: bool,
    is_const: bool,
};

pub const Block = struct {
    statements: []const Statement,
};

pub const Statement = union(enum) {
    let_binding: LetStmt,
    return_stmt: ReturnStmt,
    expression: *Expr,
    block: Block,
    if_stmt: IfStmt,
    while_stmt: WhileStmt,
};

pub const LetStmt = struct {
    name: []const u8,
    is_mutable: bool,
    is_const: bool,
    rhs: *Expr,
};

pub const ReturnStmt = struct {
    value: ?*Expr,
};

pub const IfStmt = struct {
    condition: *Expr,
    then_block: Block,
    else_block: ?Block,
};

pub const WhileStmt = struct {
    condition: *Expr,
    body: Block,
};

pub const Expr = union(enum) {
    identifier: IdentifierExpr,
    integer_literal: IntegerLiteral,
    bool_literal: BoolLiteral,
    string_literal: StringLiteral,
    call: CallExpr,
    binary: BinaryExpr,
    unary: UnaryExpr,
    member: MemberExpr,
    assignment: AssignmentExpr,
    index: IndexExpr,
};

pub const IdentifierExpr = struct {
    name: []const u8,
};

pub const IntegerLiteral = struct {
    value: i128,
};

pub const BoolLiteral = struct {
    value: bool,
};

pub const StringLiteral = struct {
    value: []const u8,
};

pub const CallExpr = struct {
    callee: *Expr,
    arguments: []const *Expr,
};

pub const MemberExpr = struct {
    base: *Expr,
    name: []const u8,
};

pub const IndexExpr = struct {
    target: *Expr,
    index: *Expr,
};

pub const BinaryExpr = struct {
    op: token.TokenKind,
    left: *Expr,
    right: *Expr,
};

pub const UnaryExpr = struct {
    op: token.TokenKind,
    operand: *Expr,
};

pub const AssignmentExpr = struct {
    target: *Expr,
    value: *Expr,
};

pub const TypeExpr = struct {
    name: []const u8,
    generics: []const TypeExpr,
};

pub fn initType(name: []const u8) TypeExpr {
    return .{ .name = name, .generics = &[_]TypeExpr{} };
}

pub fn makeType(self: *Tree, base: TypeExpr) !TypeExpr {
    if (base.generics.len == 0) return base;
    const generics = try self.allocator().alloc(TypeExpr, base.generics.len);
    std.mem.copyForwards(TypeExpr, generics, base.generics);
    return .{ .name = base.name, .generics = generics };
}

const testing = std.testing;

test "tree allocation lifecycle" {
    var tree = Tree.init(testing.allocator);
    defer tree.deinit();

    const expr = try tree.createExpr(.{ .integer_literal = .{ .value = 42 } });
    try testing.expectEqual(@as(i128, 42), expr.integer_literal.value);
}
