const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");

pub const SemanticError = error{
    DuplicateState,
    DuplicateFunction,
    DuplicateTable,
    DuplicateEvent,
    DuplicateSymbol,
    DuplicateParameter,
    UnknownIdentifier,
    UnknownFunction,
    UnknownStateField,
    ImmutableBinding,
    NonConstantInitializer,
    TypeMismatch,
    ArgumentCountMismatch,
    ReturnTypeMismatch,
    MissingReturnValue,
    UnexpectedReturnValue,
    UnsupportedMemberAccess,
    UnsupportedLoop,
    InvalidCallTarget,
    ReservedIdentifier,
    VoidValueNotAllowed,
    OutOfMemory,
};

const Tag = enum {
    void,
    bool,
    string,
    bytes,
    integer,
    custom,
};

const IntegerInfo = struct {
    signed: bool,
    bits: u16,
};

const Type = union(Tag) {
    void,
    bool,
    string,
    bytes,
    integer: IntegerInfo,
    custom: []const u8,

    fn equals(self: Type, other: Type) bool {
        if (@intFromEnum(self) != @intFromEnum(other)) return false;
        return switch (self) {
            .void, .bool, .string, .bytes => true,
            .integer => self.integer.signed == other.integer.signed and self.integer.bits == other.integer.bits,
            .custom => std.mem.eql(u8, self.custom, other.custom),
        };
    }

    fn isNumeric(self: Type) bool {
        return switch (self) {
            .integer => true,
            else => false,
        };
    }

    fn isBoolean(self: Type) bool {
        return switch (self) {
            .bool => true,
            else => false,
        };
    }

    fn isVoid(self: Type) bool {
        return switch (self) {
            .void => true,
            else => false,
        };
    }

    fn description(self: Type) []const u8 {
        return switch (self) {
            .void => "void",
            .bool => "bool",
            .string => "string",
            .bytes => "bytes",
            .integer => if (self.integer.signed) switch (self.integer.bits) {
                8 => "i8",
                16 => "i16",
                32 => "i32",
                64 => "i64",
                128 => "i128",
                else => "int",
            } else switch (self.integer.bits) {
                8 => "u8",
                16 => "u16",
                32 => "u32",
                64 => "u64",
                128 => "u128",
                else => "uint",
            },
            .custom => self.custom,
        };
    }
};

pub const StateMetadata = struct {
    name: []const u8,
    type_name: []const u8,
    is_public: bool,
};

pub const TableMetadata = struct {
    name: []const u8,
    type_name: []const u8,
    is_public: bool,
};

pub const EventFieldMetadata = struct {
    name: []const u8,
    type_name: []const u8,
};

pub const EventMetadata = struct {
    name: []const u8,
    is_public: bool,
    fields: []EventFieldMetadata,
};

pub const FunctionParamMetadata = struct {
    name: []const u8,
    type_name: []const u8,
    is_mutable: bool,
    is_const: bool,
};

pub const FunctionMetadata = struct {
    name: []const u8,
    return_type: []const u8,
    params: []FunctionParamMetadata,
    is_public: bool,
    view: bool,
    payable: bool,
};

pub const ContractMetadata = struct {
    name: []const u8,
    states: []StateMetadata,
    tables: []TableMetadata,
    events: []EventMetadata,
    functions: []FunctionMetadata,
};

pub const Metadata = struct {
    contracts: []ContractMetadata = &[_]ContractMetadata{},
    owned: bool = false,

    pub fn deinit(self: *Metadata, allocator: std.mem.Allocator) void {
        if (!self.owned) return;
        for (self.contracts) |contract| {
            allocator.free(contract.states);
            allocator.free(contract.tables);
            for (contract.events) |event| {
                allocator.free(event.fields);
            }
            allocator.free(contract.events);
            for (contract.functions) |function| {
                allocator.free(function.params);
            }
            allocator.free(contract.functions);
        }
        allocator.free(self.contracts);
        self.contracts = &[_]ContractMetadata{};
        self.owned = false;
    }
};

const ContractTemp = struct {
    name: []const u8,
    states: std.ArrayListUnmanaged(StateMetadata) = .{},
    tables: std.ArrayListUnmanaged(TableMetadata) = .{},
    events: std.ArrayListUnmanaged(EventTemp) = .{},
    functions: std.ArrayListUnmanaged(FunctionTemp) = .{},
};

const EventTemp = struct {
    name: []const u8,
    is_public: bool,
    fields: std.ArrayListUnmanaged(EventFieldMetadata) = .{},
};

const FunctionTemp = struct {
    name: []const u8,
    return_type: []const u8,
    is_public: bool,
    view: bool,
    payable: bool,
    params: std.ArrayListUnmanaged(FunctionParamMetadata) = .{},
};

const MetadataBuilder = struct {
    allocator: std.mem.Allocator,
    contracts: std.ArrayListUnmanaged(ContractTemp) = .{},

    pub fn init(allocator: std.mem.Allocator) MetadataBuilder {
        return .{ .allocator = allocator, .contracts = .{} };
    }

    pub fn deinit(self: *MetadataBuilder) void {
        for (self.contracts.items) |*contract| {
            contract.states.deinit(self.allocator);
            contract.tables.deinit(self.allocator);
            for (contract.events.items) |*event| {
                event.fields.deinit(self.allocator);
            }
            contract.events.deinit(self.allocator);
            for (contract.functions.items) |*function| {
                function.params.deinit(self.allocator);
            }
            contract.functions.deinit(self.allocator);
        }
        self.contracts.deinit(self.allocator);
    }

    fn beginContract(self: *MetadataBuilder, name: []const u8) !*ContractTemp {
        try self.contracts.append(self.allocator, .{ .name = name });
        return &self.contracts.items[self.contracts.items.len - 1];
    }

    fn addState(self: *MetadataBuilder, contract: *ContractTemp, name: []const u8, type_name: []const u8, is_public: bool) !void {
        try contract.states.append(self.allocator, .{ .name = name, .type_name = type_name, .is_public = is_public });
    }

    fn addTable(self: *MetadataBuilder, contract: *ContractTemp, name: []const u8, type_name: []const u8, is_public: bool) !void {
        try contract.tables.append(self.allocator, .{ .name = name, .type_name = type_name, .is_public = is_public });
    }

    fn addEvent(self: *MetadataBuilder, contract: *ContractTemp, name: []const u8, is_public: bool, info: EventInfo) !void {
        var temp = EventTemp{ .name = name, .is_public = is_public };
        for (info.fields) |field| {
            try temp.fields.append(self.allocator, .{ .name = field.name, .type_name = field.ty.description() });
        }
        try contract.events.append(self.allocator, temp);
    }

    fn addFunction(self: *MetadataBuilder, contract: *ContractTemp, decl: *const ast.FnDecl, info: FunctionInfo) !void {
        var temp = FunctionTemp{
            .name = decl.name,
            .return_type = info.return_type.description(),
            .is_public = decl.qualifiers.is_public,
            .view = decl.qualifiers.view,
            .payable = decl.qualifiers.payable,
        };
        for (info.params) |param| {
            try temp.params.append(self.allocator, .{
                .name = param.name,
                .type_name = param.ty.description(),
                .is_mutable = param.mutable,
                .is_const = param.is_const,
            });
        }
        try contract.functions.append(self.allocator, temp);
    }

    pub fn finish(self: *MetadataBuilder) !Metadata {
        var contracts_slice = try self.allocator.alloc(ContractMetadata, self.contracts.items.len);
        for (self.contracts.items, 0..) |contract, index| {
            const states = try self.allocator.alloc(StateMetadata, contract.states.items.len);
            std.mem.copyForwards(StateMetadata, states, contract.states.items);

            const tables = try self.allocator.alloc(TableMetadata, contract.tables.items.len);
            std.mem.copyForwards(TableMetadata, tables, contract.tables.items);

            const events = try self.allocator.alloc(EventMetadata, contract.events.items.len);
            for (contract.events.items, 0..) |event, event_index| {
                const fields = try self.allocator.alloc(EventFieldMetadata, event.fields.items.len);
                std.mem.copyForwards(EventFieldMetadata, fields, event.fields.items);
                events[event_index] = .{
                    .name = event.name,
                    .is_public = event.is_public,
                    .fields = fields,
                };
            }

            const functions = try self.allocator.alloc(FunctionMetadata, contract.functions.items.len);
            for (contract.functions.items, 0..) |function, fn_index| {
                const params = try self.allocator.alloc(FunctionParamMetadata, function.params.items.len);
                std.mem.copyForwards(FunctionParamMetadata, params, function.params.items);
                functions[fn_index] = .{
                    .name = function.name,
                    .return_type = function.return_type,
                    .params = params,
                    .is_public = function.is_public,
                    .view = function.view,
                    .payable = function.payable,
                };
            }

            contracts_slice[index] = .{
                .name = contract.name,
                .states = states,
                .tables = tables,
                .events = events,
                .functions = functions,
            };
        }

        return Metadata{ .contracts = contracts_slice, .owned = true };
    }
};

fn typesCompatible(target: Type, value: Type) bool {
    if (target.equals(value)) return true;
    if (target.isNumeric() and value.isNumeric()) {
        const t = target.integer;
        const v = value.integer;
        if (t.signed != v.signed) return false;
        return v.bits <= t.bits;
    }
    return false;
}

fn combineNumeric(left: Type, right: Type) ?Type {
    if (!left.isNumeric() or !right.isNumeric()) return null;
    const l = left.integer;
    const r = right.integer;
    if (l.signed != r.signed) return null;
    const bits = if (l.bits >= r.bits) l.bits else r.bits;
    return Type{ .integer = .{ .signed = l.signed, .bits = bits } };
}

const StateInfo = struct {
    ty: Type,
};

const TableInfo = struct {
    ty: Type,
};

const EventField = struct {
    name: []const u8,
    ty: Type,
};

const EventInfo = struct {
    fields: []EventField,
};

const FunctionParam = struct {
    name: []const u8,
    ty: Type,
    mutable: bool,
    is_const: bool,
};

const FunctionInfo = struct {
    params: []FunctionParam,
    return_type: Type,
    decl: *const ast.FnDecl,
};

const SymbolKind = enum {
    variable,
    parameter,
};

const Symbol = struct {
    ty: Type,
    mutable: bool,
    is_const: bool,
    kind: SymbolKind,
};

const Scope = struct {
    allocator: std.mem.Allocator,
    parent: ?*Scope,
    symbols: std.StringHashMapUnmanaged(Symbol) = .{},

    fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return .{ .allocator = allocator, .parent = parent, .symbols = .{} };
    }

    fn deinit(self: *Scope) void {
        self.symbols.deinit(self.allocator);
    }

    fn declare(self: *Scope, name: []const u8, symbol: Symbol) SemanticError!void {
        if (std.mem.eql(u8, name, "state")) return error.ReservedIdentifier;
        if (self.symbols.contains(name)) return error.DuplicateSymbol;
        try self.symbols.put(self.allocator, name, symbol);
    }

    fn resolve(self: *Scope, name: []const u8) ?*const Symbol {
        if (self.symbols.getPtr(name)) |ptr| return ptr;
        if (self.parent) |parent_scope| {
            return parent_scope.resolve(name);
        }
        return null;
    }
};

const ContractContext = struct {
    allocator: std.mem.Allocator,
    states: std.StringHashMapUnmanaged(StateInfo) = .{},
    tables: std.StringHashMapUnmanaged(TableInfo) = .{},
    events: std.StringHashMapUnmanaged(EventInfo) = .{},
    functions: std.StringHashMapUnmanaged(FunctionInfo) = .{},

    fn deinit(self: *ContractContext) void {
        var func_iter = self.functions.iterator();
        while (func_iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.params);
        }
        self.functions.deinit(self.allocator);
        self.states.deinit(self.allocator);
        self.tables.deinit(self.allocator);

        var event_iter = self.events.iterator();
        while (event_iter.next()) |entry| {
            self.allocator.free(entry.value_ptr.fields);
        }
        self.events.deinit(self.allocator);
    }
};

const Analyzer = struct {
    allocator: std.mem.Allocator,
    tree: *ast.Tree,
    metadata_builder: ?*MetadataBuilder = null,

    fn run(self: *Analyzer) SemanticError!void {
        const module = self.tree.getModule();
        for (module.contracts) |contract| {
            try self.analyzeContract(contract);
        }
    }

    fn analyzeContract(self: *Analyzer, contract: ast.Contract) SemanticError!void {
        var ctx = ContractContext{ .allocator = self.allocator };
        defer ctx.deinit();

        var metadata_contract: ?*ContractTemp = null;
        if (self.metadata_builder) |builder| {
            metadata_contract = builder.beginContract(contract.name) catch return error.OutOfMemory;
        }

        // First pass: gather states and function signatures.
        for (contract.items) |item| {
            switch (item) {
                .state => |state_decl| {
                    if (ctx.states.contains(state_decl.name)) return error.DuplicateState;
                    const ty = try self.resolveType(state_decl.ty);
                    try ctx.states.put(self.allocator, state_decl.name, .{ .ty = ty });
                    if (metadata_contract) |meta| {
                        self.metadata_builder.?.addState(meta, state_decl.name, ty.description(), state_decl.is_public) catch return error.OutOfMemory;
                    }
                },
                .table => |table_decl| {
                    if (ctx.tables.contains(table_decl.name)) return error.DuplicateTable;
                    const ty = try self.resolveType(table_decl.ty);
                    try ctx.tables.put(self.allocator, table_decl.name, .{ .ty = ty });
                    if (metadata_contract) |meta| {
                        self.metadata_builder.?.addTable(meta, table_decl.name, ty.description(), table_decl.is_public) catch return error.OutOfMemory;
                    }
                },
                .event => |event_decl| {
                    if (ctx.events.contains(event_decl.name)) return error.DuplicateEvent;
                    const info = try self.buildEventInfo(&event_decl);
                    try ctx.events.put(self.allocator, event_decl.name, info);
                    if (metadata_contract) |meta| {
                        self.metadata_builder.?.addEvent(meta, event_decl.name, event_decl.is_public, info) catch return error.OutOfMemory;
                    }
                },
                .function => |fn_decl| {
                    if (ctx.functions.contains(fn_decl.name)) return error.DuplicateFunction;
                    const info = try self.buildFunctionInfo(&fn_decl);
                    try ctx.functions.put(self.allocator, fn_decl.name, info);
                    if (metadata_contract) |meta| {
                        self.metadata_builder.?.addFunction(meta, &fn_decl, info) catch return error.OutOfMemory;
                    }
                },
            }
        }

        // Second pass: analyze function bodies.
        var iter = ctx.functions.iterator();
        while (iter.next()) |entry| {
            try self.checkFunction(&ctx, entry.value_ptr.*);
        }
    }

    fn buildFunctionInfo(self: *Analyzer, fn_decl: *const ast.FnDecl) SemanticError!FunctionInfo {
        var params_list = std.ArrayListUnmanaged(FunctionParam){};
        defer params_list.deinit(self.allocator);
        var seen = std.StringHashMapUnmanaged(void){};
        defer seen.deinit(self.allocator);

        for (fn_decl.params) |param| {
            if (std.mem.eql(u8, param.name, "state")) return error.ReservedIdentifier;
            if (seen.contains(param.name)) return error.DuplicateParameter;
            try seen.put(self.allocator, param.name, {});
            const ty = try self.resolveType(param.ty);
            try params_list.append(self.allocator, .{
                .name = param.name,
                .ty = ty,
                .mutable = param.is_mutable,
                .is_const = param.is_const,
            });
        }

        const return_type = if (fn_decl.return_type) |ret| try self.resolveType(ret) else Type.void;
        const params = try params_list.toOwnedSlice(self.allocator);
        return .{ .params = params, .return_type = return_type, .decl = fn_decl };
    }

    fn buildEventInfo(self: *Analyzer, event_decl: *const ast.EventDecl) SemanticError!EventInfo {
        var fields = std.ArrayListUnmanaged(EventField){};
        defer fields.deinit(self.allocator);
        var seen = std.StringHashMapUnmanaged(void){};
        defer seen.deinit(self.allocator);

        for (event_decl.params) |param| {
            if (seen.contains(param.name)) return error.DuplicateParameter;
            try seen.put(self.allocator, param.name, {});
            const ty = try self.resolveType(param.ty);
            try fields.append(self.allocator, .{ .name = param.name, .ty = ty });
        }

        return .{ .fields = try fields.toOwnedSlice(self.allocator) };
    }

    fn checkFunction(self: *Analyzer, ctx: *ContractContext, info: FunctionInfo) SemanticError!void {
        var root_scope = Scope.init(self.allocator, null);
        defer root_scope.deinit();

        for (info.params) |param| {
            try root_scope.declare(param.name, .{ .ty = param.ty, .mutable = param.mutable, .is_const = param.is_const, .kind = .parameter });
        }

        var checker = FunctionChecker{
            .analyzer = self,
            .contract = ctx,
            .info = info,
        };
        try checker.checkBlock(info.decl.body, &root_scope);
    }

    fn resolveType(self: *Analyzer, type_expr: ast.TypeExpr) SemanticError!Type {
        const name = type_expr.name;
        if (std.mem.eql(u8, name, "bool")) return Type.bool;
        if (std.mem.eql(u8, name, "string")) return Type.string;
        if (std.mem.eql(u8, name, "bytes")) return Type.bytes;

        if (name.len >= 2) {
            const prefix = name[0];
            if ((prefix == 'u' or prefix == 'i')) {
                if (parseBits(name[1..])) |bits| {
                    return Type{ .integer = .{ .signed = prefix == 'i', .bits = bits } };
                }
            }
        }

        if (type_expr.generics.len == 0) {
            return Type{ .custom = name };
        }

        var builder = std.ArrayListUnmanaged(u8){};
        const arena = self.tree.allocator();
        defer builder.deinit(arena);
        try builder.appendSlice(arena, name);
        try builder.append(arena, '<');
        for (type_expr.generics, 0..) |arg, index| {
            if (index != 0) try builder.appendSlice(arena, ", ");
            const resolved = try self.resolveType(arg);
            try builder.appendSlice(arena, resolved.description());
        }
        try builder.append(arena, '>');
        const formatted = try builder.toOwnedSlice(arena);
        return Type{ .custom = formatted };
    }
};

const FunctionChecker = struct {
    analyzer: *Analyzer,
    contract: *ContractContext,
    info: FunctionInfo,

    fn checkBlock(self: *FunctionChecker, block: ast.Block, parent_scope: *Scope) SemanticError!void {
        var scope = Scope.init(self.analyzer.allocator, parent_scope);
        defer scope.deinit();

        for (block.statements) |stmt| {
            try self.checkStatement(stmt, &scope);
        }
    }

    fn checkStatement(self: *FunctionChecker, stmt: ast.Statement, scope: *Scope) SemanticError!void {
        return switch (stmt) {
            .let_binding => |let_stmt| self.checkLet(let_stmt, scope),
            .return_stmt => |ret| self.checkReturn(ret, scope),
            .expression => |expr_ptr| {
                _ = try self.checkExpr(expr_ptr, scope);
            },
            .block => |blk| self.checkBlock(blk, scope),
            .if_stmt => |if_stmt| self.checkIf(if_stmt, scope),
            .while_stmt => |while_stmt| self.checkWhile(while_stmt, scope),
        };
    }

    fn checkIf(self: *FunctionChecker, stmt: ast.IfStmt, scope: *Scope) SemanticError!void {
        const cond_type = try self.checkExpr(stmt.condition, scope);
        if (!cond_type.isBoolean()) return error.TypeMismatch;

        try self.checkBlock(stmt.then_block, scope);
        if (stmt.else_block) |else_blk| {
            try self.checkBlock(else_blk, scope);
        }
    }

    fn checkWhile(self: *FunctionChecker, stmt: ast.WhileStmt, scope: *Scope) SemanticError!void {
        const cond_type = try self.checkExpr(stmt.condition, scope);
        if (!cond_type.isBoolean()) return error.TypeMismatch;
        try self.checkBlock(stmt.body, scope);
        // Loop lowering is not yet implemented in Phase 2; report unsupported construct for now.
        return error.UnsupportedLoop;
    }

    fn checkLet(self: *FunctionChecker, stmt: ast.LetStmt, scope: *Scope) SemanticError!void {
        const value_type = try self.checkExpr(stmt.rhs, scope);
        if (stmt.is_const and !FunctionChecker.isCompileTimeConstant(stmt.rhs)) return error.NonConstantInitializer;
        if (value_type.isVoid()) return error.VoidValueNotAllowed;
        try scope.declare(stmt.name, .{ .ty = value_type, .mutable = stmt.is_mutable, .is_const = stmt.is_const, .kind = .variable });
    }

    fn checkReturn(self: *FunctionChecker, stmt: ast.ReturnStmt, scope: *Scope) SemanticError!void {
        const expected = self.info.return_type;
        if (stmt.value) |expr_ptr| {
            const value_type = try self.checkExpr(expr_ptr, scope);
            if (expected.isVoid()) return error.UnexpectedReturnValue;
            if (!typesCompatible(expected, value_type)) return error.ReturnTypeMismatch;
        } else {
            if (!expected.isVoid()) return error.MissingReturnValue;
        }
    }

    fn checkExpr(self: *FunctionChecker, expr: *ast.Expr, scope: *Scope) SemanticError!Type {
        return switch (expr.*) {
            .identifier => |ident| self.resolveIdentifier(ident.name, scope),
            .integer_literal => |lit| literalType(lit.value),
            .bool_literal => |_| Type.bool,
            .string_literal => |_| Type.string,
            .call => |call| self.checkCall(call, scope),
            .binary => |binary| self.checkBinary(binary, scope),
            .unary => |unary| self.checkUnary(unary, scope),
            .member => |member| self.checkMember(member, scope),
            .assignment => |assignment| self.checkAssignment(assignment, scope),
            .index => |index| self.checkIndex(index, scope),
        };
    }

    fn resolveIdentifier(_: *FunctionChecker, name: []const u8, scope: *Scope) SemanticError!Type {
        if (scope.resolve(name)) |symbol| {
            return symbol.ty;
        }
        return error.UnknownIdentifier;
    }

    fn checkAssignment(self: *FunctionChecker, assignment: ast.AssignmentExpr, scope: *Scope) SemanticError!Type {
        const target_info = try self.resolveAssignable(assignment.target, scope);
        if (!target_info.mutable) return error.ImmutableBinding;
        const value_type = try self.checkExpr(assignment.value, scope);
        if (!typesCompatible(target_info.ty, value_type)) return error.TypeMismatch;
        return target_info.ty;
    }

    const AssignTarget = struct {
        ty: Type,
        mutable: bool,
    };

    fn resolveAssignable(self: *FunctionChecker, expr: *ast.Expr, scope: *Scope) SemanticError!AssignTarget {
        return switch (expr.*) {
            .identifier => |ident| {
                if (scope.resolve(ident.name)) |symbol| {
                    return AssignTarget{ .ty = symbol.ty, .mutable = symbol.mutable };
                }
                return error.UnknownIdentifier;
            },
            .member => |member| blk: {
                const field = try self.resolveStateMember(member);
                break :blk AssignTarget{ .ty = field, .mutable = true };
            },
            .index => |index_expr| blk: {
                const table_info = try self.resolveTableTarget(index_expr);
                break :blk AssignTarget{ .ty = table_info.ty, .mutable = true };
            },
            else => error.ImmutableBinding,
        };
    }

    fn checkCall(self: *FunctionChecker, call: ast.CallExpr, scope: *Scope) SemanticError!Type {
        const callee_expr = call.callee;
        const callee_type = switch (callee_expr.*) {
            .identifier => |ident| ident.name,
            else => return error.InvalidCallTarget,
        };

        const fn_info = self.contract.functions.get(callee_type) orelse return error.UnknownFunction;
        if (call.arguments.len != fn_info.params.len) return error.ArgumentCountMismatch;

        for (call.arguments, fn_info.params) |arg_expr, param| {
            const arg_type = try self.checkExpr(arg_expr, scope);
            if (!typesCompatible(param.ty, arg_type)) return error.TypeMismatch;
        }

        return fn_info.return_type;
    }

    fn checkBinary(self: *FunctionChecker, binary: ast.BinaryExpr, scope: *Scope) SemanticError!Type {
        const left = try self.checkExpr(binary.left, scope);
        const right = try self.checkExpr(binary.right, scope);

        return switch (binary.op) {
            .plus, .minus, .star, .slash, .percent => blk: {
                const combined = combineNumeric(left, right) orelse return error.TypeMismatch;
                break :blk combined;
            },
            .less, .less_equal, .greater, .greater_equal => blk: {
                _ = combineNumeric(left, right) orelse return error.TypeMismatch;
                break :blk Type.bool;
            },
            .equal_equal, .bang_equal => blk: {
                if (!typesCompatible(left, right) or !typesCompatible(right, left)) return error.TypeMismatch;
                break :blk Type.bool;
            },
            .and_and, .or_or => blk: {
                if (!left.isBoolean() or !right.isBoolean()) return error.TypeMismatch;
                break :blk Type.bool;
            },
            else => return error.TypeMismatch,
        };
    }

    fn checkUnary(self: *FunctionChecker, unary: ast.UnaryExpr, scope: *Scope) SemanticError!Type {
        const operand_type = try self.checkExpr(unary.operand, scope);
        return switch (unary.op) {
            .bang => if (operand_type.isBoolean()) Type.bool else error.TypeMismatch,
            .minus => if (operand_type.isNumeric()) operand_type else error.TypeMismatch,
            else => operand_type,
        };
    }

    fn checkMember(self: *FunctionChecker, member: ast.MemberExpr, _: *Scope) SemanticError!Type {
        if (member.base.* == .identifier and std.mem.eql(u8, member.base.identifier.name, "state")) {
            if (self.contract.states.get(member.name)) |info| {
                return info.ty;
            }
            if (self.contract.tables.get(member.name)) |table_info| {
                return table_info.ty;
            }
            return error.UnknownStateField;
        }
        return error.UnsupportedMemberAccess;
    }

    fn checkIndex(self: *FunctionChecker, index: ast.IndexExpr, scope: *Scope) SemanticError!Type {
        const table_info = try self.resolveTableTarget(index);
        // Ensure index expression is semantically valid even if we cannot type-match keys yet.
        _ = try self.checkExpr(index.index, scope);
        return table_info.ty;
    }

    fn resolveStateMember(self: *FunctionChecker, member: ast.MemberExpr) SemanticError!Type {
        if (member.base.* == .identifier and std.mem.eql(u8, member.base.identifier.name, "state")) {
            if (self.contract.states.get(member.name)) |info| {
                return info.ty;
            }
            if (self.contract.tables.get(member.name)) |table_info| {
                return table_info.ty;
            }
            return error.UnknownStateField;
        }
        return error.UnsupportedMemberAccess;
    }

    fn resolveTableTarget(self: *FunctionChecker, index_expr: ast.IndexExpr) SemanticError!TableInfo {
        if (index_expr.target.* != .member) return error.UnsupportedMemberAccess;
        const member = index_expr.target.member;
        if (member.base.* != .identifier or !std.mem.eql(u8, member.base.identifier.name, "state")) {
            return error.UnsupportedMemberAccess;
        }
        if (self.contract.tables.get(member.name)) |info| {
            return info;
        }
        if (self.contract.states.contains(member.name)) return error.TypeMismatch;
        return error.UnknownStateField;
    }

    fn isCompileTimeConstant(expr: *ast.Expr) bool {
        return switch (expr.*) {
            .integer_literal, .bool_literal, .string_literal => true,
            else => false,
        };
    }
};

fn parseBits(slice: []const u8) ?u16 {
    var value: u16 = 0;
    for (slice) |c| {
        if (c < '0' or c > '9') return null;
        value = value * 10 + @as(u16, c - '0');
    }
    return value;
}

fn literalType(value: i128) Type {
    if (value < 0) {
        return Type{ .integer = .{ .signed = true, .bits = 128 } };
    }
    const magnitude = @as(u128, @intCast(value));
    return Type{ .integer = .{ .signed = false, .bits = bitWidthUnsigned(magnitude) } };
}

fn bitWidthUnsigned(value: u128) u16 {
    if (value == 0) return 1;
    var v = value;
    var bits: u16 = 0;
    while (v != 0) : (v >>= 1) {
        bits += 1;
    }
    return bits;
}

pub fn analyze(allocator: std.mem.Allocator, tree: *ast.Tree, metadata_out: ?*Metadata) SemanticError!void {
    var builder_storage: MetadataBuilder = undefined;
    var builder_ptr: ?*MetadataBuilder = null;
    if (metadata_out != null) {
        builder_storage = MetadataBuilder.init(allocator);
        builder_ptr = &builder_storage;
    }

    var analyzer = Analyzer{ .allocator = allocator, .tree = tree, .metadata_builder = builder_ptr };
    errdefer if (builder_ptr) |builder| builder.deinit();

    try analyzer.run();

    if (builder_ptr) |builder| {
        const metadata = builder.finish() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
        };
        metadata_out.?.* = metadata;
        builder.deinit();
    }
}

pub fn describeError(err: SemanticError) []const u8 {
    return switch (err) {
        error.DuplicateState => "duplicate state declaration",
        error.DuplicateFunction => "duplicate function declaration",
        error.DuplicateTable => "duplicate table declaration",
        error.DuplicateEvent => "duplicate event declaration",
        error.DuplicateSymbol => "duplicate symbol in scope",
        error.DuplicateParameter => "duplicate parameter name",
        error.UnknownIdentifier => "unknown identifier",
        error.UnknownFunction => "unknown function",
        error.UnknownStateField => "unknown state or table member",
        error.ImmutableBinding => "assignment to immutable binding",
        error.NonConstantInitializer => "const binding requires literal initializer",
        error.TypeMismatch => "type mismatch",
        error.ArgumentCountMismatch => "argument count mismatch",
        error.ReturnTypeMismatch => "return type mismatch",
        error.MissingReturnValue => "missing return value",
        error.UnexpectedReturnValue => "unexpected return value",
        error.UnsupportedMemberAccess => "unsupported member access",
        error.UnsupportedLoop => "while loops are not supported yet",
        error.InvalidCallTarget => "invalid call target",
        error.ReservedIdentifier => "reserved identifier",
        error.VoidValueNotAllowed => "void value not allowed",
        error.OutOfMemory => "out of memory",
    };
}

const testing = std.testing;

fn parseSource(source: []const u8) !ast.Tree {
    const parser = @import("parser.zig");
    return try parser.parseModule(testing.allocator, source);
}

test "analyze simple contract" {
    const source =
        "contract Treasury {\n" ++
        "    state balance: u64;\n" ++
        "    fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = total + amount;\n" ++
        "        return;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try analyze(testing.allocator, &tree, null);
}

test "detect duplicate state" {
    const source =
        "contract A {\n" ++
        "    state balance: u64;\n" ++
        "    state balance: u64;\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.DuplicateState, analyze(testing.allocator, &tree, null));
}

test "detect duplicate table" {
    const source =
        "contract A {\n" ++
        "    table balances: Map<Address, u64>;\n" ++
        "    table balances: Map<Address, u64>;\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.DuplicateTable, analyze(testing.allocator, &tree, null));
}

test "detect duplicate event" {
    const source =
        "contract A {\n" ++
        "    event Transfer(from: Address);\n" ++
        "    event Transfer(to: Address);\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.DuplicateEvent, analyze(testing.allocator, &tree, null));
}

test "immutable assignment error" {
    const source =
        "contract A {\n" ++
        "    fn run() {\n" ++
        "        let value = 1;\n" ++
        "        value = 2;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.ImmutableBinding, analyze(testing.allocator, &tree, null));
}

test "type mismatch error" {
    const source =
        "contract A {\n" ++
        "    state balance: u64;\n" ++
        "    fn run(amount: bool) {\n" ++
        "        state.balance = amount;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.TypeMismatch, analyze(testing.allocator, &tree, null));
}

test "return type mismatch" {
    const source =
        "contract A {\n" ++
        "    fn value() -> u64 {\n" ++
        "        return;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.MissingReturnValue, analyze(testing.allocator, &tree, null));
}

test "const binding requires literal initializer" {
    const source =
        "contract A {\n" ++
        "    fn run(amount: u64) {\n" ++
        "        let const limit = amount;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.NonConstantInitializer, analyze(testing.allocator, &tree, null));
}

test "const binding is immutable" {
    const source =
        "contract A {\n" ++
        "    fn run() {\n" ++
        "        let const limit = 5;\n" ++
        "        limit = 6;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.ImmutableBinding, analyze(testing.allocator, &tree, null));
}

test "const parameter is immutable" {
    const source =
        "contract A {\n" ++
        "    fn run(const amount: u64) {\n" ++
        "        amount = 1;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.ImmutableBinding, analyze(testing.allocator, &tree, null));
}

test "unknown identifier error" {
    const source =
        "contract A {\n" ++
        "    fn run() {\n" ++
        "        let value = missing;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.UnknownIdentifier, analyze(testing.allocator, &tree, null));
}

test "unknown function call" {
    const source =
        "contract A {\n" ++
        "    fn run() {\n" ++
        "        missing();\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.UnknownFunction, analyze(testing.allocator, &tree, null));
}

test "argument count mismatch" {
    const source =
        "contract A {\n" ++
        "    fn helper(value: u64) -> u64 {\n" ++
        "        return value;\n" ++
        "    }\n" ++
        "    fn run() {\n" ++
        "        helper();\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.ArgumentCountMismatch, analyze(testing.allocator, &tree, null));
}

test "unknown state field" {
    const source =
        "contract A {\n" ++
        "    state balance: u64;\n" ++
        "    fn run() {\n" ++
        "        state.missing = 1;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parseSource(source);
    defer tree.deinit();
    try testing.expectError(SemanticError.UnknownStateField, analyze(testing.allocator, &tree, null));
}
