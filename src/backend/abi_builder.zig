const std = @import("std");
const crypto = std.crypto;
const ast = @import("../frontend/ast.zig");
const parser = @import("../frontend/parser.zig");
const state_mapper = @import("state_mapper.zig");
const zvm = @import("zvm");

pub const AbiBuilderError = error{
    OutOfMemory,
    DuplicateState,
    DuplicateTable,
};

const StateMapper = state_mapper.StateMapper;
const StateMapperError = state_mapper.StateMapperError;

const Visibility = struct {
    pub const public = "public";
    pub const private = "private";
};

const Mutability = struct {
    pub const view = "view";
    pub const payable = "payable";
    pub const nonpayable = "nonpayable";
};

const JsonParam = struct {
    name: []const u8,
    type: []const u8,
};

const JsonEventField = struct {
    name: []const u8,
    type: []const u8,
    indexed: bool,
};

const JsonFunction = struct {
    name: []const u8,
    selector: []const u8,
    visibility: []const u8,
    mutability: []const u8,
    inputs: []const JsonParam,
    outputs: []const JsonParam,
};

const JsonEvent = struct {
    name: []const u8,
    signature: []const u8,
    topic: []const u8,
    visibility: []const u8,
    fields: []const JsonEventField,
};

const JsonStateVar = struct {
    name: []const u8,
    slot: u64,
    type: []const u8,
    visibility: []const u8,
};

const JsonTable = struct {
    name: []const u8,
    slot: u64,
    key_type: []const u8,
    value_type: []const u8,
    visibility: []const u8,
};

const JsonContractAbi = struct {
    contract: []const u8,
    functions: []const JsonFunction,
    events: []const JsonEvent,
    state_variables: []const JsonStateVar,
    tables: []const JsonTable,
};

pub const AbiBuilder = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AbiBuilder {
        return .{ .allocator = allocator };
    }

    pub fn buildContract(self: *AbiBuilder, contract: ast.Contract) AbiBuilderError![]u8 {
        var mapper = StateMapper.init(self.allocator);
        defer mapper.deinit();

        var state_entries = std.ArrayListUnmanaged(JsonStateVar){};
        defer state_entries.deinit(self.allocator);
        defer {
            for (state_entries.items) |entry| {
                self.allocator.free(@constCast(entry.type));
            }
        }

        var table_entries = std.ArrayListUnmanaged(JsonTable){};
        defer table_entries.deinit(self.allocator);
        defer {
            for (table_entries.items) |entry| {
                self.allocator.free(@constCast(entry.key_type));
                self.allocator.free(@constCast(entry.value_type));
            }
        }

        // First pass: register state and tables to guarantee slot ordering.
        for (contract.items) |item| {
            switch (item) {
                .state => |state_decl| {
                    _ = try self.assignState(&mapper, &state_entries, state_decl);
                },
                .table => |table_decl| {
                    _ = try self.assignTable(&mapper, &table_entries, table_decl);
                },
                else => {},
            }
        }

        var functions = std.ArrayListUnmanaged(JsonFunction){};
        defer functions.deinit(self.allocator);
        defer {
            for (functions.items) |fn_entry| {
                self.allocator.free(@constCast(fn_entry.selector));
                freeParamsSlice(self.allocator, fn_entry.inputs);
                freeParamsSlice(self.allocator, fn_entry.outputs);
            }
        }

        var events = std.ArrayListUnmanaged(JsonEvent){};
        defer events.deinit(self.allocator);
        defer {
            for (events.items) |event_entry| {
                self.allocator.free(@constCast(event_entry.signature));
                self.allocator.free(@constCast(event_entry.topic));
                freeEventFieldsSlice(self.allocator, event_entry.fields);
            }
        }

        // Second pass: collect callable interface and events.
        for (contract.items) |item| {
            switch (item) {
                .function => |fn_decl| {
                    if (!fn_decl.qualifiers.is_public) continue;
                    const entry = try self.buildFunctionEntry(fn_decl);
                    try functions.append(self.allocator, entry);
                },
                .event => |event_decl| {
                    if (!event_decl.is_public) continue;
                    const entry = try self.buildEventEntry(event_decl);
                    try events.append(self.allocator, entry);
                },
                else => {},
            }
        }

        const abi = JsonContractAbi{
            .contract = contract.name,
            .functions = functions.items,
            .events = events.items,
            .state_variables = state_entries.items,
            .tables = table_entries.items,
        };

        var writer = std.Io.Writer.Allocating.init(self.allocator);
        defer writer.deinit();

        var stringify = std.json.Stringify{
            .writer = &writer.writer,
            .options = .{ .whitespace = .indent_2 },
        };

        stringify.write(abi) catch return AbiBuilderError.OutOfMemory;

        const abi_json = writer.toOwnedSlice() catch return AbiBuilderError.OutOfMemory;
        return abi_json;
    }

    fn assignState(
        self: *AbiBuilder,
        mapper: *StateMapper,
        state_entries: *std.ArrayListUnmanaged(JsonStateVar),
        state_decl: ast.StateDecl,
    ) AbiBuilderError!u64 {
        const slot = mapper.assignState(state_decl.name) catch |err| switch (err) {
            StateMapperError.DuplicateState => return AbiBuilderError.DuplicateState,
            StateMapperError.OutOfMemory => return AbiBuilderError.OutOfMemory,
        };

        const type_str = try self.formatType(state_decl.ty);
        try state_entries.append(self.allocator, .{
            .name = state_decl.name,
            .slot = slot,
            .type = type_str,
            .visibility = if (state_decl.is_public) Visibility.public else Visibility.private,
        });

        return slot;
    }

    fn assignTable(
        self: *AbiBuilder,
        mapper: *StateMapper,
        table_entries: *std.ArrayListUnmanaged(JsonTable),
        table_decl: ast.TableDecl,
    ) AbiBuilderError!u64 {
        const slot = mapper.assignTable(table_decl.name) catch |err| switch (err) {
            StateMapperError.DuplicateState => return AbiBuilderError.DuplicateTable,
            StateMapperError.OutOfMemory => return AbiBuilderError.OutOfMemory,
        };

        const generics = table_decl.ty.generics;
        const key_type = if (generics.len >= 1) generics[0] else ast.initType("void");
        const value_type = if (generics.len >= 2) generics[1] else ast.initType("void");
        const key_str = try self.formatType(key_type);
        const value_str = try self.formatType(value_type);
        try table_entries.append(self.allocator, .{
            .name = table_decl.name,
            .slot = slot,
            .key_type = key_str,
            .value_type = value_str,
            .visibility = if (table_decl.is_public) Visibility.public else Visibility.private,
        });

        return slot;
    }

    fn buildFunctionEntry(self: *AbiBuilder, fn_decl: ast.FnDecl) AbiBuilderError!JsonFunction {
        const signature = try makeSignature(self.allocator, fn_decl);
        defer self.allocator.free(signature);

        const selector = zvm.KalixLoader.computeSelector(signature);
        const selector_value = std.mem.readInt(u32, &selector, .big);
        const selector_str = try std.fmt.allocPrint(self.allocator, "0x{X:0>8}", .{selector_value});

        const inputs = try self.buildParams(fn_decl.params);
        const outputs = try self.buildReturn(fn_decl.return_type);

        return JsonFunction{
            .name = fn_decl.name,
            .selector = selector_str,
            .visibility = if (fn_decl.qualifiers.is_public) Visibility.public else Visibility.private,
            .mutability = functionMutability(fn_decl.qualifiers),
            .inputs = inputs,
            .outputs = outputs,
        };
    }

    fn buildEventEntry(self: *AbiBuilder, event_decl: ast.EventDecl) AbiBuilderError!JsonEvent {
        const signature = try makeEventSignature(self.allocator, event_decl);
        errdefer self.allocator.free(signature);
        const topic = self.computeEventTopic(signature) catch |err| {
            self.allocator.free(signature);
            return err;
        };
        errdefer self.allocator.free(topic);
        const fields = try self.buildEventFields(event_decl.params);
        return JsonEvent{
            .name = event_decl.name,
            .signature = signature,
            .topic = topic,
            .visibility = if (event_decl.is_public) Visibility.public else Visibility.private,
            .fields = fields,
        };
    }

    fn buildParams(self: *AbiBuilder, params: []const ast.Param) AbiBuilderError![]const JsonParam {
        if (params.len == 0) return emptyParams();

        const entries = try self.allocator.alloc(JsonParam, params.len);
        for (params, 0..) |param, idx| {
            const type_str = try self.formatType(param.ty);
            entries[idx] = JsonParam{
                .name = param.name,
                .type = type_str,
            };
        }
        return entries;
    }

    fn buildEventFields(self: *AbiBuilder, params: []const ast.Param) AbiBuilderError![]const JsonEventField {
        if (params.len == 0) return emptyEventFields();

        const fields = try self.allocator.alloc(JsonEventField, params.len);
        for (params, 0..) |param, idx| {
            const type_str = try self.formatType(param.ty);
            fields[idx] = JsonEventField{
                .name = param.name,
                .type = type_str,
                .indexed = param.is_const,
            };
        }
        return fields;
    }

    fn computeEventTopic(self: *AbiBuilder, signature: []const u8) AbiBuilderError![]u8 {
        var hash: [32]u8 = undefined;
        crypto.hash.sha3.Keccak256.hash(signature, &hash, .{});
        return allocPrefixedHexLower(self.allocator, &hash) catch
            AbiBuilderError.OutOfMemory;
    }

    fn buildReturn(self: *AbiBuilder, return_type: ?ast.TypeExpr) AbiBuilderError![]const JsonParam {
        if (return_type) |ret_ty| {
            const entries = try self.allocator.alloc(JsonParam, 1);
            entries[0] = .{
                .name = "",
                .type = try self.formatType(ret_ty),
            };
            return entries;
        }
        return emptyParams();
    }

    fn formatType(self: *AbiBuilder, ty: ast.TypeExpr) AbiBuilderError![]u8 {
        var buffer = std.ArrayListUnmanaged(u8){};
        defer buffer.deinit(self.allocator);

        try writeType(&buffer, self.allocator, ty);

        return buffer.toOwnedSlice(self.allocator);
    }
};

pub fn makeSignature(allocator: std.mem.Allocator, fn_decl: ast.FnDecl) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);

    try buffer.appendSlice(allocator, fn_decl.name);
    try buffer.append(allocator, '(');
    for (fn_decl.params, 0..) |param, idx| {
        if (idx != 0) try buffer.append(allocator, ',');
        try writeType(&buffer, allocator, param.ty);
    }
    try buffer.append(allocator, ')');

    return buffer.toOwnedSlice(allocator);
}

fn makeEventSignature(allocator: std.mem.Allocator, event_decl: ast.EventDecl) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);

    try buffer.appendSlice(allocator, event_decl.name);
    try buffer.append(allocator, '(');
    for (event_decl.params, 0..) |param, idx| {
        if (idx != 0) try buffer.append(allocator, ',');
        try writeType(&buffer, allocator, param.ty);
    }
    try buffer.append(allocator, ')');

    return buffer.toOwnedSlice(allocator);
}

fn functionMutability(qualifiers: ast.Qualifiers) []const u8 {
    if (qualifiers.view) return Mutability.view;
    if (qualifiers.payable) return Mutability.payable;
    return Mutability.nonpayable;
}

fn writeType(buffer: *std.ArrayListUnmanaged(u8), allocator: std.mem.Allocator, ty: ast.TypeExpr) !void {
    try buffer.appendSlice(allocator, ty.name);
    if (ty.generics.len == 0) return;
    try buffer.append(allocator, '<');
    for (ty.generics, 0..) |generic, idx| {
        if (idx != 0) try buffer.append(allocator, ',');
        try writeType(buffer, allocator, generic);
    }
    try buffer.append(allocator, '>');
}

fn emptyParams() []const JsonParam {
    const empty = [_]JsonParam{};
    return empty[0..];
}

fn emptyEventFields() []const JsonEventField {
    const empty = [_]JsonEventField{};
    return empty[0..];
}

fn freeParamsSlice(allocator: std.mem.Allocator, params: []const JsonParam) void {
    if (params.len == 0) return;
    const mutable_params = @constCast(params);
    for (mutable_params) |param| {
        allocator.free(@constCast(param.type));
    }
    allocator.free(mutable_params);
}

fn freeEventFieldsSlice(allocator: std.mem.Allocator, fields: []const JsonEventField) void {
    if (fields.len == 0) return;
    const mutable_fields = @constCast(fields);
    for (mutable_fields) |field| {
        allocator.free(@constCast(field.type));
    }
    allocator.free(mutable_fields);
}

fn allocPrefixedHexLower(allocator: std.mem.Allocator, bytes: []const u8) std.mem.Allocator.Error![]u8 {
    const hex_chars = "0123456789abcdef";
    const total_len = bytes.len * 2 + 2;
    var out = try allocator.alloc(u8, total_len);
    out[0] = '0';
    out[1] = 'x';
    var idx: usize = 0;
    for (bytes) |byte| {
        out[2 + idx] = hex_chars[byte >> 4];
        out[3 + idx] = hex_chars[byte & 0x0F];
        idx += 2;
    }
    return out;
}

const testing = std.testing;

fn buildAbiForSource(source: []const u8) ![]u8 {
    var tree = try parser.parseModule(testing.allocator, source);
    defer tree.deinit();

    const module = tree.getModule();
    const contract = module.contracts[0];

    var builder = AbiBuilder.init(testing.allocator);
    return try builder.buildContract(contract);
}

test "abi builder emits contract metadata" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const abi_json = try buildAbiForSource(source);
    defer testing.allocator.free(abi_json);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, abi_json, .{});
    defer parsed.deinit();

    const root = parsed.value;
    const obj = root.object;

    try testing.expect(obj.contains("contract"));
    try testing.expect(std.mem.eql(u8, obj.get("contract").?.string, "Treasury"));

    const functions = obj.get("functions").?.array.items;
    try testing.expectEqual(@as(usize, 1), functions.len);

    const selector_value = zvm.KalixLoader.computeSelector("deposit(u64)");
    const selector_hex = try std.fmt.allocPrint(testing.allocator, "0x{X:0>8}", .{std.mem.readInt(u32, &selector_value, .big)});
    defer testing.allocator.free(selector_hex);

    const func_obj = functions[0].object;
    try testing.expect(std.mem.eql(u8, func_obj.get("selector").?.string, selector_hex));
    try testing.expect(std.mem.eql(u8, func_obj.get("mutability").?.string, Mutability.nonpayable));

    const state = obj.get("state_variables").?.array.items;
    try testing.expectEqual(@as(usize, 1), state.len);
    const state_obj = state[0].object;
    try testing.expectEqual(@as(u64, 0), @intFromFloat(state_obj.get("slot").?.float));
    try testing.expect(std.mem.eql(u8, state_obj.get("type").?.string, "u64"));

    const tables = obj.get("tables").?.array.items;
    try testing.expectEqual(@as(usize, 0), tables.len);

    const events = obj.get("events").?.array.items;
    try testing.expectEqual(@as(usize, 0), events.len);
}

test "abi builder encodes event signatures and indexing" {
    const source =
        "contract Logs {\n" ++
        "    pub event ValueChanged(const previous: u64, next: u64);\n" ++
        "}\n";

    const abi_json = try buildAbiForSource(source);
    defer testing.allocator.free(abi_json);

    var parsed = try std.json.parseFromSlice(std.json.Value, testing.allocator, abi_json, .{});
    defer parsed.deinit();

    const obj = parsed.value.object;
    const events = obj.get("events").?.array.items;
    try testing.expectEqual(@as(usize, 1), events.len);

    const event_obj = events[0].object;
    try testing.expect(std.mem.eql(u8, event_obj.get("name").?.string, "ValueChanged"));
    try testing.expect(std.mem.eql(u8, event_obj.get("signature").?.string, "ValueChanged(u64,u64)"));

    var hash: [32]u8 = undefined;
    crypto.hash.sha3.Keccak256.hash("ValueChanged(u64,u64)", &hash, .{});
    const expected_topic = try allocPrefixedHexLower(testing.allocator, &hash);
    defer testing.allocator.free(expected_topic);

    try testing.expect(std.mem.eql(u8, event_obj.get("topic").?.string, expected_topic));

    const fields = event_obj.get("fields").?.array.items;
    try testing.expectEqual(@as(usize, 2), fields.len);
    const prev_field = fields[0].object;
    const next_field = fields[1].object;
    try testing.expect(prev_field.get("indexed").?.bool);
    try testing.expect(!next_field.get("indexed").?.bool);
    try testing.expect(std.mem.eql(u8, prev_field.get("type").?.string, "u64"));
    try testing.expect(std.mem.eql(u8, next_field.get("type").?.string, "u64"));
}

test "abi builder matches golden fixture" {
    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const abi_json = try buildAbiForSource(source);
    defer testing.allocator.free(abi_json);

    const fixture = try std.fs.cwd().readFileAlloc(testing.allocator, "src/backend/testdata/abi/treasury.json", @enumFromInt(std.math.maxInt(usize)));
    defer testing.allocator.free(fixture);

    const trimmed_actual = std.mem.trim(u8, abi_json, " \n\r\t");
    const trimmed_expected = std.mem.trim(u8, fixture, " \n\r\t");
    try testing.expect(std.mem.eql(u8, trimmed_expected, trimmed_actual));
}
