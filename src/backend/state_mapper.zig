const std = @import("std");

pub const StateMapperError = error{
    DuplicateState,
    OutOfMemory,
};

pub const ResourceKind = enum {
    state,
    table,
};

const Entry = struct {
    slot: u64,
    kind: ResourceKind,
};

pub const StateMapper = struct {
    allocator: std.mem.Allocator,
    slots: std.StringHashMapUnmanaged(Entry) = .{},
    next_slot: u64 = 0,

    pub fn init(allocator: std.mem.Allocator) StateMapper {
        return StateMapper{
            .allocator = allocator,
            .slots = .{},
            .next_slot = 0,
        };
    }

    pub fn deinit(self: *StateMapper) void {
        self.slots.deinit(self.allocator);
    }

    pub fn assignState(self: *StateMapper, name: []const u8) StateMapperError!u64 {
        return self.assignResource(name, .state);
    }

    pub fn assignTable(self: *StateMapper, name: []const u8) StateMapperError!u64 {
        return self.assignResource(name, .table);
    }

    fn assignResource(self: *StateMapper, name: []const u8, kind: ResourceKind) StateMapperError!u64 {
        if (self.slots.contains(name)) return StateMapperError.DuplicateState;
        const slot = self.next_slot;
        self.slots.put(self.allocator, name, Entry{ .slot = slot, .kind = kind }) catch |err| switch (err) {
            error.OutOfMemory => return StateMapperError.OutOfMemory,
        };
        self.next_slot += 1;
        return slot;
    }

    pub fn getStateSlot(self: *StateMapper, name: []const u8) ?u64 {
        if (self.slots.get(name)) |entry| {
            return if (entry.kind == .state) entry.slot else null;
        }
        return null;
    }

    pub fn getTableSlot(self: *StateMapper, name: []const u8) ?u64 {
        if (self.slots.get(name)) |entry| {
            return if (entry.kind == .table) entry.slot else null;
        }
        return null;
    }
};

const testing = std.testing;

test "state mapper assigns sequential slots" {
    var mapper = StateMapper.init(testing.allocator);
    defer mapper.deinit();

    try testing.expectEqual(@as(u64, 0), try mapper.assignState("total"));
    try testing.expectEqual(@as(u64, 1), try mapper.assignTable("balances"));
    try testing.expectEqual(@as(?u64, 1), mapper.getTableSlot("balances"));
}

test "state mapper detects duplicates" {
    var mapper = StateMapper.init(testing.allocator);
    defer mapper.deinit();

    try testing.expectEqual(@as(u64, 0), try mapper.assignState("owner"));
    try testing.expectError(StateMapperError.DuplicateState, mapper.assignState("owner"));
}
