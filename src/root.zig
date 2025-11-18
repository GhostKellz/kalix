//! By convention, root.zig is the root source file when making a library.
const std = @import("std");
pub const frontend = @import("frontend/mod.zig");
pub const backend = @import("backend/mod.zig");

pub fn bufferedPrint() !void {
    // Stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.flush(); // Don't forget to flush!
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

pub fn parseModule(allocator: std.mem.Allocator, source: []const u8) !frontend.ast.Tree {
    return frontend.parser.parseModule(allocator, source);
}

pub fn analyzeTree(allocator: std.mem.Allocator, tree: *frontend.ast.Tree) !void {
    try frontend.semantics.analyze(allocator, tree);
}

test "basic add functionality" {
    try std.testing.expect(add(3, 7) == 10);
}

test "backend lowering control flow fixture" {
    try backend.lowering.verifyControlFlowFixture(std.testing.allocator);
}
