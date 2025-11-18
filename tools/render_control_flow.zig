const std = @import("std");
const kalix = @import("kalix");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const rendered = try kalix.backend.lowering.renderControlFlowFixture(allocator);
    defer allocator.free(rendered);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{s}\n", .{rendered});
}
