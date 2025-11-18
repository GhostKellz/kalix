const std = @import("std");
const kalix = @import("kalix");
const backend = kalix.backend;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const rendered = try backend.lowering.renderControlFlowFixture(allocator);
    defer allocator.free(rendered);

    const fixture_path = "src/backend/testdata/lowering/control_flow.json";
    try std.fs.cwd().makePath(std.fs.path.dirname(fixture_path) orelse ".");
    var file = try std.fs.cwd().createFile(fixture_path, .{ .truncate = true });
    defer file.close();
    try file.writeAll(rendered);

    std.log.info("updated {s}", .{fixture_path});
}
