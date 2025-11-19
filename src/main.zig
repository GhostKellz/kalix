const std = @import("std");
const kalix = @import("kalix");

const CliError = error{InvalidArguments};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const raw_args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, raw_args);

    runWithArgs(allocator, raw_args) catch |err| switch (err) {
        CliError.InvalidArguments => {
            std.log.err("usage: kalix <input.kalix> [output_dir]", .{});
            return;
        },
        else => return err,
    };
}

fn runWithArgs(allocator: std.mem.Allocator, raw_args: []const [:0]const u8) !void {
    var args = try allocator.alloc([]const u8, raw_args.len);
    defer allocator.free(args);
    for (raw_args, 0..) |arg, idx| {
        args[idx] = std.mem.sliceTo(arg, 0);
    }
    try runCli(allocator, args);
}

fn runCli(allocator: std.mem.Allocator, args: []const []const u8) !void {
    if (args.len < 2 or args.len > 3) return CliError.InvalidArguments;

    const input_path = args[1];
    const output_dir = if (args.len == 3) args[2] else null;

    if (output_dir) |dir_path| {
        try std.fs.cwd().makePath(dir_path);
    }

    const source = try std.fs.cwd().readFileAlloc(input_path, allocator, @enumFromInt(std.math.maxInt(usize)));
    defer allocator.free(source);

    var tree = try kalix.parseModule(allocator, source);
    defer tree.deinit();

    const module = tree.getModule();
    var artifacts = try kalix.backend.artifact_builder.buildModule(allocator, module);
    defer {
        for (artifacts) |*artifact| artifact.deinit(allocator);
        allocator.free(artifacts);
    }

    if (artifacts.len == 0) {
        std.log.warn("no contracts found in input", .{});
        return;
    }

    for (artifacts) |*artifact| {
        const file_name = try std.fmt.allocPrint(allocator, "{s}.zvmc", .{artifact.name});
        defer allocator.free(file_name);

        const destination = try computeOutputPath(allocator, file_name, output_dir, input_path);
        defer allocator.free(destination);

        const serialized = try kalix.backend.artifact_builder.serializeArtifact(allocator, artifact);
        defer allocator.free(serialized);

        var file = try std.fs.cwd().createFile(destination, .{ .truncate = true });
        defer file.close();
        try file.writeAll(serialized);

        const gas_report = artifact.gas;
        std.log.info(
            "wrote {s} (gas total: {d}, state L{d}/S{d}, table L{d}/S{d})",
            .{
                destination,
                gas_report.total,
                gas_report.storage.state_loads,
                gas_report.storage.state_stores,
                gas_report.storage.table_loads,
                gas_report.storage.table_stores,
            },
        );

        for (gas_report.functions) |fn_report| {
            std.log.info(
                "  fn {s}: gas {d}, state L{d}/S{d}, table L{d}/S{d}",
                .{
                    fn_report.name,
                    fn_report.gas,
                    fn_report.storage.state_loads,
                    fn_report.storage.state_stores,
                    fn_report.storage.table_loads,
                    fn_report.storage.table_stores,
                },
            );
        }
    }
}

fn computeOutputPath(
    allocator: std.mem.Allocator,
    file_name: []const u8,
    output_dir: ?[]const u8,
    input_path: []const u8,
) ![]u8 {
    if (output_dir) |dir_path| {
        return try std.fs.path.join(allocator, &.{ dir_path, file_name });
    }

    const parent = try parentDirectory(allocator, input_path);
    defer if (parent) |dir| allocator.free(dir);

    if (parent) |dir| {
        return try std.fs.path.join(allocator, &.{ dir, file_name });
    }

    return try allocator.dupe(u8, file_name);
}

fn parentDirectory(allocator: std.mem.Allocator, path: []const u8) !?[]u8 {
    const sep = std.fs.path.sep;
    if (std.mem.lastIndexOfScalar(u8, path, sep)) |idx| {
        const slice = if (idx == 0)
            path[0..1]
        else
            path[0..idx];
        if (slice.len == 0) return null;
        return try allocator.dupe(u8, slice);
    }

    // Windows root drive handling (e.g., C:\file)
    if (std.mem.indexOfScalar(u8, path, ':')) |colon_idx| {
        if (colon_idx + 1 < path.len and path[colon_idx + 1] == '\\') {
            return try allocator.dupe(u8, path[0 .. colon_idx + 2]);
        }
    }

    return null;
}

const testing = std.testing;

test "cli writes artifact next to contract" {
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    const tmp_path = try tmp.dir.realpathAlloc(testing.allocator, ".");
    defer testing.allocator.free(tmp_path);
    const file_path = try std.fs.path.join(testing.allocator, &.{ tmp_path, "Treasury.kalix" });
    defer testing.allocator.free(file_path);

    {
        var file = try tmp.dir.createFile("Treasury.kalix", .{ .truncate = true });
        defer file.close();
        try file.writeAll(source);
    }

    const args = [_][]const u8{ "kalix", file_path };
    try runCli(testing.allocator, &args);

    const output_file = try tmp.dir.openFile("Treasury.zvmc", .{});
    defer output_file.close();
    const size = try output_file.getEndPos();
    try testing.expect(size > 0);
}
