const std = @import("std");
const kalix = @import("kalix");

const Io = std.Io;
const Dir = Io.Dir;

const CliError = error{InvalidArguments};

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    const raw_args = try init.minimal.args.toSlice(init.arena.allocator());

    runWithArgs(allocator, io, raw_args) catch |err| switch (err) {
        CliError.InvalidArguments => {
            std.log.err("usage: kalix <input.kalix> [output_dir]", .{});
            return;
        },
        else => return err,
    };
}

fn runWithArgs(allocator: std.mem.Allocator, io: Io, raw_args: []const [:0]const u8) !void {
    var args = try allocator.alloc([]const u8, raw_args.len);
    defer allocator.free(args);
    for (raw_args, 0..) |arg, idx| {
        args[idx] = std.mem.sliceTo(arg, 0);
    }
    try runCli(allocator, io, args);
}

fn runCli(allocator: std.mem.Allocator, io: Io, args: []const []const u8) !void {
    if (args.len < 2 or args.len > 3) return CliError.InvalidArguments;

    const input_path = args[1];
    const output_dir = if (args.len == 3) args[2] else null;

    const cwd = Dir.cwd();

    if (output_dir) |dir_path| {
        try cwd.createDirPath(io, dir_path);
    }

    const source = try cwd.readFileAlloc(io, input_path, allocator, .unlimited);
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

        var file = try cwd.createFile(io, destination, .{ .truncate = true });
        defer file.close(io);
        try file.writeStreamingAll(io, serialized);

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
        return try Dir.path.join(allocator, &.{ dir_path, file_name });
    }

    const parent = try parentDirectory(allocator, input_path);
    defer if (parent) |dir| allocator.free(dir);

    if (parent) |dir| {
        return try Dir.path.join(allocator, &.{ dir, file_name });
    }

    return try allocator.dupe(u8, file_name);
}

fn parentDirectory(allocator: std.mem.Allocator, path: []const u8) !?[]u8 {
    const sep = Dir.path.sep;
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

    // Construct path relative to cwd: .zig-cache/tmp/{sub_path}/Treasury.kalix
    const file_path = try Dir.path.join(testing.allocator, &.{ ".zig-cache", "tmp", &tmp.sub_path, "Treasury.kalix" });
    defer testing.allocator.free(file_path);

    {
        var file = try tmp.dir.createFile(testing.io, "Treasury.kalix", .{ .truncate = true });
        defer file.close(testing.io);
        try file.writeStreamingAll(testing.io, source);
    }

    const args = [_][]const u8{ "kalix", file_path };
    try runCli(testing.allocator, testing.io, &args);

    const output_file = try tmp.dir.openFile(testing.io, "Treasury.zvmc", .{});
    defer output_file.close(testing.io);
    const file_stat = try output_file.stat(testing.io);
    try testing.expect(file_stat.size > 0);
}
