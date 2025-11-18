const std = @import("std");
const abi_builder = @import("src/backend/abi_builder.zig");
const parser = @import("src/frontend/parser.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const source =
        "contract Treasury {\n" ++
        "    pub state balance: u64;\n" ++
        "    pub fn deposit(amount: u64) {\n" ++
        "        let mut total = amount;\n" ++
        "        state.balance = state.balance + total;\n" ++
        "    }\n" ++
        "}\n";

    var tree = try parser.parseModule(allocator, source);
    defer tree.deinit();

    const module = tree.getModule();
    const contract = module.contracts[0];

    var builder = abi_builder.AbiBuilder.init(allocator);
    const abi_json = try builder.buildContract(contract);
    defer allocator.free(abi_json);

    try std.io.getStdOut().writer().writeAll(abi_json);
}
