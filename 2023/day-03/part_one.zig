const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const schematic = try common.parse(data);

    var sum: u32 = 0;

    for (schematic.numbers.items) |row| {
        for (row.items) |num| {
            if (try num.has_adjacent_symbols(&schematic)) {
                sum += num.val;
            }
        }
    }

    try stdout.print("{d}\n", .{sum});
}
