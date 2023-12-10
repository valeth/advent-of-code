const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const races = try common.parse(data);
    defer races.deinit();

    var prod: u64 = 1;

    for (races.times.items, 0..) |race_time, i| {
        const race_record = races.distances.items[i];

        const nums_beat = common.calculate(race_time, race_record);

        prod *= nums_beat;
    }

    try stdout.print("{d}\n", .{prod});
}
