const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const races = try common.parse(data);
    defer races.deinit();

    var prod: u32 = 1;

    for (races.times.items, 0..) |race_time, i| {
        const race_record = races.distances.items[i];

        var min: u32 = undefined;

        for (1..race_time) |speed| {
            const rem_race_time = race_time - speed;
            const dist = rem_race_time * speed;
            if (dist > race_record) {
                min = @intCast(speed);
                break;
            }
        }

        var max: u32 = undefined;

        var speed: u32 = race_time - 1;
        while (speed > 0) {
            const rem_race_time = race_time - speed;
            const dist = rem_race_time * speed;

            if (dist > race_record) {
                max = speed;
                break;
            }

            speed -= 1;
        }

        const nums_beat = max - min + 1;

        prod *= nums_beat;
    }

    try stdout.print("{d}\n", .{prod});
}
