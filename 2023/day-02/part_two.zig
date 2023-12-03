const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const games = try common.parse(data);
    defer games.deinit();

    var sum: u32 = 0;

    for (games.items) |game| {
        var min_red: ?u32 = null;
        var min_green: ?u32 = null;
        var min_blue: ?u32 = null;

        for (game.sets.items) |set| {
            for (set.items) |item| {
                switch (item.color) {
                    .red => {
                        if (min_red == null or min_red.? < item.amount) {
                            min_red = item.amount;
                        }
                    },
                    .green => {
                        if (min_green == null or min_green.? < item.amount) {
                            min_green = item.amount;
                        }
                    },
                    .blue => {
                        if (min_blue == null or min_blue.? < item.amount) {
                            min_blue = item.amount;
                        }
                    },
                }
            }
        }

        const power = min_red.? * min_green.? * min_blue.?;

        sum += power;
    }

    try stdout.print("{d}\n", .{sum});
}
