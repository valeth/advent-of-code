const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

const MAX_RED = 12;
const MAX_GREEN = 13;
const MAX_BLUE = 14;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const games = try common.parse(data);
    defer games.deinit();

    var sum: u32 = 0;

    games: for (games.items) |game| {
        for (game.sets.items) |set| {
            for (set.items) |item| {
                switch (item.color) {
                    .red => {
                        if (item.amount > MAX_RED) {
                            continue :games;
                        }
                    },
                    .green => {
                        if (item.amount > MAX_GREEN) {
                            continue :games;
                        }
                    },
                    .blue => {
                        if (item.amount > MAX_BLUE) {
                            continue :games;
                        }
                    },
                }
            }
        }

        sum += game.id;
    }

    try stdout.print("{d}\n", .{sum});
}
