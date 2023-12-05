const std = @import("std");
const common = @import("common.zig");

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const cards = try common.parse(data);
    defer cards.deinit();

    var sum: u32 = 0;

    for (cards.items) |card| {
        var points: u32 = 0;

        for (card.winning.items) |winning_num| {
            for (card.having.items) |having_num| {
                if (winning_num == having_num) {
                    if (points == 0) {
                        points = 1;
                    } else {
                        points *= 2;
                    }
                }
            }
        }

        sum += points;
    }

    try stdout.print("{d}\n", .{sum});
}
