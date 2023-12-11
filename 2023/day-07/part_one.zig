const std = @import("std");
const common = @import("common.zig");
const Hand = common.Hand;

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const hands = try common.parse(data);
    defer hands.deinit();

    var sum: u64 = 0;

    std.mem.sort(Hand, hands.items, {}, compare_hands);

    for (hands.items, 1..) |hand, rank| {
        const value = hand.bid * rank;
        sum += value;
    }

    try stdout.print("{d}\n", .{sum});
}

fn compare_hands(_: void, lhs: Hand, rhs: Hand) bool {
    if (lhs.type == rhs.type) {
        for (lhs.cards, rhs.cards) |lcard, rcard| {
            if (lcard != rcard) {
                return lcard < rcard;
            }
        }
    } else {
        return lhs.type.as_u8() < rhs.type.as_u8();
    }

    return false;
}
