const std = @import("std");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;
const OccuranceMap = std.AutoHashMap(Card, u8);

pub const Hand = struct {
    cards: [5]Card,
    bid: u64,
    type: HandType,
};

pub const HandType = enum {
    Five,
    Four,
    FullHouse,
    Three,
    TwoPair,
    OnePair,
    HighCard,

    pub fn as_u8(self: HandType) u8 {
        return switch (self) {
            .Five => 7,
            .Four => 6,
            .FullHouse => 5,
            .Three => 4,
            .TwoPair => 3,
            .OnePair => 2,
            .HighCard => 1,
        };
    }
};

pub const Card = u8;

pub fn parse(data: []const u8) !List(Hand) {
    var lines = std.mem.tokenizeScalar(u8, data, '\n');

    var hands = List(Hand).init(alloc);

    while (lines.next()) |line| {
        var line_parts = std.mem.tokenizeScalar(u8, line, ' ');

        const cards_part = line_parts.next().?;

        var cards: [5]Card = undefined;
        var occurances = OccuranceMap.init(alloc);
        defer occurances.deinit();

        for (cards_part, 0..) |card, i| {
            const card_num = switch (card) {
                'A' => 14,
                'K' => 13,
                'Q' => 12,
                'J' => 11,
                'T' => 10,
                '2'...'9' => card - 48,
                else => unreachable,
            };
            cards[i] = card_num;

            if (occurances.get(card_num)) |val| {
                try occurances.put(card_num, val + 1);
            } else {
                try occurances.put(card_num, 1);
            }
        }

        const hand_type = determine_hand_type(&occurances);

        const bid_part = line_parts.next().?;
        const bid = try std.fmt.parseInt(u64, bid_part, 10);

        const hand = .{
            .cards = cards,
            .bid = bid,
            .type = hand_type,
        };

        try hands.append(hand);
    }

    return hands;
}

fn determine_hand_type(occurances: *const OccuranceMap) HandType {
    switch (occurances.count()) {
        1 => return HandType.Five,
        2 => {
            var iter = occurances.valueIterator();
            while (iter.next()) |val| {
                switch (val.*) {
                    1, 4 => return HandType.Four,
                    else => return HandType.FullHouse,
                }
            }
        },
        3 => {
            var iter = occurances.valueIterator();
            while (iter.next()) |val| {
                switch (val.*) {
                    3 => return HandType.Three,
                    2 => return HandType.TwoPair,
                    else => continue,
                }
                unreachable;
            }
        },
        4 => return HandType.OnePair,
        5 => return HandType.HighCard,
        else => unreachable,
    }

    unreachable;
}
