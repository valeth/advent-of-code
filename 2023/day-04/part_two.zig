const std = @import("std");
const common = @import("common.zig");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;
const HashMap = std.AutoHashMap(usize, u8);

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const cards = try common.parse(data);
    defer cards.deinit();

    const last_card_id = cards.items.len - 1;

    var matches = HashMap.init(alloc);
    defer matches.deinit();

    var card_stack = List(usize).init(alloc);
    defer card_stack.deinit();

    {
        var card_id = cards.items.len;
        var cards_iter = std.mem.reverseIterator(cards.items);
        while (cards_iter.next()) |card| {
            card_id -= 1;

            try matches.put(card_id, 0);
            try card_stack.append(card_id);

            for (card.winning.items) |winning_num| {
                for (card.having.items) |having_num| {
                    if (winning_num == having_num) {
                        if (matches.get(card_id)) |match| {
                            try matches.put(card_id, match + 1);
                        }
                    }
                }
            }
        }
    }

    var total_cards: u32 = 0;

    while (card_stack.popOrNull()) |card_id| {
        total_cards += 1;

        const card_matches = matches.get(card_id).?;

        if (card_matches != 0 and card_id < last_card_id) {
            const remaining_cards = last_card_id - card_id;

            if (remaining_cards > 0) {
                const remaining_matches = if (remaining_cards >= card_matches) card_matches else remaining_cards;
                const new_cards_start = card_id + 1;
                const new_cards_end = card_id + remaining_matches + 1;

                for (new_cards_start..new_cards_end) |new_card_id| {
                    try card_stack.append(new_card_id);
                }
            }
        }
    }

    try stdout.print("{d}\n", .{total_cards});
}
