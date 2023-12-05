const std = @import("std");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;

pub const Card = struct {
    winning: List(u32),
    having: List(u32),
};

pub fn parse(data: []const u8) !List(Card) {
    var lines = std.mem.tokenizeSequence(u8, data, "\n");

    var cards = List(Card).init(alloc);

    while (lines.next()) |line| {
        var line_parts = std.mem.tokenizeSequence(u8, line, ": ");
        _ = line_parts.next();

        const numbers = line_parts.next().?;
        var number_parts = std.mem.tokenizeSequence(u8, numbers, " | ");

        const winning = number_parts.next().?;
        var winning_numbers = std.mem.tokenizeSequence(u8, winning, " ");

        var winning_parsed = List(u32).init(alloc);

        while (winning_numbers.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            try winning_parsed.append(num);
        }

        const having = number_parts.next().?;
        var having_numbers = std.mem.tokenizeSequence(u8, having, " ");
        var having_parsed = List(u32).init(alloc);

        while (having_numbers.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            try having_parsed.append(num);
        }

        const card = .{
            .winning = winning_parsed,
            .having = having_parsed,
        };

        try cards.append(card);
    }

    return cards;
}
