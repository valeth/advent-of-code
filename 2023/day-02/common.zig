const std = @import("std");
const List = std.ArrayList;
const alloc = std.heap.page_allocator;

pub const Game = struct {
    id: u32,
    sets: List(List(Cubes)),
};

pub const Cubes = struct {
    amount: u32,
    color: CubeColor,
};

pub const CubeColor = enum {
    red,
    green,
    blue,
};

pub fn parse(data: []const u8) !List(Game) {
    var lines = std.mem.tokenizeSequence(u8, data, "\n");
    var games = List(Game).init(alloc);

    while (lines.next()) |line| {
        var game_seq = std.mem.tokenizeSequence(u8, line, ": ");

        const game_info = game_seq.next().?;
        const game_id = try std.fmt.parseInt(u32, game_info[5..], 10);

        const game_sets = game_seq.next().?;
        var sets_seq = std.mem.tokenizeSequence(u8, game_sets, "; ");

        var sets = List(List(Cubes)).init(alloc);

        while (sets_seq.next()) |set| {
            var items = std.mem.tokenizeSequence(u8, set, ", ");

            var x = List(Cubes).init(alloc);

            while (items.next()) |item| {
                var cubes_data = std.mem.tokenizeScalar(u8, item, ' ');

                const amount = try std.fmt.parseInt(u32, cubes_data.next().?, 10);

                const color_str = cubes_data.next().?;
                var color: ?CubeColor = null;

                if (std.mem.eql(u8, color_str, "blue")) {
                    color = CubeColor.blue;
                } else if (std.mem.eql(u8, color_str, "red")) {
                    color = CubeColor.red;
                } else if (std.mem.eql(u8, color_str, "green")) {
                    color = CubeColor.green;
                } else {
                    unreachable;
                }

                try x.append(.{ .amount = amount, .color = color.? });
            }

            try sets.append(x);
        }

        const game = .{ .id = game_id, .sets = sets };
        try games.append(game);
    }

    return games;
}
