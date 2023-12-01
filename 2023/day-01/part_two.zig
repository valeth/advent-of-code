const std = @import("std");
const List = std.ArrayList;

// Can't be bothered to read the file dynamically
const data = @embedFile("inputs/puzzle.txt");

const numbers = [_][]const u8{
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
};

const Digit = struct {
    pos: usize,
    val: u8,
};

fn digit_position(_: void, a: Digit, b: Digit) bool {
    return a.pos < b.pos;
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;

    const stdout = std.io.getStdOut().writer();
    var lines = std.mem.tokenizeSequence(u8, data, "\n");
    var sum: u32 = 0;

    while (lines.next()) |line| {
        var digits = List(Digit).init(alloc);
        defer digits.deinit();

        for (line, 0..) |byte, idx| {
            if (std.ascii.isDigit(byte)) {
                const digit = try std.fmt.charToDigit(byte, 10);
                try digits.append(.{ .pos = idx, .val = digit });
            }
        }

        var line_idx: usize = 0;

        while (line_idx < line.len) {
            const cur = line[line_idx..];

            for (numbers, 1..) |number, digit| {
                if (std.mem.startsWith(u8, cur, number)) {
                    try digits.append(.{ .pos = line_idx, .val = @intCast(digit) });
                }
            }

            line_idx += 1;
        }

        const sorted = try digits.toOwnedSlice();
        std.mem.sort(Digit, sorted, {}, digit_position);
        const first = sorted[0].val;
        const last = sorted[sorted.len - 1].val;

        const val = (first * 10) + last;
        sum += val;
    }

    try stdout.print("{}\n", .{sum});
}
