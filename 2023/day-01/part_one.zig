const std = @import("std");

// Can't be bothered to read the file dynamically
const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    var lines = std.mem.tokenizeSequence(u8, data, "\n");
    var sum: u32 = 0;

    while (lines.next()) |line| {
        var first: ?u8 = null;
        var last: ?u8 = null;

        for (line) |byte| {
            if (std.ascii.isDigit(byte)) {
                if (first == null) {
                    first = byte;
                }
                last = byte;
            }
        }

        if (first != null and last != null) {
            const first_d = try std.fmt.charToDigit(first.?, 10);
            const last_d = try std.fmt.charToDigit(last.?, 10);
            const val = (first_d * 10) + last_d;

            sum += val;
        }
    }

    try stdout.print("{}", .{sum});
}
