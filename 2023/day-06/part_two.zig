const std = @import("std");
const common = @import("common.zig");
const alloc = std.heap.page_allocator;
const Buffer = std.ArrayList(u8);

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const races = try common.parse(data);
    defer races.deinit();

    const time = try concat_numbers(races.times.items);
    const record = try concat_numbers(races.distances.items);

    const nums_beat = common.calculate(time, record);

    try stdout.print("{d}\n", .{nums_beat});
}

fn concat_numbers(nums: []const u32) !u64 {
    var buf = Buffer.init(alloc);
    defer buf.deinit();

    for (nums) |race_times| {
        const segment = try std.fmt.allocPrint(alloc, "{d}", .{race_times});
        try buf.appendSlice(segment);
    }

    const num = try std.fmt.parseInt(u64, buf.items, 10);

    return num;
}
