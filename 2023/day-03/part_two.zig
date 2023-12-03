const std = @import("std");
const common = @import("common.zig");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;
const HashMap = std.AutoHashMap;

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const schematic = try common.parse(data);

    var sym_map = HashMap(common.Symbol, List(u32)).init(alloc);

    for (schematic.numbers.items) |row| {
        for (row.items) |num| {
            const adjacent = try num.adjacent_symbols(&schematic);

            for (adjacent.items) |sym| {
                if (sym.val == '*') {
                    const entry = try sym_map.getOrPut(sym);

                    if (entry.found_existing) {
                        try entry.value_ptr.*.append(num.val);
                    } else {
                        var list = List(u32).init(alloc);
                        try list.append(num.val);
                        entry.value_ptr.* = list;
                    }
                }
            }
        }
    }

    var sum: u32 = 0;

    var iter = sym_map.iterator();
    while (iter.next()) |sym| {
        const nums = sym.value_ptr.*.items;

        if (nums.len == 2) {
            const ratio = nums[0] * nums[1];
            sum += ratio;
        }
    }

    try stdout.print("{d}\n", .{sum});
}
