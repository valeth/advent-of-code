const std = @import("std");
const common = @import("common.zig");
const Idx = common.Idx;
const AtomicIdx = std.atomic.Value(Idx);
const AtomicOrder = std.builtin.AtomicOrder;
const Mappings = common.Mappings;
const List = std.ArrayList;
const alloc = std.heap.page_allocator;

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const parsed = try common.parse(data);
    const mappings = parsed.mappings;
    const seeds = parsed.seeds;

    var chunk = std.mem.window(Idx, seeds.items, 2, 2);
    var threads = List(std.Thread).init(alloc);

    var min_loc = AtomicIdx.init(18_446_744_073_709_551_615);

    // Just brute force this bitch
    while (chunk.next()) |pair| {
        const start = pair[0];
        const end = pair[0] + pair[1];

        const thread = try std.Thread.spawn(.{}, get_min_location, .{ &mappings, &min_loc, start, end });
        try threads.append(thread);
    }

    for (threads.items) |thread| {
        thread.join();
    }

    try stdout.print("{d}\n", .{min_loc.raw});
}

fn get_min_location(mappings: *const Mappings, min_loc: *AtomicIdx, start: Idx, end: Idx) !void {
    for (start..end) |seed| {
        const ss_map = mappings.get(.{ .Seed, .Soil }).?;
        const soil = ss_map.get(seed);

        const sf_map = mappings.get(.{ .Soil, .Fertilizer }).?;
        const fert = sf_map.get(soil);

        const fw_map = mappings.get(.{ .Fertilizer, .Water }).?;
        const water = fw_map.get(fert);

        const wl_map = mappings.get(.{ .Water, .Light }).?;
        const light = wl_map.get(water);

        const lt_map = mappings.get(.{ .Light, .Temperature }).?;
        const temp = lt_map.get(light);

        const th_map = mappings.get(.{ .Temperature, .Humidity }).?;
        const humi = th_map.get(temp);

        const hl_map = mappings.get(.{ .Humidity, .Location }).?;
        const loc = hl_map.get(humi);

        if (loc < min_loc.load(AtomicOrder.SeqCst)) {
            min_loc.store(loc, AtomicOrder.SeqCst);
        }
    }
}
