const std = @import("std");
const common = @import("common.zig");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;
const Idx = common.Idx;

const data = @embedFile("inputs/puzzle.txt");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const parsed = try common.parse(data);
    const mappings = parsed.mappings;
    const seeds = parsed.seeds;

    var min_loc: ?Idx = null;

    for (seeds.items) |seed| {
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

        if (min_loc == null or min_loc.? > loc) {
            min_loc = loc;
        }
    }

    try stdout.print("{d}\n", .{min_loc.?});
}
