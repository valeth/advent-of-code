const std = @import("std");
const alloc = std.heap.page_allocator;
const List = std.ArrayList;
pub const Mappings = std.AutoHashMap(MappingKey, Mapping);
pub const Idx = u64;

pub const SeedList = List(Idx);

pub const MappingType = enum {
    Seed,
    Soil,
    Fertilizer,
    Water,
    Light,
    Temperature,
    Humidity,
    Location,
};

pub const MappingKey = struct {
    MappingType,
    MappingType,
};

pub const Mapping = struct {
    dest_ranges: []MappingRange,
    source_ranges: []MappingRange,

    pub fn get(self: Mapping, val: Idx) Idx {
        for (self.source_ranges, 0..) |src_range, i| {
            if (val >= src_range.start and val < src_range.end) {
                const dest_range = self.dest_ranges[i];
                const dest_offset = (val - src_range.start);
                return dest_range.start + dest_offset;
            }
        }

        return val;
    }
};

pub const Input = struct {
    mappings: Mappings,
    seeds: SeedList,
};

pub const MappingRange = struct {
    start: Idx,
    end: Idx,
};

pub fn parse(data: []const u8) !Input {
    var lines = std.mem.tokenizeSequence(u8, data, "\n");

    var mappings = Mappings.init(alloc);
    var seeds: SeedList = undefined;
    var mapping_types: [2]MappingType = undefined;
    var dest_ranges = List(MappingRange).init(alloc);
    var source_ranges = List(MappingRange).init(alloc);
    var collect_mapping = false;

    while (lines.next()) |line| {
        if (std.mem.startsWith(u8, line, "seeds:")) {
            seeds = try parse_seed_list(line);
        } else if (std.mem.endsWith(u8, line, "map:")) {
            mapping_types = parse_mapping_header(line);
        } else {
            const range = try parse_mapping_body(line);
            try dest_ranges.append(range[0]);
            try source_ranges.append(range[1]);
        }

        if (lines.peek()) |next_line| {
            const new_mapping = std.mem.endsWith(u8, next_line, "map:");
            collect_mapping = new_mapping and dest_ranges.items.len != 0;
        } else {
            collect_mapping = true;
        }

        if (collect_mapping) {
            const dranges = try dest_ranges.toOwnedSlice();
            const sranges = try source_ranges.toOwnedSlice();

            const mapping = .{
                .dest_ranges = dranges,
                .source_ranges = sranges,
            };

            const key = .{
                mapping_types[0],
                mapping_types[1],
            };

            try mappings.put(key, mapping);
        }
    }

    const parsed = .{ .mappings = mappings, .seeds = seeds };

    return parsed;
}

fn parse_seed_list(line: []const u8) !SeedList {
    var list = SeedList.init(alloc);

    var seeds = std.mem.tokenizeSequence(u8, line, " ");
    _ = seeds.next();

    while (seeds.next()) |seed| {
        const seed_num = try std.fmt.parseInt(Idx, seed, 10);
        try list.append(seed_num);
    }

    return list;
}

fn parse_mapping_header(line: []const u8) [2]MappingType {
    const map_types = std.mem.trimRight(u8, line, " map:");

    var map_types_iter = std.mem.tokenizeSequence(u8, map_types, "-to-");

    var types: [2]MappingType = undefined;

    var i: u8 = 0;
    while (map_types_iter.next()) |map_type| {
        if (std.mem.eql(u8, map_type, "seed")) {
            types[i] = MappingType.Seed;
        } else if (std.mem.eql(u8, map_type, "soil")) {
            types[i] = MappingType.Soil;
        } else if (std.mem.eql(u8, map_type, "fertilizer")) {
            types[i] = MappingType.Fertilizer;
        } else if (std.mem.eql(u8, map_type, "water")) {
            types[i] = MappingType.Water;
        } else if (std.mem.eql(u8, map_type, "light")) {
            types[i] = MappingType.Light;
        } else if (std.mem.eql(u8, map_type, "temperature")) {
            types[i] = MappingType.Temperature;
        } else if (std.mem.eql(u8, map_type, "humidity")) {
            types[i] = MappingType.Humidity;
        } else if (std.mem.eql(u8, map_type, "location")) {
            types[i] = MappingType.Location;
        } else {
            unreachable;
        }

        i += 1;
    }

    return types;
}

fn parse_mapping_body(line: []const u8) ![2]MappingRange {
    var values = std.mem.tokenizeScalar(u8, line, ' ');

    var parsed: [3]Idx = undefined;

    var i: u8 = 0;
    while (values.next()) |val| {
        const num = try std.fmt.parseInt(Idx, val, 10);
        parsed[i] = num;
        i += 1;
    }

    const dest_range = .{
        .start = parsed[0],
        .end = parsed[0] + parsed[2],
    };

    const source_range = .{
        .start = parsed[1],
        .end = parsed[1] + parsed[2],
    };

    return .{ dest_range, source_range };
}
