const std = @import("std");
const alloc = std.heap.page_allocator;

pub const Races = struct {
    times: List(u32),
    distances: List(u32),

    pub fn deinit(self: Races) void {
        self.times.deinit();
        self.distances.deinit();
    }
};

const List = std.ArrayList;

pub fn parse(data: []const u8) !Races {
    var lines = std.mem.tokenizeScalar(u8, data, '\n');

    var times = List(u32).init(alloc);

    {
        const line = lines.next().?;
        var numbers = std.mem.tokenizeScalar(u8, line, ' ');
        _ = numbers.next();

        while (numbers.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            try times.append(num);
        }
    }

    var dists = List(u32).init(alloc);

    {
        const line = lines.next().?;
        var numbers = std.mem.tokenizeScalar(u8, line, ' ');
        _ = numbers.next();

        while (numbers.next()) |num_str| {
            const num = try std.fmt.parseInt(u32, num_str, 10);
            try dists.append(num);
        }
    }

    const races = .{
        .times = times,
        .distances = dists,
    };

    return races;
}
