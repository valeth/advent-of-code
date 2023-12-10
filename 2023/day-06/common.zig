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

pub fn calculate(time: u64, record_dist: u64) u64 {
    var min: u64 = undefined;

    for (1..time) |speed| {
        const rem_race_time = time - speed;
        const dist = rem_race_time * speed;
        if (dist > record_dist) {
            min = @intCast(speed);
            break;
        }
    }

    var max: u64 = undefined;

    var speed: u64 = time - 1;
    while (speed > 0) {
        const rem_race_time = time - speed;
        const dist = rem_race_time * speed;

        if (dist > record_dist) {
            max = speed;
            break;
        }

        speed -= 1;
    }

    return max - min + 1;
}

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
