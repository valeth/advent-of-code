const std = @import("std");
const alloc = std.heap.page_allocator;

const List = std.ArrayList;
const HashMap = std.AutoHashMap;
const SymbolMap = HashMap(usize, HashMap(usize, u8));

pub const Symbol = struct {
    val: u8,
    row: usize,
    col: usize,
};

pub const Number = struct {
    val: u32,
    row: usize,
    col: usize,
    len: usize,

    pub fn has_adjacent_symbols(self: *const Number, schematic: *const Schematic) !bool {
        const adjacent = try self.adjacent_symbols(schematic);
        defer adjacent.deinit();
        return adjacent.items.len != 0;
    }

    pub fn adjacent_symbols(self: *const Number, schematic: *const Schematic) !List(Symbol) {
        const symbols = schematic.symbols;
        const height = schematic.height;
        var adjacent = List(Symbol).init(alloc);

        const num_start = self.col;
        const num_end = self.col + self.len;

        for (num_start..num_end) |num_pos| {
            if (num_pos > 0 and num_pos == num_start) {
                const start = if (self.row == 0) self.row else self.row - 1;
                const end = if (self.row == height - 1) self.row + 1 else self.row + 2;

                for (start..end) |row| {
                    if (symbols.get(row)) |sym_row| {
                        if (sym_row.get(num_pos - 1)) |sym| {
                            try adjacent.append(.{ .val = sym, .row = row, .col = num_pos - 1 });
                        }
                    }
                }
            }

            if (self.row > 0) {
                if (symbols.get(self.row - 1)) |sym_row_above| {
                    if (sym_row_above.get(num_pos)) |sym| {
                        try adjacent.append(.{ .val = sym, .row = self.row - 1, .col = num_pos });
                    }
                }
            }

            if (self.row < height - 1) {
                if (symbols.get(self.row + 1)) |sym_row_below| {
                    if (sym_row_below.get(num_pos)) |sym| {
                        try adjacent.append(.{ .val = sym, .row = self.row + 1, .col = num_pos });
                    }
                }
            }

            if (num_pos == num_end - 1) {
                const start = if (self.row == 0) self.row else self.row - 1;
                const end = if (self.row == height - 1) self.row + 1 else self.row + 2;

                for (start..end) |row| {
                    if (symbols.get(row)) |sym_row| {
                        if (sym_row.get(num_pos + 1)) |sym| {
                            try adjacent.append(.{ .val = sym, .row = row, .col = num_pos + 1 });
                        }
                    }
                }
            }
        }

        return adjacent;
    }
};

pub const Schematic = struct {
    numbers: List(List(Number)),
    symbols: SymbolMap,
    width: usize,
    height: usize,
};

pub fn parse(data: []const u8) !Schematic {
    var lines = std.mem.tokenizeSequence(u8, data, "\n");
    var numbers = List(List(Number)).init(alloc);
    var symbols = SymbolMap.init(alloc);
    var width: ?usize = null;

    var i: usize = 0;
    while (lines.next()) |line| {
        if (width == null) {
            width = line.len;
        }

        var symbols_row = HashMap(usize, u8).init(alloc);
        var numbers_row = List(Number).init(alloc);
        var num_buf = List(u8).init(alloc);

        var j: usize = 0;
        for (line) |byte| {
            if (std.ascii.isDigit(byte)) {
                try num_buf.append(byte);
            } else {
                if (try parse_number(&num_buf, i, j)) |num| {
                    try numbers_row.append(num);
                }

                if (byte != '.') {
                    try symbols_row.put(j, byte);
                }
            }

            j += 1;
        }

        if (try parse_number(&num_buf, i, j)) |num| {
            try numbers_row.append(num);
        }

        try numbers.append(numbers_row);
        try symbols.put(i, symbols_row);

        i += 1;
    }

    const schematic = .{
        .numbers = numbers,
        .symbols = symbols,
        .width = width.?,
        .height = i,
    };

    return schematic;
}

fn parse_number(buf: *List(u8), row: usize, col: usize) !?Number {
    const num_raw = try buf.toOwnedSlice();
    const len = num_raw.len;

    if (len > 0) {
        const val = try std.fmt.parseInt(u32, num_raw, 10);
        const num = .{
            .val = val,
            .row = row,
            .col = col - len,
            .len = len,
        };
        return num;
    }

    return null;
}
