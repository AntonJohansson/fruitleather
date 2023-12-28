const std = @import("std");

var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
const gpa = gpa_state.allocator();
var arena_state = std.heap.ArenaAllocator.init(gpa);
const arena = arena_state.allocator();

var name_map = std.StringHashMap(*Entry).init(gpa);
var roots: std.BoundedArray(*Entry, 128) = .{};
var entries_being_tracked: std.BoundedArray(*Entry, 128) = .{};

pub fn start(name: []const u8) void {
    const entry = name_map.get(name) orelse blk: {
        const new_entry = arena.create(Entry) catch unreachable;
        new_entry.* = .{};
        name_map.put(name, new_entry) catch unreachable;

        if (entries_being_tracked.len > 0) {
            const parent = entries_being_tracked.get(entries_being_tracked.len-1);
            parent.children.appendAssumeCapacity(new_entry);
        } else {
            roots.appendAssumeCapacity(new_entry);
        }

        break :blk new_entry;
    };

    entry.name = name;

    if (entries_being_tracked.len == 0) {
        entry.is_root = true;
    }

    entry.startTime();
    entries_being_tracked.appendAssumeCapacity(entry);
}

pub fn end() void {
    std.debug.assert(entries_being_tracked.len != 0);
    const entry = entries_being_tracked.pop();
    entry.endTime();
}

fn cmpEntry(_: void, lhs: *Entry, rhs: *Entry) bool {
    return lhs.mean_std().avg < rhs.mean_std().avg;
}

pub fn sort() void {
    var worklist: std.BoundedArray(*Entry, 64) = .{};

    std.sort.pdq(*Entry, roots.slice(), {}, cmpEntry);

    for (roots.slice()) |e| {
        worklist.appendAssumeCapacity(e);
    }

    while (worklist.len > 0) {
        const e = worklist.pop();

        std.sort.pdq(*Entry, e.children.slice(), {}, cmpEntry);

        for (e.children.slice()) |c| {
            worklist.appendAssumeCapacity(c);
        }
    }
}

pub fn dump() void {
    var worklist: std.BoundedArray(struct {
        entry: *Entry= undefined,
        depth: u32 = 0,
    } , 64) = .{};

    for (roots.slice()) |s| {
        worklist.appendAssumeCapacity(.{.entry=s});
    }

    const depth_width = 2;
    var name_buf: [16]u8 = undefined;

    while (worklist.len > 0) {
        const work = worklist.pop();

        const ns_per_us = 1000;
        const result = work.entry.mean_std();
        {
            @memset(&name_buf, ' ');
            @memcpy(name_buf[depth_width*work.depth..(depth_width*work.depth+work.entry.name.len)], work.entry.name);

            std.log.info("{s}|{:8}", .{
                name_buf,
                result.avg/ns_per_us,
            });
        }

        for (work.entry.children.slice()) |c| {
            worklist.appendAssumeCapacity(.{
                .entry = c,
                .depth = work.depth+1,
            });
        }
    }
}

const Result = struct {
    avg: u64,
    std: u64,
    min: u64,
    max: u64,
};

fn CircularBuffer(comptime T: type, comptime max_len: usize) type {
    return struct {
        data: [max_len]T = undefined,
        top: usize = 0,
        size: usize = 0,

        pub fn push(self: *@This(), element: T) void {
            self.data[self.top] = element;
            self.top = (self.top + 1) % self.data.len;
            if (self.size < self.data.len)
                self.size += 1;
        }

        pub fn peek(self: *@This()) T {
            return self.data[self.top];
        }

        pub fn peekRelative(self: *@This(), offset: i64) T {
            const index: usize = @intCast(@mod(@as(i64, @intCast(self.top)) + @as(i64, @intCast(self.data.len)) + offset, @as(i64, @intCast(self.data.len))));
            return self.data[index];
        }

        pub fn slice(self: *@This()) []T {
            return self.data[0..self.size];
        }
    };
}

const Entry = struct {
    name: []const u8 = undefined,
    samples: CircularBuffer(u64, 256) = .{},
    start_time: std.time.Instant = undefined,
    is_root: bool = false,

    children: std.BoundedArray(*Entry, 16) = .{},

    const Self = @This();

    pub fn startTime(self: *Self) void {
        self.start_time = std.time.Instant.now() catch unreachable;
    }

    pub fn endTime(self: *Self) void {
        const end_time = std.time.Instant.now() catch unreachable;
        self.samples.push(end_time.since(self.start_time));
    }

    pub fn mean_std(self: *Self) Result {
        var avg: u64 = 0;
        for (self.samples.slice()) |s| {
            avg += s;
        }
        avg /= self.samples.size;

        var variance: u64 = 0;
        var min: u64 = std.math.maxInt(u64);
        var max: u64 = std.math.minInt(u64);
        if (self.samples.size > 1) {
            for (self.samples.data) |s| {
                const d = @as(i64, @intCast(s)) - @as(i64, @intCast(avg));
                const mul = @mulWithOverflow(d,d);
                // Skip on overflow
                if (mul[1] == 1)
                    continue;
                variance = @addWithOverflow(variance, @as(@TypeOf(variance), @intCast(mul[0])))[0];

                if (s < min)
                    min = s;
                if (s > max)
                    max = s;
            }
            variance /= self.samples.size-1;
        }

        const std_float = std.math.sqrt(@as(f64, @floatFromInt(variance)));

        return Result {
            .avg = avg,
            .std = @intFromFloat(std_float),
            .min = min,
            .max = max,
        };
    }
};
