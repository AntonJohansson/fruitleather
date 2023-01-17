const std = @import("std");

pub fn err(comptime format: []const u8, args: anytype) void {
    const stderr = std.io.getStdErr().writer();
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    var tty_config = std.debug.detectTTYConfig();

    tty_config.setColor(stderr, .Red);
    nosuspend stderr.writeAll("Error: ") catch return;
    tty_config.setColor(stderr, .Reset);
    nosuspend stderr.print(format, args) catch return;
}

pub fn errAtFmt(filename: []const u8, buf: []const u8, index: usize, errmsg: []const u8, comptime format: []const u8, args: anytype) void {
    var buffer: [128]u8 = undefined;
    const locmsg = std.fmt.bufPrint(&buffer, format, args) catch return;
    errAt(filename, buf, index, errmsg, locmsg);
}

pub fn errAt(filename: []const u8, buf: []const u8, index: usize, errmsg: []const u8, locmsg: []const u8) void {
    var line_start_index = index;
    while (buf[line_start_index] != '\n' and
           buf[line_start_index] != '\r' and
           line_start_index > 0)
        : (line_start_index -= 1) {}

    var line_end_index = index+1;
    while (buf[line_end_index] != '\n' and
           buf[line_end_index] != '\r' and
           line_end_index < buf.len)
        : (line_end_index += 1) {}

    var line_count_index: usize = line_start_index;
    var line_count: usize = 1;
    while (line_count_index > 0)
        : (line_count_index -= 1) {
        if (buf[line_count_index] == '\n' or
            buf[line_count_index] == '\r')
            line_count += 1;
    }

    const stderr = std.io.getStdErr().writer();
    std.debug.getStderrMutex().lock();
    defer std.debug.getStderrMutex().unlock();
    var tty_config = std.debug.detectTTYConfig();

    tty_config.setColor(stderr, .Red);
    nosuspend stderr.print("Error: {s}\n", .{errmsg}) catch return;
    tty_config.setColor(stderr, .Reset);

    const loc_width = 10;

    tty_config.setColor(stderr, .Red);
    var loc_buffer: [64]u8 = undefined;
    const loc = std.fmt.bufPrint(&loc_buffer, "{s}:{}", .{filename, line_count}) catch return;
    nosuspend stderr.print("{s:[width]}| ", .{.string = loc, .width = loc_width}) catch return;
    tty_config.setColor(stderr, .Reset);

    nosuspend stderr.print("{s}\n", .{buf[(line_start_index+1)..line_end_index]}) catch return;
    tty_config.setColor(stderr, .Red);
    nosuspend stderr.print("{s:[width]}\n", .{.string = "^\\", .width = loc_width + 2 + index+1 - line_start_index}) catch return;
    nosuspend stderr.print("{s:[width]}\n", .{.string = locmsg, .width = loc_width + 2 + locmsg.len + index+1 - line_start_index}) catch return;
    tty_config.setColor(stderr, .Reset);
}
