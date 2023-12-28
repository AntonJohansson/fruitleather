const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const log = @import("log.zig");

const ParseState = parser.ParseState;
const AstNode = parser.AstNode;

pub fn emit(state: *ParseState, filename: []const u8, statements: std.ArrayList(*AstNode)) !void {
    const file = std.fs.cwd().createFile(filename, .{})  catch |err|  {
        std.log.err("Failed to open file: {s} ({})", .{filename, err});
        return;
    };
    defer file.close();
    const writer = file.writer();

    try writer.writeAll("digraph {\n");
    try writer.writeAll("bgcolor=\"black\"");

    for (statements.items) |node| {
        dumpStatementsToDotImpl(state, writer, node);
    }

    try writer.writeAll("}\n");
}

fn dumpStatementsToDotImpl(state: *ParseState, writer: std.fs.File.Writer, node: *AstNode) void {
    writer.print("\"{*}\" [fontcolor=\"white\",color=\"white\",label=\"", .{node}) catch return;
    // TODO(anjo): In more recent version of zig we should be able to generate
    // all cases at compile time using inline else => |i| {}.
    switch (node.*.ast_type) {
        .header => {
            writer.writeAll("header") catch return;
        },
        .intrin => {
            writer.writeAll("intrin") catch return;
        },
        .block => {
            writer.writeAll("block") catch return;
        },
        .thm => {
            writer.writeAll("thm") catch return;
        },
        .def => {
            writer.writeAll("def") catch return;
        },
        .pf => {
            writer.writeAll("pf") catch return;
        },
        .code => {
            writer.writeAll("code") catch return;
        },
        .var_name, .call_op, .unary_op, .sum, .prod, .text,
        .number => |i| {
            const loc = state.buffer.locations.items[i];
            const name = state.filebuf[loc.start..loc.end];
            writer.print("{s}: {s}", .{@tagName(node.*.ast_type), name}) catch return;
        },
        .bin_op => |i| {
            const loc = state.buffer.locations.items[i.op];
            const name = state.filebuf[loc.start..loc.end];
            writer.print("{s}", .{name}) catch return;
        },
        .var_decl  => |i| {
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields, 0..) |f,j| {
                if (f.type != usize)
                    continue;
                const index = @field(i, f.name);
                const loc = state.buffer.locations.items[index];
                const name = state.filebuf[loc.start..loc.end];
                if (j > 0)
                    writer.writeAll("\\n") catch return;
                writer.print("{s}", .{name}) catch return;
            }
        },
        .type_decl => |i| {
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields, 0..) |f, j| {
                const index = @field(i, f.name);
                const loc = state.buffer.locations.items[index];
                const name = state.filebuf[loc.start..loc.end];
                if (j > 0)
                    writer.writeAll("\\n") catch return;
                writer.print("{s}", .{name}) catch return;
            }
        },
        .op_decl => |i| {
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields, 0..) |f, j| {
                const index = @field(i, f.name);
                if (@TypeOf(index) == usize) {
                    const loc = state.buffer.locations.items[index];
                    const name = state.filebuf[loc.start..loc.end];
                    if (j > 0)
                        writer.writeAll("\\n") catch return;
                    writer.print("{s}", .{name}) catch return;
                } else {
                    writer.print("{s}", .{f.name}) catch return;
                }
            }
        },
        .mat => |i| {
            writer.writeAll("Matrix\n") catch return;
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields, 0..) |f, j| {
                if (j > 0)
                    writer.writeAll("\\n") catch return;
                if (f.type == usize) {
                    const index = @field(i, f.name);
                    const loc = state.buffer.locations.items[index];
                    const name = state.filebuf[loc.start..loc.end];
                    writer.print("{s}", .{name}) catch return;
                } else {
                    writer.print("{s}: {}", .{f.name, @field(i, f.name)}) catch return;
                }
            }
        },
    }
    writer.print("\\ntype: {}", .{node.typeindex}) catch return;
    writer.writeAll("\"]\n") catch return;

    for (node.children.items) |child| {
        writer.print("\"{*}\" -> \"{*}\" [color=\"white\"]\n", .{node, child}) catch return;
        dumpStatementsToDotImpl(state, writer, child);
    }
}
