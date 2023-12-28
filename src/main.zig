const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typer = @import("typer.zig");
const latex = @import("latex.zig");
const dot = @import("dot.zig");
const log = @import("log.zig");
const profile = @import("profile.zig");

const ParseState = parser.ParseState;
const AstNode = parser.AstNode;

pub fn main() !void {
    const pa = std.heap.page_allocator;
    const args = std.process.argsAlloc(pa) catch {
        std.log.err("Failed to process commandline arguments!", .{});
        return;
    };
    defer std.process.argsFree(pa, args);

    if (args.len != 2) {
        std.log.err("usage: fl [input file]", .{});
        return;
    }

    profile.start("total");
    defer {
        profile.end();
        profile.sort();
        profile.dump();
    }

    const input_file_path = args[1];
    var buf: []const u8 = undefined;
    {
        profile.start("read");
        defer profile.end();

        const input_file = std.fs.cwd().openFile(input_file_path, .{}) catch |err| {
            std.log.err("Failed to open file: {s} ({})", .{input_file_path, err});
            return;
        };
        defer input_file.close();
        buf = input_file.readToEndAlloc(pa, 1024 * 1024 * 1024) catch {
            std.log.err("Failed to read file: {s}", .{input_file_path});
            return;
        };
    }

    profile.start("lexer");
    var buffer = lexer.lex(pa, input_file_path, buf) catch |err| {
        log.err("Lexing failed ({})\n", .{err});
        return;
    };
    profile.end();

    {
        profile.start("dump tokens");
        defer profile.end();

        const filename = "tokens";
        const file = std.fs.cwd().createFile(filename, .{})  catch |err|  {
            std.log.err("Failed to open file: {s} ({})", .{filename, err});
            return;
        };
        defer file.close();
        const writer = file.writer();

        for (buffer.tokens.items, 0..) |t, j| {
            const loc = buffer.locations.items[j];
            try writer.print("{} \t\t {s}\n", .{t, buf[loc.start..loc.end]});
        }
    }

    var arena = std.heap.ArenaAllocator.init(pa);
    defer arena.deinit();

    profile.start("parser");
    var parse_state = parser.ParseState{
        .buffer = &buffer,
        .allocator = arena.allocator(),
        .filebuf = buf,
        .filename = input_file_path,
        .identifier_ops_decl = std.ArrayList(*parser.AstOpDecl).init(arena.allocator()),
    };
    var statements = try parser.parse(&parse_state);
    profile.end();

    profile.start("dump dot");
    dot.emit(&parse_state, "ast.dot", statements) catch {
        std.log.err("Failed to dump parse_state to .dot", .{});
    };
    profile.end();

    profile.start("typer");
    const ast_types = typer.propagateTypes(&parse_state, &statements) catch {
        std.log.err("Failed to set AST types", .{});
        return;
    };
    profile.end();

    profile.start("dump dot");
    dot.emit(&parse_state, "ast.typed.dot", statements) catch {
        std.log.err("Failed to dump parse_state to .dot", .{});
    };
    profile.end();

    profile.start("dump latex");
    latex.emit(&parse_state, "out.tex", statements, ast_types) catch {
        std.log.err("Failed top dump statements to latex!", .{});
    };
    profile.end();
}
