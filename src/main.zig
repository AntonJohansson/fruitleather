const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const log = @import("log.zig");

pub fn main() !void {
    const pa = std.heap.page_allocator;
    const args = try std.process.argsAlloc(pa);
    defer std.process.argsFree(pa, args);

    if (args.len != 2) {
        std.log.err("usage: fl [input file]", .{});
        return;
    }

    //var bench_timer = try std.time.Timer.start();
    //var total_timer = try std.time.Timer.start();

    const input_file = args[1];
    std.log.info("Reading file: {s}", .{input_file});
    const file = std.fs.cwd().openFile(input_file, .{}) catch |err| {
        std.log.err("Failed to open file: {s} ({})", .{input_file, err});
        return;
    };

    const buf = file.readToEndAlloc(pa, 1024*1024*1024) catch {
        std.log.err("Failed to read file: {s}", .{input_file});
        return;
    };

    //const bench_read = bench_timer.read();

    std.log.info("Dumping file contents:\n{s}", .{buf});

    //bench_timer.reset();
    var buffer = lexer.lex(pa, input_file, buf) catch |err| {
        log.err("Lexing failed ({})\n", .{err});
        return;
    };
    //const bench_lex = bench_timer.read();

    for (buffer.tokens.items) |t, j| {
        const loc = buffer.locations.items[j];
        std.debug.print("-- {} \t\t {s}\n", .{t, buf[loc.start..loc.end]});
    }

    var arena = std.heap.ArenaAllocator.init(pa);
    defer arena.deinit();
    var parse_state = parser.ParseState {
        .buffer = &buffer,
        .allocator = arena.allocator(),
        .filebuf = buf,
        .filename = input_file,
    };

    ////bench_timer.reset();
    const statements = parser.parse(&parse_state) catch |err| {
        log.err("Parsing failed ({})\n", .{err});
        return;
    };
    ////const bench_parse = bench_timer.read();

    _ = statements;
    //try dumpStatementsToDot(&parse_state, "ast.dot", statements);

    ////var types = std.StringHashMap(std.BoundedArray([]const u8, 8)).init(pa);
    ////defer types.deinit();

    ////var vars = std.StringHashMap([]const u8).init(pa);
    ////defer vars.deinit();

    ////bench_timer.reset();
    ////for (statements.items) |node| {
    ////    switch (node.ast_type) {
    ////        .var_name => {},
    ////        .var_decl => {
    ////            const var_loc = parse_state.buffer.locations.items[node.ast_type.var_decl.var_name];
    ////            const type_loc = parse_state.buffer.locations.items[node.ast_type.var_decl.type_name];
    ////            const type_name = buf[type_loc.start..type_loc.end];
    ////            const var_name = buf[var_loc.start..var_loc.end];

    ////            var has_error = false;
    ////            if (vars.contains(var_name)) {
    ////                log.errAt(input_file, buf, var_loc.start, "Invalid variable declaration", "Repeat variable declaration");
    ////                has_error = true;

    ////            }
    ////            if (!types.contains(type_name)) {
    ////                log.errAt(input_file, buf, type_loc.start, "Invalid variable declaration", "Undeclared type");
    ////                has_error = true;
    ////            }

    ////            if (!has_error) {
    ////                try vars.put(var_name, type_name);
    ////            }
    ////        },
    ////        .type_decl => {
    ////            const type_loc = parse_state.buffer.locations.items[node.ast_type.type_decl.type_name];
    ////            const latex_loc = parse_state.buffer.locations.items[node.ast_type.type_decl.latex_string];
    ////            const type_name = buf[type_loc.start..type_loc.end];
    ////            const latex_string = buf[latex_loc.start..latex_loc.end];

    ////            if (types.contains(type_name)) {
    ////                log.errAt(input_file, buf, type_loc.start, "Invalid type declaration", "Repeat type declaration");
    ////            } else {
    ////                var latex_args = try std.BoundedArray([]const u8, 8).init(0);
    ////                try latex_args.append(latex_string);

    ////                var child: *parser.AstNode = node.children.get(0);
    ////                const child_loc = parse_state.buffer.locations.items[child.ast_type.var_name];
    ////                const child_name = buf[child_loc.start..child_loc.end];
    ////                try latex_args.append(child_name);

    ////                try types.put(type_name, latex_args);
    ////            }
    ////        },
    ////        else => {},
    ////    }
    ////}
    ////const bench_type = bench_timer.read();

    ////bench_timer.reset();
    ////for (statements.items) |node| {
    ////    dumpExpression(&parse_state, buf, vars, types, node);
    ////}
    ////const bench_out = bench_timer.read();

    //const total_time = total_timer.read();
    //const us_per_ns = 1000;
    //std.log.info(" total: {} us", .{total_time/us_per_ns});
    //std.log.info("==============", .{});
    //std.log.info("  read: {} us", .{bench_read/us_per_ns});
    //std.log.info(" lexer: {} us", .{bench_lex/us_per_ns});
    //_ = buffer;
    ////std.log.info("parser: {} us", .{bench_parse/us_per_ns});
    ////std.log.info("  type: {} us", .{bench_type/us_per_ns});
    ////std.log.info("   out: {} us", .{bench_out/us_per_ns});
}

//fn dumpExpression(parse_state: *parser.ParseState, buf: []const u8, vars: std.StringHashMap([]const u8), types: std.StringHashMap(std.BoundedArray([]const u8, 8)), node: *parser.AstNode) void {
//    switch (node.ast_type) {
//        .var_name => {
//            const var_loc = parse_state.buffer.locations.items[node.ast_type.var_name];
//            const var_name = buf[var_loc.start..var_loc.end];
//            const type_name = vars.get(var_name);
//            if (type_name == null) {
//                std.debug.print("{s}\n", .{var_name});
//            } else {
//                const latex_args = types.get(type_name.?) orelse return;
//                const latex_string = latex_args.get(0);
//                const latex_var = latex_args.get(1);
//                var new_latex_string = parse_state.allocator.alloc(u8, latex_string.len) catch return;
//                var i: usize = 0;
//                while (i < latex_string.len) {
//                    const c = latex_string[i];
//                    if (c == '$' and
//                        i < latex_string.len - latex_var.len and
//                        std.mem.eql(u8, latex_string[(i+1)..(i+1+latex_var.len)], latex_var)) {
//                        for (var_name) |d,j| {
//                            new_latex_string[i+j] = d;
//                        }
//                        i += 1 + latex_var.len;
//                    } else {
//                        new_latex_string[i] = c;
//                        i += 1;
//                    }
//                }
//                std.debug.print("{s}, {s}, {s}, {s}\n", .{var_name, latex_string, latex_var, new_latex_string});
//            }
//        },
//        .number => {
//            const loc = parse_state.buffer.locations.items[node.ast_type.number];
//            const name = buf[loc.start..loc.end];
//            std.debug.print("{s}\n", .{name});
//        },
//        .bin_op => {
//            const loc = parse_state.buffer.locations.items[node.ast_type.bin_op];
//            const op = buf[loc.start..loc.end];
//
//            dumpExpression(parse_state, buf, vars, types, node.children.get(0));
//            std.debug.print("{s}\n", .{op});
//            dumpExpression(parse_state, buf, vars, types, node.children.get(1));
//        },
//        else => {},
//    }
//}

//
// Consumers of AST
//

//fn dumpStatementsToDotImpl(state: *parser.ParseState, writer: std.fs.File.Writer, node: *parser.AstNode) void {
//    writer.print("\"{*}\" [label=\"{s}\"]\n", .{node, @tagName(node.*.ast_type)}) catch return;
//    //const fields = @typeInfo(@TypeOf(AstType)).Union.fiels;
//    //switch (node.*.ast_type) {
//    //    inline else => {
//    //        //std.debug.print("|| {any}\n", .{@TypeOf(i)});
//    //    }
//    //    //.var_name,
//    //    //.bin_op,
//    //    //.number => |i| {
//    //    //    const loc = state.buffer.locations.items[i];
//    //    //    const name = state.filebuf[loc.start..loc.end];
//    //    //    std.debug.print("{s}\n", .{name});
//    //    //},
//    //    //.var_decl  => |i| {
//    //    //    const ti = @typeInfo(@TypeOf(i)).Struct;
//    //    //    inline for (ti.fields) |f| {
//    //    //        if (f.field_type != usize)
//    //    //            continue;
//    //    //        const index = @field(i, f.name);
//    //    //        const loc = state.buffer.locations.items[index];
//    //    //        const name = state.filebuf[loc.start..loc.end];
//    //    //        std.debug.print("{s}, {s}\n", .{f.name, name});
//    //    //    }
//    //    //},
//    //    //.type_decl => |i| {
//    //    //    const ti = @typeInfo(@TypeOf(i)).Struct;
//    //    //    inline for (ti.fields) |f| {
//    //    //        const index = @field(i, f.name);
//    //    //        const loc = state.buffer.locations.items[index];
//    //    //        const name = state.filebuf[loc.start..loc.end];
//    //    //        std.debug.print("{s}, {s}\n", .{f.name, name});
//    //    //    }
//    //    //},
//    //    //.op_decl => |i| {
//    //    //    const ti = @typeInfo(@TypeOf(i)).Struct;
//    //    //    inline for (ti.fields) |f| {
//    //    //        const index = @field(i, f.name);
//    //    //        const loc = state.buffer.locations.items[index];
//    //    //        const name = state.filebuf[loc.start..loc.end];
//    //    //        std.debug.print("{s}, {s}\n", .{f.name, name});
//    //    //    }
//    //    //},
//    //}
//    writer.writeAll("\"]\n") catch return;
//    //const ti = comptime @TypeOf(@field(node.*.ast_type, @tagName(node.*.ast_type)));
//    //std.debug.print("{}\n", .{ti});
//    //inline for (ti.fields) |t| {
//    //    if (t.field_type == usize) {
//    //        std.debug.print("{s}\n", .{t.name});
//    //    }
//    //}
//    for (node.children.constSlice()) |child| {
//        writer.print("\"{*}\" -> \"{*}\"\n", .{node, child}) catch return;
//        dumpStatementsToDotImpl(state, writer, child);
//    }
//}
//
//fn dumpStatementsToDot(state: *parser.ParseState, filename: []const u8, statements: std.ArrayList(*parser.AstNode)) !void {
//    const file = std.fs.cwd().createFile(filename, .{}) catch |err| {
//        std.log.err("Failed to open file: {s} ({})", .{filename, err});
//        return;
//    };
//    defer file.close();
//    const writer = file.writer();
//
//    try writer.writeAll("digraph {\n");
//
//    for (statements.items) |node| {
//        dumpStatementsToDotImpl(state, writer, node);
//    }
//
//    try writer.writeAll("}\n");
//}
