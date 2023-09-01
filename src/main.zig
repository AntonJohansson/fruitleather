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

    var bench_timer = try std.time.Timer.start();
    var total_timer = try std.time.Timer.start();

    bench_timer.reset();
    const input_file = args[1];
    const file = std.fs.cwd().openFile(input_file, .{}) catch |err| {
        std.log.err("Failed to open file: {s} ({})", .{ input_file, err });
        return;
    };

    const buf = file.readToEndAlloc(pa, 1024 * 1024 * 1024) catch {
        std.log.err("Failed to read file: {s}", .{input_file});
        return;
    };
    const bench_read = bench_timer.read();

    bench_timer.reset();
    var buffer = lexer.lex(pa, input_file, buf) catch |err| {
        log.err("Lexing failed ({})\n", .{err});
        return;
    };
    const bench_lex = bench_timer.read();

    //for (buffer.tokens.items) |t, j| {
    //    const loc = buffer.locations.items[j];
    //    std.debug.print("-- {} \t\t {s}\n", .{t, buf[loc.start..loc.end]});
    //}

    var arena = std.heap.ArenaAllocator.init(pa);
    defer arena.deinit();
    var parse_state = parser.ParseState{
        .buffer = &buffer,
        .allocator = arena.allocator(),
        .filebuf = buf,
        .filename = input_file,
    };

    bench_timer.reset();
    var statements = parser.parse(&parse_state) catch |err| {
        log.err("Parsing failed ({})\n", .{err});
        return;
    };
    try dumpStatementsToDot(&parse_state, "ast.dot", statements);
    const bench_parse = bench_timer.read();

    bench_timer.reset();
    const ast_types = try setAstTypes(&parse_state, &statements);
    try dumpStatementsToDot(&parse_state, "ast.typed.dot", statements);
    const bench_type = bench_timer.read();

    bench_timer.reset();
    try dumpStatementsToLatex(&parse_state, "out.tex", statements, ast_types);
    const bench_out = bench_timer.read();

    const total_time = total_timer.read();
    const us_per_ns = 1000;
    std.log.info(" total: {} us", .{total_time / us_per_ns});
    std.log.info("==============", .{});
    std.log.info("  read: {} us", .{bench_read / us_per_ns});
    std.log.info(" lexer: {} us", .{bench_lex / us_per_ns});
    std.log.info("parser: {} us", .{bench_parse / us_per_ns});
    std.log.info("  type: {} us", .{bench_type / us_per_ns});
    std.log.info("   out: {} us", .{bench_out / us_per_ns});
    std.log.info("-------------", .{});
    std.log.info("   sum: {} us", .{(bench_out + bench_type + bench_parse + bench_lex + bench_read) / us_per_ns});
}

const TypeInfo = struct {
    name: []const u8,
    latex_string: []const u8,
    var_name: []const u8,
};

const OpInfo = struct {
    name: []const u8,
    latex_string: []const u8,
    var_name_a: []const u8,
    var_name_b: []const u8,
    typeindex_a: usize,
    typeindex_b: usize,
    rettypeindex: usize,
};

const AstTypes = struct {
    types: std.StringHashMap(usize),
    vars: std.StringHashMap([]const u8),
    typearray: std.ArrayList(TypeInfo),
    oparray: std.ArrayList(OpInfo),
};

fn setNodeType(state: *parser.ParseState, node: *parser.AstNode, ast_types: *AstTypes, new_type: usize) void {
    std.debug.assert(node.typeindex == 0);
    if (node.ast_type == .var_name) {
        const var_loc = state.buffer.locations.items[node.ast_type.var_name];
        const var_name = state.filebuf[var_loc.start..var_loc.end];
        ast_types.vars.put(var_name, ast_types.typearray.items[new_type].name) catch unreachable;
        node.typeindex = new_type;
    } else {
        node.typeindex = new_type;
    }

    if (node.ast_type == .bin_op) {
        const op_loc = state.buffer.locations.items[node.ast_type.bin_op.op];
        const op_name = state.filebuf[op_loc.start..op_loc.end];
        var opinfo: ?OpInfo = null;
        for (ast_types.oparray.items) |oi| {
            if (oi.rettypeindex == new_type and std.mem.eql(u8, op_name, oi.name)) {
                opinfo = oi;
                break;
            }
        }

        if (opinfo != null) {
            if (opinfo.?.typeindex_a != 0) {
                if (node.children.get(0).typeindex == 0) {
                    setNodeType(state, node.children.get(0), ast_types, opinfo.?.typeindex_a);
                } else {
                    log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Type conflict", "Has {s}, propagating {s} from op {s}({s},{s})->{s}", .{
                        ast_types.typearray.items[node.children.get(0).typeindex].name,
                        ast_types.typearray.items[opinfo.?.typeindex_a].name,
                        op_name,
                        ast_types.typearray.items[opinfo.?.typeindex_a].name,
                        ast_types.typearray.items[opinfo.?.typeindex_b].name,
                        ast_types.typearray.items[opinfo.?.rettypeindex].name,
                    });
                }
            }

            if (opinfo.?.typeindex_b != 0) {
                if (node.children.get(1).typeindex == 0) {
                    setNodeType(state, node.children.get(1), ast_types, opinfo.?.typeindex_b);
                } else {
                    log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Type conflict", "Has {s}, propagating {s} from op {s}({s},{s})->{s}", .{
                        ast_types.typearray.items[node.children.get(1).typeindex].name,
                        ast_types.typearray.items[opinfo.?.typeindex_b].name,
                        op_name,
                        ast_types.typearray.items[opinfo.?.typeindex_a].name,
                        ast_types.typearray.items[opinfo.?.typeindex_b].name,
                        ast_types.typearray.items[opinfo.?.rettypeindex].name,
                    });
                }
            }
        }

    } else {
        //unreachable;
    }
}

fn setSubtreeTypes(state: *parser.ParseState, node: *parser.AstNode, ast_types: *AstTypes) usize {
    switch (node.ast_type) {
        .var_name => |v| {
            // Look at variable declarations
            node.typeindex = 0;
            const var_loc = state.buffer.locations.items[v];
            const var_name = state.filebuf[var_loc.start..var_loc.end];
            const type_name = ast_types.vars.get(var_name);
            if (type_name != null) {
                for (ast_types.typearray.items) |t,i| {
                    if (std.mem.eql(u8, type_name.?, t.name)) {
                        node.typeindex = i;
                        break;
                    }
                }
                // print error here
            }
        },
        .number => {
            // Number type
            node.typeindex = 1;
        },
        .bin_op => |v| {
            var ta = setSubtreeTypes(state, node.children.get(0), ast_types);
            var tb = setSubtreeTypes(state, node.children.get(1), ast_types);
            // CONTHERE: We only assign when either type is unknown, maybe print error
            // if there is a type mismatch? If this is too strong of an assumption,
            // we'll see in actual testing.
            if (state.buffer.tokens.items[v.op] == .Equal) {
                if (ta == 0) {
                    setNodeType(state, node.children.get(0), ast_types, tb);
                } else if (tb == 0) {
                    setNodeType(state, node.children.get(1), ast_types, ta);
                } else if (ta != tb) {
                    const op_loc = state.buffer.locations.items[v.op];
                    log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Type conflict", "{s} = {s}", .{
                        ast_types.typearray.items[ta].name,
                        ast_types.typearray.items[tb].name
                    });

                }

                node.typeindex = node.children.get(0).typeindex;
            } else {
                // Look at ops on types and assign accordingly

                var matching_ops = std.BoundedArray(*OpInfo,  16).init(0) catch unreachable;

                const op_loc = state.buffer.locations.items[v.op];
                const op_name = state.filebuf[op_loc.start..op_loc.end];
                for (ast_types.oparray.items) |*o| {
                    if (std.mem.eql(u8, op_name, o.name)) {
                        if ((o.typeindex_a == ta and o.typeindex_b == tb) or
                            (ta == 0 and o.typeindex_b == tb) or
                            (o.typeindex_a == ta and tb == 0)) {
                            matching_ops.append(o) catch unreachable;
                        }
                    }
                }

                if (matching_ops.len == 1) {
                    const o = matching_ops.get(0);
                    if ((ta == 0 and o.typeindex_b == tb)) {
                        setNodeType(state, node.children.get(0), ast_types, o.typeindex_a);
                    } else if ((o.typeindex_a == ta and tb == 0)) {
                        setNodeType(state, node.children.get(1), ast_types, o.typeindex_b);
                    }
                    node.typeindex = o.rettypeindex;
                } else {
                    // TODO(anjo): a bit verbose
                    for (matching_ops.constSlice()) |o| {
                        log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Multiple ops apply", "{s}({s},{s}) -> {s}", .{
                            op_name,
                            ast_types.typearray.items[o.typeindex_a].name,
                            ast_types.typearray.items[o.typeindex_b].name,
                            ast_types.typearray.items[o.rettypeindex].name
                        });
                    }
                }
            }
        },
        .unary_op => {
            // ?
            _ = setSubtreeTypes(state, node.children.get(0), ast_types);
        },
        .mat => |v| {
            // Set to matrix or vector type depending on size
            _ = setSubtreeTypes(state, node.children.get(0), ast_types);
            if (v.cols == 1 or v.rows == 1) {
                node.typeindex = 3;
            } else {
                node.typeindex = 2;
            }
        },
        .sum => {
            // Depends on type of terms and ops on this type
            _ = setSubtreeTypes(state, node.children.get(1), ast_types);
        },
        .prod => {
            // Depends on type of factor and ops on this type
            _ = setSubtreeTypes(state, node.children.get(1), ast_types);
        },
        .int => {
            // Depends on type of factor and ops on this type
            _ = setSubtreeTypes(state, node.children.get(1), ast_types);
        },
        .call_op => {
            // ?
            _ = setSubtreeTypes(state, node.children.get(0), ast_types);
        },
        else => unreachable,
    }
    return node.typeindex;
}

fn setAstTypes(state: *parser.ParseState, statements: *std.ArrayList(*parser.AstNode)) !AstTypes {
    var ast_types = AstTypes {
        .types = std.StringHashMap(usize).init(state.allocator),
        .vars = std.StringHashMap([]const u8).init(state.allocator),
        .typearray = std.ArrayList(TypeInfo).init(state.allocator),
        .oparray = std.ArrayList(OpInfo).init(state.allocator),
    };

    try ast_types.typearray.append(TypeInfo{ .name = "Unknown", .latex_string = "", .var_name = "" });
    try ast_types.typearray.append(TypeInfo{ .name = "Number", .latex_string = "", .var_name = ""});
    try ast_types.typearray.append(TypeInfo{ .name = "Matrix", .latex_string = "\\mathrm{\\mathbf{$x}}", .var_name = "x"});
    try ast_types.typearray.append(TypeInfo {.name = "Vector",  .latex_string = "\\vec{$x}", .var_name = "x"});

    try ast_types.types.put("Unknown", 0);
    try ast_types.types.put("Number", 1);
    try ast_types.types.put("Matrix", 2);
    try ast_types.types.put("Vector",  3);

    var si: usize = 0;
    while (si < statements.items.len) {
        const parent = statements.items[si];
        if (parent.ast_type != .code) {
            si += 1;
            continue;
        }
        var sj: usize = 0;
        while (sj < parent.children.len) {
            const node = parent.children.get(sj);
            switch (node.ast_type) {
                .var_decl => {
                    const var_loc = state.buffer.locations.items[node.ast_type.var_decl.var_name];
                    const type_loc = state.buffer.locations.items[node.ast_type.var_decl.type_name];
                    const type_name = state.filebuf[type_loc.start..type_loc.end];
                    const var_name = state.filebuf[var_loc.start..var_loc.end];

                    var has_error = false;
                    if (ast_types.vars.contains(var_name)) {
                        log.errAt(state.filename, state.filebuf, var_loc.start, "Invalid variable declaration", "Repeat variable declaration");
                        has_error = true;
                    }
                    if (!ast_types.types.contains(type_name)) {
                        log.errAt(state.filename, state.filebuf, type_loc.start, "Invalid variable declaration", "Undeclared type");
                        has_error = true;
                    }

                    if (!has_error) {
                        try ast_types.vars.put(var_name, type_name);
                        _ = parent.children.orderedRemove(sj);
                    } else {
                        sj += 1;
                    }
                },
                .type_decl => {
                    const type_loc = state.buffer.locations.items[node.ast_type.type_decl.type_name];
                    const latex_loc = state.buffer.locations.items[node.ast_type.type_decl.latex_string];
                    const type_name = state.filebuf[type_loc.start..type_loc.end];
                    const latex_string = state.filebuf[latex_loc.start..latex_loc.end];

                    if (ast_types.types.contains(type_name)) {
                        log.errAt(state.filename, state.filebuf, type_loc.start, "Invalid type declaration", "Repeat type declaration");
                        sj += 1;
                    } else {
                        var child: *parser.AstNode = node.children.get(0);
                        const child_loc = state.buffer.locations.items[child.ast_type.var_name];
                        const child_name = state.filebuf[child_loc.start..child_loc.end];

                        const typeindex = ast_types.typearray.items.len;
                        try ast_types.typearray.append(TypeInfo {
                            .name = type_name,
                            .latex_string = latex_string,
                            .var_name = child_name,
                        });
                        try ast_types.types.put(type_name, typeindex);
                        _ = parent.children.orderedRemove(sj);
                    }
                },
                .op_decl => {
                    const op_loc = state.buffer.locations.items[node.ast_type.op_decl.op];
                    const return_type_loc = state.buffer.locations.items[node.ast_type.op_decl.return_type];
                    const latex_loc = state.buffer.locations.items[node.ast_type.op_decl.latex_string];

                    const op_name = state.filebuf[op_loc.start..op_loc.end];
                    const return_typename = state.filebuf[return_type_loc.start..return_type_loc.end];
                    const tr = ast_types.types.get(return_typename) orelse blk: {
                        log.errAt(state.filename, state.filebuf, return_type_loc.start, "Parsing op decl.", "Invalid return type, definition not found");
                        break :blk 0;
                    };

                    const latex_string = state.filebuf[latex_loc.start..latex_loc.end];

                    // TODO: don't hardode 2 args
                    const varname_a_loc  = state.buffer.locations.items[node.children.get(0).children.get(0).ast_type.var_decl.var_name];
                    const typename_a_loc = state.buffer.locations.items[node.children.get(0).children.get(0).ast_type.var_decl.type_name];
                    const varname_b_loc  = state.buffer.locations.items[node.children.get(0).children.get(1).ast_type.var_decl.var_name];
                    const typename_b_loc = state.buffer.locations.items[node.children.get(0).children.get(1).ast_type.var_decl.type_name];
                    const varname_a  = state.filebuf[varname_a_loc.start..varname_a_loc.end];
                    const typename_a = state.filebuf[typename_a_loc.start..typename_a_loc.end];
                    const varname_b  = state.filebuf[varname_b_loc.start..varname_b_loc.end];
                    const typename_b = state.filebuf[typename_b_loc.start..typename_b_loc.end];
                    var ta: usize = ast_types.types.get(typename_a) orelse 0;
                    var tb: usize = ast_types.types.get(typename_b) orelse 0;
                    try ast_types.oparray.append(OpInfo {
                        .name = op_name,
                        .latex_string = latex_string,
                        .var_name_a = varname_a,
                        .var_name_b = varname_b,
                        .typeindex_a = ta,
                        .typeindex_b = tb,
                        .rettypeindex = tr,
                    });
                    _ = parent.children.orderedRemove(sj);
                },
                else => {
                    _ = setSubtreeTypes(state, node, &ast_types);
                    sj += 1;
                },
            }
        }

        if (parent.children.len == 0) {
            _ = statements.orderedRemove(si);
        } else {
            si += 1;
        }
    }
    return ast_types;
}

fn dumpStatementsToLatex(state: *parser.ParseState, filename: []const u8, statements: std.ArrayList(*parser.AstNode), ast_types: AstTypes) !void {
    const file = std.fs.cwd().createFile(filename, .{})  catch |err|  {
        std.log.err("Failed to open file: {s} ({})", .{filename, err});
        return;
    };
    defer file.close();
    const writer = file.writer();

    try writer.writeAll("\\documentclass[a4paper,12pt]{article}\n");
    try writer.writeAll("\n");
    try writer.writeAll("%http://mirrors.ibiblio.org/CTAN/macros/latex/contrib/physics/physics.pdf\n");
    try writer.writeAll("\n");
    try writer.writeAll("\\usepackage{physics, amsmath, amssymb, amsthm, mathtools}\n");
    try writer.writeAll("\n");
    try writer.writeAll("\\usepackage[T1]{fontenc}\n");
    try writer.writeAll("\\usepackage[a4paper,left=2.5cm,right=2.5cm,top=2.5cm,bottom=2.5cm]{geometry}\n");
    try writer.writeAll("\\usepackage{etoolbox}\n");
    try writer.writeAll("\\usepackage{tikz-cd}\n");
    try writer.writeAll("\n");
    try writer.writeAll("\\def\\QED{\\hfill$\\square$}\n");
    try writer.writeAll("\n");
    try writer.writeAll("\\newcommand{\\eqv}{\\Leftrightarrow}\n");
    try writer.writeAll("\\newcommand{\\imp}{\\Rightarrow}\n");
    try writer.writeAll("\\newcommand{\\limp}{\\Leftarrow}\n");
    try writer.writeAll("\\newcommand{\\opr}[1]{\\hat{#1}}\n");
    try writer.writeAll("\\newcommand{\\lpoly}[2]{\\ell_{#1}^{(#2)}}\n");
    try writer.writeAll("\\newcommand{\\End}{\\mathrm{End}\\,}\n");
    try writer.writeAll("\\newcommand{\\Span}{\\mathrm{span}\\,}\n");
    try writer.writeAll("\\newcommand{\\dc}[1]{\\mathrm{<}#1\\mathrm{>}}\n");
    try writer.writeAll("\n");
    try writer.writeAll("\\begin{document}");

    try writer.writeAll("\n");

    for (statements.items) |node| {
        try dumpExpression(state, writer, state.filebuf, ast_types, node, false, true);
    }

    try writer.writeAll("\\end{document}\n");
}

fn mapVarName(name: []const u8) []const u8 {
    if      (std.mem.eql(u8, name, "alpha")) {
        return "\\alpha";
    } else if (std.mem.eql(u8, name, "beta")) {
        return "\\beta";
    } else if (std.mem.eql(u8, name, "gamma")) {
        return "\\gamma";
    } else if (std.mem.eql(u8, name, "delta")) {
        return "\\delta";
    } else if (std.mem.eql(u8, name, "epsilon")) {
        return "\\epsilon";
    } else if (std.mem.eql(u8, name, "zeta")) {
        return "\\zeta";
    } else if (std.mem.eql(u8, name, "eta")) {
        return "\\eta";
    } else if (std.mem.eql(u8, name, "theta")) {
        return "\\theta";
    } else if (std.mem.eql(u8, name, "iota")) {
        return "\\iota";
    } else if (std.mem.eql(u8, name, "kappa")) {
        return "\\kappa";
    } else if (std.mem.eql(u8, name, "lambda")) {
        return "\\lambda";
    } else if (std.mem.eql(u8, name, "mu")) {
        return "\\mu";
    } else if (std.mem.eql(u8, name, "nu")) {
        return "\\nu";
    } else if (std.mem.eql(u8, name, "xi")) {
        return "\\xi";
    } else if (std.mem.eql(u8, name, "omicro")) {
        return "\\omicon";
    } else if (std.mem.eql(u8, name, "pi")) {
        return "\\pi";
    } else if (std.mem.eql(u8, name, "sigma")) {
        return "\\sigma";
    } else if (std.mem.eql(u8, name, "tau")) {
        return "\\tau";
    } else if (std.mem.eql(u8, name, "upsilon")) {
        return "\\upsilon";
    } else if (std.mem.eql(u8, name, "chi")) {
        return "\\chi";
    } else if (std.mem.eql(u8, name, "phi")) {
        return "\\phi";
    } else if (std.mem.eql(u8, name, "psi")) {
        return "\\psi";
    } else if (std.mem.eql(u8, name, "omega")) {
        return "\\omega";
    } else if (std.mem.eql(u8, name, "Alpha")) {
        return "\\Alpha";
    } else if (std.mem.eql(u8, name, "Beta")){
        return "\\Beta";
    } else if (std.mem.eql(u8, name, "Gamma")) {
        return "\\Gamma";
    } else if (std.mem.eql(u8, name, "Delta")) {
        return "\\Delta";
    } else if (std.mem.eql(u8, name, "Epsilon")) {
        return "\\Epsilon";
    } else if (std.mem.eql(u8, name, "Zeta")) {
        return "\\Zeta";
    } else if (std.mem.eql(u8, name, "Eta")) {
        return "\\Eta";
    } else if (std.mem.eql(u8, name, "Theta")) {
        return "\\Theta";
    } else if (std.mem.eql(u8, name, "Iota")) {
        return "\\Iota";
    } else if (std.mem.eql(u8, name, "Kappa")) {
        return "\\Kappa";
    } else if (std.mem.eql(u8, name, "Lambda")) {
        return "\\Lambda";
    } else if (std.mem.eql(u8, name, "Mu")) {
        return "\\Mu";
    } else if (std.mem.eql(u8, name, "Nu")) {
        return "\\Nu";
    } else if (std.mem.eql(u8, name, "Xi")) {
        return "\\Xi";
    } else if (std.mem.eql(u8, name, "Omicron")) {
        return "\\Omicon";
    } else if (std.mem.eql(u8, name, "Pi")) {
        return "\\Pi";
    } else if (std.mem.eql(u8, name, "Sigma")) {
        return "\\Sigma";
    } else if (std.mem.eql(u8, name, "Tau")) {
        return "\\Tau";
    } else if (std.mem.eql(u8, name, "Upsilon")) {
        return "\\Upsilon";
    } else if (std.mem.eql(u8, name, "Chi")) {
        return "\\Chi";
    } else if (std.mem.eql(u8, name, "Phi")) {
        return "\\Phi";
    } else if (std.mem.eql(u8, name, "Psi")) {
        return "\\Psi";
    } else if (std.mem.eql(u8, name, "Omega")) {
        return "\\Omega";
    } else {return name;}
}

fn precendence() void {
    // a * (b + c)
    // a / (b + c)
    // a * (-c)
    // (-a) * (-c)
    // -a / (b + c)
    // (-a)^b

    // a ^ b
    //  - a is not a var_name
    //  - a is not a number
    //  - a is not a matrix

    // a * b
    //  - a | b bin_op with lower presedence
    //  - b unary op with negative value

    // a / b
    //  - a | b bin_op with lower presedence

}

fn dumpExpression(parse_state: *parser.ParseState, writer: std.fs.File.Writer, buf: []const u8, ast_types: AstTypes, node: *parser.AstNode, in_mat: bool, allow_in_parens: bool) std.fs.File.Writer.Error!void {
    if (node.ast_type != .bin_op) {
        if (node.has_newline)
            try writer.writeAll(" \\\\\n\t ");
        if (node.has_align)
            try writer.writeAll(" & ");
    }

    if (node.bracketed and allow_in_parens) {
        try writer.writeAll("(");
    }

    switch (node.ast_type) {
        .header => {
            switch (node.ast_type.header.depth) {
                1 => try writer.writeAll("\\section{"),
                2 => try writer.writeAll("\\subsection{"),
                3 => try writer.writeAll("\\subsubsection{"),
                else => try writer.writeAll("\\subsubsection{"),
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), in_mat, allow_in_parens);
            try writer.writeAll("}\n");
        },
        .code => {
            if (node.ast_type.code.small) {
                try writer.writeAll("$");
            } else {
                try writer.writeAll("\n");
                try writer.writeAll("\\begin{align*}\n");
                try writer.writeAll("\t");
            }
            for (node.children.constSlice()) |child,i| {
                try dumpExpression(parse_state, writer, buf, ast_types, child, in_mat, allow_in_parens);
                if (node.children.len > 1 and i < node.children.len-1)
                    try writer.writeAll("\\\\\n\t");
            }
            if (node.ast_type.code.small) {
                try writer.writeAll("$");
            } else {
                try writer.writeAll("\n");
                try writer.writeAll("\\end{align*}\n");
            }
        },
        .text => {
            const text_loc = parse_state.buffer.locations.items[node.ast_type.text];
            const text = buf[text_loc.start..text_loc.end];
            try writer.print("{s}", .{text});
        },
        .var_name => {
            const var_loc = parse_state.buffer.locations.items[node.ast_type.var_name];
            const var_name = mapVarName(buf[var_loc.start..var_loc.end]);
            const typeindex = node.typeindex;
            if (typeindex == 0) {
                try writer.print("{s}", .{var_name});
            } else {
                const typeinfo = ast_types.typearray.items[typeindex];
                const latex_string = typeinfo.latex_string;
                if (latex_string.len > 0) {
                    const latex_var = typeinfo.var_name;
                    var new_latex_string = parse_state.allocator.alloc(u8, 4*latex_string.len) catch return;
                    var i: usize = 0;
                    var k: usize = 0;
                    while (i < latex_string.len) {
                        const c = latex_string[i];
                        if (c == '$' and
                            i < latex_string.len - latex_var.len and

                            std.mem.eql(u8, latex_string[(i+1)..(i+1+latex_var.len)], latex_var)) {
                            for (var_name) |d,j| {
                                new_latex_string[k+j] = d;
                            }
                            k += var_name.len;
                            i += 1 + latex_var.len;
                        } else {
                            new_latex_string[k] = c;
                            k += 1;
                            i += 1;
                        }
                    }
                    new_latex_string.len = k;
                    try writer.print("{s}", .{new_latex_string});
                } else {
                    try writer.print("{s}", .{var_name});
                }
            }
        },
        .number => {
            const loc = parse_state.buffer.locations.items[node.ast_type.number];
            const name = buf[loc.start..loc.end];
            try writer.print("{s}", .{name});
        },
        .bin_op => |v| {
            const typeindex = node.typeindex;
            const childa = node.children.get(0);
            const childb = node.children.get(1);
            const ta = childa.typeindex;
            const tb = childb.typeindex;

            switch (parse_state.buffer.tokens.items[node.ast_type.bin_op.op]) {
                .Div => try writer.writeAll("\\frac{"),
                else   => {},
            }

            var left_allow_in_parens = allow_in_parens;
            switch (parse_state.buffer.tokens.items[node.ast_type.bin_op.op]) {
                .Div => {left_allow_in_parens = false;},
                else => {},
            }

            try dumpExpression(parse_state, writer, buf, ast_types, childa, in_mat, left_allow_in_parens);

            if (node.has_newline)
                try writer.writeAll(" \\\\\n\t ");
            if (node.has_align)
                try writer.writeAll(" & ");

            const op_loc = parse_state.buffer.locations.items[v.op];
            const op_name = parse_state.filebuf[op_loc.start..op_loc.end];
            var opinfo: ?OpInfo = null;
            for (ast_types.oparray.items) |o| {
                if (std.mem.eql(u8, op_name, o.name) and o.typeindex_a == ta and o.typeindex_b == tb) {
                    opinfo = o;
                    break;
                }
            }

            if (opinfo != null and opinfo.?.latex_string.len > 0) {
                try writer.writeAll(" ");
                try writer.writeAll(opinfo.?.latex_string);
                try writer.writeAll(" ");
            } else {
                switch (parse_state.buffer.tokens.items[node.ast_type.bin_op.op]) {
                    .Add => try writer.writeAll(" + "),
                    .Sub => try writer.writeAll(" - "),
                    .Mul         => {
                        if (typeindex == 1) {
                            try writer.writeAll(" \\cdot ");
                        } else {
                            try writer.writeAll(" ");
                        }
                    },
                    .Div         => {
                        try writer.writeAll("}{");
                    },
                    .Equal       => try writer.writeAll(" = "),
                    .Superscript => try writer.writeAll("^{"),
                    .Subscript => try writer.writeAll("_{"),
                    .LeftImp  => try writer.writeAll("\\Leftarrow"),
                    .RightImp => try writer.writeAll("\\Rightarrow"),
                    .Eqv         => try writer.writeAll("\\Leftrightarrow"),
                    .LeftAngleBracket  => try writer.writeAll(" < "),
                    .RightAngleBracket => try writer.writeAll(" > "),
                    .LessThanEqual    => try writer.writeAll(" \\leq "),
                    .GreaterThanEqual  => try writer.writeAll(" \\geq "),
                    .Comma       => if (in_mat) {
                        try writer.writeAll(" & ");
                    } else {
                        try writer.writeAll(", ");
                    },
                    .In        => try writer.writeAll(" \\in "),
                    .Semicolon => try writer.writeAll(" \\\\ "),
                    else         => return,
                }
            }

            var right_allow_in_parens = allow_in_parens;
            switch (parse_state.buffer.tokens.items[node.ast_type.bin_op.op]) {
                .Div => {right_allow_in_parens = false;},
                .Superscript => {right_allow_in_parens = false;},
                else => {},
            }

            try dumpExpression(parse_state, writer, buf, ast_types, childb, in_mat, right_allow_in_parens);

            switch (parse_state.buffer.tokens.items[node.ast_type.bin_op.op]) {
                .Superscript => try writer.writeAll("}"),
                .Subscript => try writer.writeAll("}"),
                .Div       => try writer.writeAll("}"),
                else   => {},
            }
        },
        .unary_op => {
            const loc = parse_state.buffer.locations.items[node.ast_type.unary_op];
            const op = buf[loc.start..loc.end];

            try writer.print("{s}", .{op});
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), in_mat, allow_in_parens);
        },
        .mat => |v| {
            if (node.typeindex == 2) {
                switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                    .LeftBracket => {
                        try writer.writeAll("\\mqty(");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), true, allow_in_parens);
                        try writer.writeAll(")");
                    },
                    .LeftAngleBracket => {
                        try writer.writeAll("\\left<");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                        try writer.writeAll("\\right>");
                    },
                    .LeftBrace => {
                        try writer.writeAll("\\Big\\{");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                        try writer.writeAll("\\Big\\}");
                    },
                    else => return,
                }
            } else if (node.typeindex == 3) {
                if (v.cols > 1) {
                    switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                        .LeftBracket => {
                            try writer.writeAll("\\left(");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                            try writer.writeAll("\\right)");
                        },
                        .LeftAngleBracket => {
                            try writer.writeAll("\\left<");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                            try writer.writeAll("\\right>");
                        },
                        .LeftBrace => {
                            try writer.writeAll("\\Big\\{");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                            try writer.writeAll("\\Big\\}");
                        },
                        else => return,
                    }
                } else {
                    switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                        .LeftBracket => {
                            try writer.writeAll("\\mqty(");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), true, allow_in_parens);
                            try writer.writeAll(")");
                        },
                        .LeftAngleBracket => {
                            try writer.writeAll("\\left<");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                            try writer.writeAll("\\right>");
                        },
                        .LeftBrace => {
                            try writer.writeAll("\\Big\\{");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), false, allow_in_parens);
                            try writer.writeAll("\\Big\\}");
                        },
                        else => return,
                    }
                }
            } else {
                unreachable;
            }
        },
        .sum => {
            const num_args = node.children.get(0).ast_type.bin_op.num_args_in_subtree;
            try writer.writeAll("\\sum_{");
            if (num_args == 2) {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0), in_mat, allow_in_parens);
                try writer.writeAll(" \\in ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            } else {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0).children.get(0), in_mat, allow_in_parens);
                try writer.writeAll(" = ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("}");
                try writer.writeAll("^{");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(1), in_mat, allow_in_parens);
        },
        .prod => {
            const num_args = node.children.get(0).ast_type.bin_op.num_args_in_subtree;
            try writer.writeAll("\\prod_{");
            if (num_args == 2) {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0), in_mat, allow_in_parens);
                try writer.writeAll(" \\in ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            } else {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0).children.get(0), in_mat, allow_in_parens);
                try writer.writeAll(" = ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("}");
                try writer.writeAll("^{");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(1), in_mat, allow_in_parens);
        },
        .int => |v| {
            std.debug.print("{}\n", .{node.children.get(0).ast_type});
            const num_args = if (node.children.get(0).ast_type == .bin_op)
                node.children.get(0).ast_type.bin_op.num_args_in_subtree
            else                1;


            const token = parse_state.buffer.tokens.items[v];
            if      (token == .Int1) {
                try writer.writeAll("\\int_{");
            } else if (token == .Int2) {
                try writer.writeAll("\\iint_{");
            } else if (token == .Int3)  {
                try writer.writeAll("\\iiint_{");
            } else if (token == .Oint1) {
                try writer.writeAll("\\oint_{");
            } else if (token == .Oint2) {
                try writer.writeAll("\\oiint_{");
            } else if (token == .Oint3) {
                try writer.writeAll("\\oiiint_{");
            } else {unreachable;}

            if (num_args == 1) {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            } else {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(0), in_mat, allow_in_parens);
                try writer.writeAll("}^{");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0).children.get(1), in_mat, allow_in_parens);
                try writer.writeAll("} ");
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(1), in_mat, allow_in_parens);
        },
        .call_op => {
            const loc = parse_state.buffer.locations.items[node.ast_type.call_op];
            const op = buf[loc.start..loc.end];

            if (op.len > 1) {
                try writer.print("\\mathrm{{{s}}}(", .{op});
            } else {
                try writer.print("{s}(", .{op});
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.get(0), in_mat, allow_in_parens);
            try writer.writeAll(")");
        },
        else => {},
    }

    if (node.bracketed and allow_in_parens) {
        try writer.writeAll(")");
    }
}

//
// Consumers of AST
//

fn dumpStatementsToDotImpl(state: *parser.ParseState, writer: std.fs.File.Writer, node: *parser.AstNode) void {
    writer.print("\"{*}\" [fontcolor=\"white\",color=\"white\",label=\"", .{node}) catch return;
    // TODO(anjo): In more recent version of zig we should be able to generate
    // all cases at compile time using inline else => |i| {}.
    switch (node.*.ast_type) {
        .header => {
            writer.writeAll("header") catch return;
        },
        .code => {
            writer.writeAll("code") catch return;
        },
        .var_name, .call_op, .unary_op, .sum, .prod, .int, .text,
        .number => |i| {
            const loc = state.buffer.locations.items[i];
            const name = state.filebuf[loc.start..loc.end];
            writer.print("{s}", .{name}) catch return;
        },
        .bin_op => |i| {
            const loc = state.buffer.locations.items[i.op];
            const name = state.filebuf[loc.start..loc.end];
            writer.print("{s}", .{name}) catch return;
        },
        .var_decl  => |i| {
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields) |f,j| {
                if (f.field_type != usize)
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
            inline for (ti.fields) |f, j| {
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
            inline for (ti.fields) |f, j| {
                const index = @field(i, f.name);
                const loc = state.buffer.locations.items[index];
                const name = state.filebuf[loc.start..loc.end];
                if (j > 0)
                    writer.writeAll("\\n") catch return;
                writer.print("{s}", .{name}) catch return;
            }
        },
        .mat => |i| {
            writer.writeAll("Matrix\n") catch return;
            const ti = @typeInfo(@TypeOf(i)).Struct;
            inline for (ti.fields) |f, j| {
                if (j > 0)
                    writer.writeAll("\\n") catch return;
                if (f.field_type == usize) {
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

    for (node.children.constSlice()) |child| {
        writer.print("\"{*}\" -> \"{*}\" [color=\"white\"]\n", .{node, child}) catch return;
        dumpStatementsToDotImpl(state, writer, child);
    }
}

fn dumpStatementsToDot(state: *parser.ParseState, filename: []const u8, statements: std.ArrayList(*parser.AstNode)) !void {
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
