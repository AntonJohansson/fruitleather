const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const typer = @import("typer.zig");
const log = @import("log.zig");

const ParseState = parser.ParseState;
const AstNode = parser.AstNode;
const AstTypes = typer.AstTypes;

pub fn emit(state: *ParseState, filename: []const u8, statements: std.ArrayList(*AstNode), ast_types: AstTypes) !void {
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
    try writer.writeAll("\\theoremstyle{theorem}");
    try writer.writeAll("\\newtheorem{theorem}{Thm}[section]");
    try writer.writeAll("\\newtheoremstyle{Def}{1em}{1em}{}{}{\\bfseries}{.}{.5em}{}");
    try writer.writeAll("\\theoremstyle{Def}");
    try writer.writeAll("\\newtheorem{Def}{Def}[section]");
    try writer.writeAll("\\theoremstyle{Def}");
    try writer.writeAll("\\newtheorem{Ex}{Ex}[section]");
    try writer.writeAll("\\theoremstyle{corollary}");
    try writer.writeAll("\\newtheorem{corollary}{Cor}[theorem]");
    try writer.writeAll("\\theoremstyle{lemma}");
    try writer.writeAll("\\newtheorem{lemma}{Lem}[theorem]");
    try writer.writeAll("\\theoremstyle{remark}");
    try writer.writeAll("\\newtheorem*{remark}{Rem}");
    try writer.writeAll("\n");
    try writer.writeAll("\\begin{document}");

    try writer.writeAll("\n");

    for (statements.items) |node| {
        try dumpExpression(state, writer, state.filebuf, ast_types, node, false, true);
    }

    try writer.writeAll("\n");
    try writer.writeAll("\\end{document}\n");
}

fn mapVarName(name: []const u8) []const u8 {
    // TODO: string switch
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

fn dumpExpression(parse_state: *ParseState, writer: std.fs.File.Writer, buf: []const u8, ast_types: AstTypes, node: *AstNode, in_mat: bool, allow_in_parens: bool) std.fs.File.WriteError!void {
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
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], in_mat, allow_in_parens);
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
            for (node.children.items, 0..) |child,i| {
                try dumpExpression(parse_state, writer, buf, ast_types, child, in_mat, allow_in_parens);
                if (node.children.items.len > 1 and i < node.children.items.len-1)
                    try writer.writeAll("\\\\\n\t");
            }
            if (node.ast_type.code.small) {
                try writer.writeAll("$");
                try writer.writeAll(" ");
            } else {
                try writer.writeAll("\n");
                try writer.writeAll("\\end{align*}\n");
            }
        },
        .text => {
            const text_loc = parse_state.buffer.locations.items[node.ast_type.text];
            const text = strip(buf[text_loc.start..text_loc.end]);
            try writer.print("{s}", .{text});
        },
        .var_name => {
            const var_loc = parse_state.loc(node.ast_type.var_name);
            const var_name: []const u8 = mapVarName(buf[var_loc.start..var_loc.end]);
            const typeindex = node.typeindex;
            if (typeindex == 0) {
                try writer.print("{s}", .{var_name});
            } else {
                const typeinfo = ast_types.typearray.items[typeindex];
                const latex_string = typeinfo.latex_string;
                if (latex_string.len > 0) {
                    var vars: [1][]const u8 = undefined;
                    vars[0] = var_name;
                    const new_latex_string = replaceVarsInLatexString(parse_state.allocator, latex_string, &vars) catch unreachable;
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
            const childa = node.children.items[0];
            const childb = node.children.items[1];

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

            const opinfo = ast_types.ops.get(v.op);

            if (opinfo != null and opinfo.?.latex_string.len > 0) {
                try writer.writeAll(" ");
                try writer.writeAll(opinfo.?.latex_string);
                try writer.writeAll(" ");
            } else {
                switch (parse_state.tok(node.ast_type.bin_op.op)) {
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
                    .To  => try writer.writeAll(" \\rightarrow "),
                    .LeftImp  => try writer.writeAll(" \\Leftarrow "),
                    .RightImp => try writer.writeAll(" \\Rightarrow "),
                    .Eqv         => try writer.writeAll(" \\Leftrightarrow "),
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
                    else         => unreachable,
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
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], in_mat, allow_in_parens);
        },
        .mat => |v| {
            if (node.typeindex == 2) {
                switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                    .LeftBracket => {
                        try writer.writeAll("\\mqty(");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], true, allow_in_parens);
                        try writer.writeAll(")");
                    },
                    .LeftAngleBracket => {
                        try writer.writeAll("\\left<");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                        try writer.writeAll("\\right>");
                    },
                    .LeftBrace => {
                        try writer.writeAll("\\Big\\{");
                        try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                        try writer.writeAll("\\Big\\}");
                    },
                    else => return,
                }
            } else if (node.typeindex == 3) {
                if (v.cols > 1) {
                    switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                        .LeftBracket => {
                            try writer.writeAll("\\left(");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                            try writer.writeAll("\\right)");
                        },
                        .LeftAngleBracket => {
                            try writer.writeAll("\\left<");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                            try writer.writeAll("\\right>");
                        },
                        .LeftBrace => {
                            try writer.writeAll("\\Big\\{");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                            try writer.writeAll("\\Big\\}");
                        },
                        else => return,
                    }
                } else {
                    switch (parse_state.buffer.tokens.items[node.ast_type.mat.bracket]) {
                        .LeftBracket => {
                            try writer.writeAll("\\mqty(");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], true, allow_in_parens);
                            try writer.writeAll(")");
                        },
                        .LeftAngleBracket => {
                            try writer.writeAll("\\left<");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
                            try writer.writeAll("\\right>");
                        },
                        .LeftBrace => {
                            try writer.writeAll("\\Big\\{");
                            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], false, allow_in_parens);
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
            const num_args = node.children.items[0].ast_type.bin_op.num_args_in_subtree;
            try writer.writeAll("\\sum_{");
            if (num_args == 2) {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(" \\in ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("} ");
            } else {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0].children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(" = ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("}");
                try writer.writeAll("^{");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("} ");
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[1], in_mat, allow_in_parens);
        },
        .prod => {
            const num_args = node.children.items[0].ast_type.bin_op.num_args_in_subtree;
            try writer.writeAll("\\prod_{");
            if (num_args == 2) {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(" \\in ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("} ");
            } else {
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0].children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(" = ");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("}");
                try writer.writeAll("^{");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0].children.items[1], in_mat, allow_in_parens);
                try writer.writeAll("} ");
            }
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[1], in_mat, allow_in_parens);
        },
        .call_op => |v| {
            const op = parse_state.name(parse_state.loc(v));

            const opinfo = ast_types.ops.get(v);

            if (opinfo != null and opinfo.?.latex_string.len > 0) {
                const node_args = node.children.items[0];
                const num_args = if (node_args.ast_type == .bin_op and parse_state.tok(node_args.ast_type.bin_op.op) == .Comma) node_args.ast_type.bin_op.num_args_in_subtree else 1;
                var flat_nodes = parse_state.allocator.alloc(*parser.AstNode, num_args) catch unreachable;
                var flat_index: usize = 0;
                parser.flattenBinOpSubtree(parse_state, flat_nodes, &flat_index, node_args);
                defer parse_state.allocator.free(flat_nodes);

                dumpExpressionFromLatexString(parse_state, writer, opinfo.?.latex_string, buf, ast_types, flat_nodes, in_mat, allow_in_parens) catch unreachable;
            } else if (op.len > 1) {
                try writer.print("\\mathrm{{{s}}}(", .{op});
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(")");
            } else {
                try writer.print("{s}(", .{op});
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[0], in_mat, allow_in_parens);
                try writer.writeAll(")");
            }
        },
        .intrin => {
        },
        .thm => {
            try writer.writeAll("\n");
            try writer.writeAll("\\begin{theorem}\n");

            const has_title = node.children.items.len == 3;
            var i: usize = 0;

            try writer.writeAll("\\label{thm:");
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
            try writer.writeAll("}\n");
            i += 1;

            if (has_title) {
                try writer.writeAll("(");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
                try writer.writeAll(")");
                try writer.writeAll(" ");
                i += 1;
            }

            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);

            try writer.writeAll("\n");
            try writer.writeAll("\\end{theorem}\n");
        },
        .pf => {
            try writer.writeAll("\n");
            try writer.writeAll("\\begin{proof}\n");

            const has_title = node.children.items.len == 3;
            var i: usize = 0;

            try writer.writeAll("\\label{pf:");
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
            try writer.writeAll("}\n");
            i += 1;

            if (has_title) {
                try writer.writeAll("(");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
                try writer.writeAll(")");
                try writer.writeAll(" ");
                i += 1;
            }

            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);

            try writer.writeAll("\n");
            try writer.writeAll("\\end{proof}\n");
        },
        .def => {
            try writer.writeAll("\n");
            try writer.writeAll("\\begin{Def}\n");

            const has_title = node.children.items.len == 3;
            var i: usize = 0;

            try writer.writeAll("\\label{def:");
            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
            try writer.writeAll("}\n");
            i += 1;

            if (has_title) {
                try writer.writeAll("(");
                try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);
                try writer.writeAll(")");
                try writer.writeAll(" ");
                i += 1;
            }

            try dumpExpression(parse_state, writer, buf, ast_types, node.children.items[i], in_mat, allow_in_parens);

            try writer.writeAll("\n");
            try writer.writeAll("\\end{Def}\n");
        },
        .block => {
            for (node.children.items) |c| {
                try dumpExpression(parse_state, writer, buf, ast_types, c, in_mat, allow_in_parens);
            }
        },
        else => {},
    }

    if (node.bracketed and allow_in_parens) {
        try writer.writeAll(")");
    }
}

fn replaceVarsInLatexString(allocator: std.mem.Allocator, latex_string: []const u8, var_names: [][]const u8) VarReplaceError![]const u8 {
    var new_latex_string = allocator.alloc(u8, 4*latex_string.len) catch {
        return VarReplaceError.OutOfMemory;
    };
    var i: usize = 0;
    var k: usize = 0;
    while (i < latex_string.len) {
        const c = latex_string[i];
        if (c == '$' and i < latex_string.len - 3) {
            if (latex_string[i+1] != '{') {
                return VarReplaceError.InvalidFormat;
            }

            const s = std.mem.indexOfSentinel(u8, '}', @as([*:'}']const u8, @ptrCast(latex_string[i+2..])));
            if (latex_string[i+2+s] != '}') {
                return VarReplaceError.InvalidFormat;
            }

            const index = std.fmt.parseInt(u8, latex_string[i+2..i+2+s], 0) catch {
                return VarReplaceError.InvalidFormat;
            };
            if (index >= var_names.len) {
                return VarReplaceError.IndexOutOfRange;
            }
            const var_name = var_names[index];
            for (var_name, 0..) |d,j| {
                new_latex_string[k+j] = d;
            }
            k += var_name.len;
            i = i+2+s+1;
        } else {
            new_latex_string[k] = c;
            k += 1;
            i += 1;
        }
    }
    new_latex_string.len = k;
    return new_latex_string;
}

const VarReplaceError = error {
    IndexOutOfRange,
    InvalidFormat,
    OutOfMemory,
};

fn dumpExpressionFromLatexString(parse_state: *parser.ParseState, writer: std.fs.File.Writer, latex_string: []const u8, buf: []const u8, ast_types: AstTypes, nodes: []*parser.AstNode, in_mat: bool, allow_in_parens: bool) !void {
    var i: usize = 0;
    while (i < latex_string.len) {
        const c = latex_string[i];
        if (c == '$' and i < latex_string.len - 3) {
            if (latex_string[i+1] != '{') {
                return VarReplaceError.InvalidFormat;
            }

            const s = std.mem.indexOfSentinel(u8, '}', @as([*:'}']const u8, @ptrCast(latex_string[i+2..])));
            if (latex_string[i+2+s] != '}') {
                return VarReplaceError.InvalidFormat;
            }

            const index = std.fmt.parseInt(u8, latex_string[i+2..i+2+s], 0) catch {
                return VarReplaceError.InvalidFormat;
            };
            if (index >= nodes.len) {
                return VarReplaceError.IndexOutOfRange;
            }

            try dumpExpression(parse_state, writer, buf, ast_types, nodes[index], in_mat, allow_in_parens);

            i = i+2+s+1;
        } else {
            try writer.writeByte(c);
            i += 1;
        }
    }
}

fn strip(str: []const u8) []const u8 {
    var i: usize = 0;
    while (str[i] == ' ') : (i += 1) {
        continue;
    }
    return str[i..];
}

