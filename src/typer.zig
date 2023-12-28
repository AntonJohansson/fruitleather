const std = @import("std");
const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const log = @import("log.zig");

const ParseState = parser.ParseState;
const AstNode = parser.AstNode;
const OpType = parser.OpType;

pub const TypeInfo = struct {
    name: []const u8,
    latex_string: []const u8,
    var_name: []const u8,
};

pub const ArgInfo = struct {
    name: []const u8,
    typeindex: usize,
};

pub const OpInfo = struct {
    name: []const u8,
    latex_string: []const u8,
    args: []ArgInfo,
    rettypeindex: usize,
    type: OpType,
};

pub const AstTypes = struct {
    types: std.StringHashMap(usize),
    vars: std.StringHashMap([]const u8),
    typearray: std.ArrayList(TypeInfo),
    oparray: std.ArrayList(OpInfo),
    ops: std.AutoHashMap(usize, *const OpInfo),
};

pub fn propagateTypes(state: *ParseState, statements: *std.ArrayList(*AstNode)) !AstTypes {
    var ast_types = AstTypes {
        .types = std.StringHashMap(usize).init(state.allocator),
        .vars = std.StringHashMap([]const u8).init(state.allocator),
        .typearray = std.ArrayList(TypeInfo).init(state.allocator),
        .oparray = std.ArrayList(OpInfo).init(state.allocator),
        .ops = std.AutoHashMap(usize, *const OpInfo).init(state.allocator),
    };

    try ast_types.typearray.append(TypeInfo { .name = "Unknown", .latex_string = "", .var_name = "" });
    try ast_types.typearray.append(TypeInfo { .name = "Number", .latex_string = "", .var_name = ""});
    try ast_types.typearray.append(TypeInfo { .name = "Matrix", .latex_string = "\\mathrm{\\mathbf{${0}}}", .var_name = "x"});
    try ast_types.typearray.append(TypeInfo { .name = "Vector", .latex_string = "\\vec{${0}}", .var_name = "x"});

    try ast_types.types.put("Unknown", 0);
    try ast_types.types.put("Number", 1);
    try ast_types.types.put("Matrix", 2);
    try ast_types.types.put("Vector",  3);

    const code_nodes = collectCodeNodes(state, statements);

    for (code_nodes.items) |parent| {
        var sj: usize = 0;
        while (sj < parent.children.items.len) {
            const node = parent.children.items[sj];
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
                        var child: *AstNode = node.children.items[0];
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
                    const op_decl = node.ast_type.op_decl;
                    const op_loc = state.buffer.locations.items[op_decl.op];
                    const return_type_loc = state.buffer.locations.items[op_decl.return_type];
                    const latex_loc = state.buffer.locations.items[op_decl.latex_string];

                    const op_name = state.filebuf[op_loc.start..op_loc.end];
                    const return_typename = state.filebuf[return_type_loc.start..return_type_loc.end];
                    const tr = ast_types.types.get(return_typename) orelse blk: {
                        log.errAt(state.filename, state.filebuf, return_type_loc.start, "Parsing op decl.", "Invalid return type, definition not found");
                        break :blk 0;
                    };

                    const latex_string = state.filebuf[latex_loc.start..latex_loc.end];

                    switch (op_decl.type) {
                        .BinaryOp => {
                            // TODO: don't hardode 2 args
                            const varname_a_loc  = state.loc(node.children.items[0].children.items[0].ast_type.var_decl.var_name);
                            const typename_a_loc = state.loc(node.children.items[0].children.items[0].ast_type.var_decl.type_name);
                            const varname_b_loc  = state.loc(node.children.items[0].children.items[1].ast_type.var_decl.var_name);
                            const typename_b_loc = state.loc(node.children.items[0].children.items[1].ast_type.var_decl.type_name);
                            const varname_a  = state.name(varname_a_loc);
                            const typename_a = state.name(typename_a_loc);
                            const varname_b  = state.name(varname_b_loc);
                            const typename_b = state.name(typename_b_loc);
                            var ta: usize = ast_types.types.get(typename_a) orelse 0;
                            var tb: usize = ast_types.types.get(typename_b) orelse 0;

                            // TODO(anjo): Freeing?
                            var args = try state.allocator.alloc(ArgInfo, 2);

                            args[0] = .{.name = varname_a, .typeindex = ta};
                            args[1] = .{.name = varname_b, .typeindex = tb};
                            try ast_types.oparray.append(OpInfo {
                                .name = op_name,
                                .latex_string = latex_string,
                                .args = args,
                                .rettypeindex = tr,
                                .type = op_decl.type,
                            });
                            _ = parent.children.orderedRemove(sj);
                        },
                        .PostfixUnaryOp,
                        .PrefixUnaryOp => {
                            const varname_a_loc  = state.loc(node.children.items[0].ast_type.var_decl.var_name);
                            const typename_a_loc = state.loc(node.children.items[0].ast_type.var_decl.type_name);
                            const varname_a  = state.name(varname_a_loc);
                            const typename_a = state.name(typename_a_loc);
                            var ta: usize = ast_types.types.get(typename_a) orelse 0;

                            // TODO(anjo): Freeing?
                            var args = try state.allocator.alloc(ArgInfo, 1);
                            args[0] = .{.name = varname_a, .typeindex = ta};

                            try ast_types.oparray.append(OpInfo {
                                .name = op_name,
                                .latex_string = latex_string,
                                .args = args,
                                .rettypeindex = tr,
                                .type = op_decl.type,
                            });
                            _ = parent.children.orderedRemove(sj);
                        },
                        .Function => {
                            const node_args = node.children.items[0];
                            const num_args = if (node_args.ast_type == .bin_op and state.tok(node_args.ast_type.bin_op.op) == .Comma) node_args.ast_type.bin_op.num_args_in_subtree else 1;

                            var flat_nodes = try state.allocator.alloc(*AstNode, num_args);
                            var flat_index: usize = 0;
                            parser.flattenBinOpSubtree(state, flat_nodes, &flat_index, node_args);
                            defer state.allocator.free(flat_nodes);

                            // TODO(anjo): Freeing?
                            var args = try state.allocator.alloc(ArgInfo, num_args);

                            for (flat_nodes, 0..) |n,i| {
                                const varname_loc  = state.loc(n.ast_type.var_decl.var_name);
                                const typename_loc = state.loc(n.ast_type.var_decl.type_name);
                                const varname = state.name(varname_loc);
                                const typename = state.name(typename_loc);
                                const typeindex: usize = ast_types.types.get(typename) orelse 0;

                                args[i] = .{
                                    .name = varname,
                                    .typeindex = typeindex,
                                };
                            }

                            try ast_types.oparray.append(OpInfo {
                                .name = op_name,
                                .latex_string = latex_string,
                                .args = args,
                                .rettypeindex = tr,
                                .type = op_decl.type,
                            });
                            _ = parent.children.orderedRemove(sj);
                        },
                    }
                },
                else => {
                    _ = setSubtreeTypes(state, node, &ast_types);
                    sj += 1;
                },
            }
        }
    }

    // Remove empty non-text nodes
    var si: usize = 0;
    while (si < statements.items.len) {
        const parent = statements.items[si];
        if (parent.ast_type != .text and parent.children.items.len == 0) {
            _ = statements.orderedRemove(si);
        } else {
            si += 1;
        }
    }

    return ast_types;
}

fn setNodeType(state: *ParseState, node: *AstNode, ast_types: *AstTypes, new_type: usize) void {
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
        const op_loc = state.loc(node.ast_type.bin_op.op);
        const op_name = state.name(op_loc);
        var opinfo: ?OpInfo = null;
        for (ast_types.oparray.items) |oi| {
            if (oi.rettypeindex == new_type and std.mem.eql(u8, op_name, oi.name)) {
                opinfo = oi;
                break;
            }
        }

        if (opinfo) |oi| {
            for (oi.args, 0..) |arg,i| {
                if (arg.typeindex == 0)
                    continue;

                if (node.children.items[i].typeindex == 0) {
                    setNodeType(state, node.children.items[i], ast_types, arg.typeindex);
                } else {
                    log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Type conflict", "Has {s}, propagating {s} from op {s}(TODO: args)->{s}", .{
                        ast_types.typearray.items[node.children.items[i].typeindex].name,
                        ast_types.typearray.items[arg.typeindex].name,
                        op_name,
                        ast_types.typearray.items[oi.rettypeindex].name,
                    });
                }
            }
        }

    } else {
        //unreachable;
    }
}

const MatchingOpInfo = struct {
    oi: *const OpInfo,
    matching_args: u8,
};
fn cmpMatchingOps(context: void, a: MatchingOpInfo, b: MatchingOpInfo) std.math.Order {
    _ = context;
    return std.math.order(a.matching_args, b.matching_args).invert();
}

fn setSubtreeTypes(state: *ParseState, node: *AstNode, ast_types: *AstTypes) usize {
    switch (node.ast_type) {
        .var_name => |v| {
            // Look at variable declarations
            node.typeindex = 0;
            const var_loc = state.buffer.locations.items[v];
            const var_name = state.filebuf[var_loc.start..var_loc.end];
            const type_name = ast_types.vars.get(var_name);
            if (type_name != null) {
                for (ast_types.typearray.items, 0..) |t,i| {
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
            var ta = setSubtreeTypes(state, node.children.items[0], ast_types);
            var tb = setSubtreeTypes(state, node.children.items[1], ast_types);
            // CONTHERE: We only assign when either type is unknown, maybe print error
            // if there is a type mismatch? If this is too strong of an assumption,
            // we'll see in actual testing.
            if (state.buffer.tokens.items[v.op] == .Equal) {
                if (ta == 0) {
                    setNodeType(state, node.children.items[0], ast_types, tb);
                } else if (tb == 0) {
                    setNodeType(state, node.children.items[1], ast_types, ta);
                } else if (ta != tb) {
                    const op_loc = state.buffer.locations.items[v.op];
                    log.errAtFmt(state.filename, state.filebuf, op_loc.start, "Type conflict", "{s} = {s}", .{
                        ast_types.typearray.items[ta].name,
                        ast_types.typearray.items[tb].name
                    });

                }

                node.typeindex = node.children.items[0].typeindex;
            } else {
                // Look at ops on types and assign accordingly

                const op_loc = state.buffer.locations.items[v.op];
                const op_name = state.filebuf[op_loc.start..op_loc.end];
                const flat_nodes = [_]*AstNode{node.children.items[0], node.children.items[1]};
                const arg_types = [2]usize{ta, tb};

                setTypeFromMatchingOp(state, ast_types, v.op, op_name, node, &arg_types, &flat_nodes);
            }
        },
        .unary_op => {
            // ?
            _ = setSubtreeTypes(state, node.children.items[0], ast_types);
        },
        .mat => |v| {
            // Set to matrix or vector type depending on size
            _ = setSubtreeTypes(state, node.children.items[0], ast_types);
            if (v.cols == 1 or v.rows == 1) {
                node.typeindex = 3;
            } else {
                node.typeindex = 2;
            }
        },
        .sum => {
            // Depends on type of terms and ops on this type
            _ = setSubtreeTypes(state, node.children.items[1], ast_types);
        },
        .prod => {
            // Depends on type of factor and ops on this type
            _ = setSubtreeTypes(state, node.children.items[1], ast_types);
        },
        .call_op => |v| {
            const node_args = node.children.items[0];
            const num_args = if (node_args.ast_type == .bin_op and state.tok(node_args.ast_type.bin_op.op) == .Comma) node_args.ast_type.bin_op.num_args_in_subtree else 1;

            var flat_nodes = state.allocator.alloc(*AstNode, num_args) catch unreachable;
            var flat_index: usize = 0;
            parser.flattenBinOpSubtree(state, flat_nodes, &flat_index, node_args);
            defer state.allocator.free(flat_nodes);

            // TODO(anjo): Freeing
            var arg_types = state.allocator.alloc(usize, num_args) catch unreachable;
            for (flat_nodes, 0..) |n,i| {
                const typeindex = setSubtreeTypes(state, n, ast_types);
                arg_types[i] = typeindex;
            }

            const op_name = state.name(state.loc(v));

            setTypeFromMatchingOp(state, ast_types, v, op_name, node, arg_types, flat_nodes);
        },
        .intrin => {
    },
    .block => {
        for (node.children.items) |c| {
                _ = setSubtreeTypes(state, c, ast_types);
            }
            std.log.info("block", .{});
        },
        .thm => {
            const has_title = node.children.items.len == 3;
            const i: usize = if (has_title) 2 else 1;
            _ = setSubtreeTypes(state, node.children.items[i], ast_types);
            std.log.info("block", .{});
        },
        .def => {
            const has_title = node.children.items.len == 3;
            const i: usize = if (has_title) 2 else 1;
            _ = setSubtreeTypes(state, node.children.items[i], ast_types);
            std.log.info("block", .{});
        },
        else => unreachable,
    }
    return node.typeindex;
}

fn collectCodeSubnodes(state: *ParseState, node: *AstNode, result: *std.ArrayList(*AstNode)) void {
    if (node.ast_type == .code) {
        result.append(node) catch unreachable;
    } else {
        for (node.children.items) |c| {
            collectCodeSubnodes(state, c, result);
        }
    }
}

fn collectCodeNodes(state: *ParseState, statements: *std.ArrayList(*AstNode)) std.ArrayList(*AstNode) {
    var result = std.ArrayList(*AstNode).init(state.allocator);
    for (statements.items) |n| {
        collectCodeSubnodes(state, n, &result);
    }
    return result;
}

fn setTypeFromMatchingOp(state: *ParseState, ast_types: *AstTypes, op_index: usize, op_name: []const u8, result_node: *AstNode, arg_types: []const usize, flat_nodes: []const *AstNode) void {
    var matching_ops = std.PriorityQueue(MatchingOpInfo, void, cmpMatchingOps).init(state.allocator, {});

    for (ast_types.oparray.items) |*o| {
        if (std.mem.eql(u8, op_name, o.name) and o.args.len == arg_types.len) {
            var matching_args: u8 = 0;
            for (o.args, 0..) |a,i| {
                if (a.typeindex == 0 or arg_types[i] == 0 or a.typeindex == arg_types[i]) {
                    matching_args += 1;
                }
            }

            if (matching_args > 0) {
                matching_ops.add(.{
                    .oi = o,
                    .matching_args = matching_args
                }) catch unreachable;
            }
        }
    }

    if (matching_ops.removeOrNull()) |best_m| {
        var multiple_ops_apply = false;
        while (matching_ops.removeOrNull()) |m| {
            if (m.matching_args < best_m.matching_args)
                break;
            multiple_ops_apply = true;
            log.err("Multiple ops apply [{}]: {s}() -> {s}\n", .{
                m.matching_args,
                m.oi.name,
                ast_types.typearray.items[m.oi.rettypeindex].name,
            });
        }

        if (!multiple_ops_apply) {
            ast_types.ops.put(op_index, best_m.oi) catch unreachable;
            for (best_m.oi.args, 0..) |a,i| {
                if (a.typeindex == 0 or flat_nodes[i].typeindex != 0)
                    continue;
                setNodeType(state, flat_nodes[i], ast_types, a.typeindex);
            }
            result_node.typeindex = best_m.oi.rettypeindex;
        }
    }
}
