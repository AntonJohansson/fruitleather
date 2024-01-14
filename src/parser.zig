const std = @import("std");
const lexer = @import("lexer.zig");
const log = @import("log.zig");

pub const OpType = enum {
    BinaryOp,
    PrefixUnaryOp,
    PostfixUnaryOp,
    Function,
};

pub const AstOpDecl = struct {
    op: usize,
    identifier_presedence: ?usize = null,
    return_type: usize,
    latex_string: usize,
    type: OpType,
};

pub const AstVarDecl = struct {
    var_name: usize,
    type_name: usize,
};

pub const AstTypeDecl = struct {
    type_name: usize,
    latex_string: usize,
};

pub const AstMatrix = struct {
    bracket: usize,
    rows: usize,
    cols: usize,
};

pub const AstBinOpPresedence = enum {
    Semicolon,
    Comma,
    Add,
    Mul,
    Exp,
};

pub const AstBinOp = struct {
    op: usize,
    num_args_in_subtree: u32,
};

pub const AstHeader = struct {
    depth: u32,
};

pub const AstCode = struct {
    small: bool,
};

pub const AstIden = struct {
};

pub const AstType = union(enum) {
    var_name: usize,
    number: usize,
    var_decl:  AstVarDecl,
    type_decl: AstTypeDecl,
    op_decl: AstOpDecl,
    bin_op: AstBinOp,
    unary_op: usize,
    call_op: usize,
    mat: AstMatrix,
    sum: usize,
    prod: usize,
    text: usize,
    code: AstCode,
    header: AstHeader,
    intrin: usize,
    thm: usize,
    def: usize,
    pf: usize,
    block: usize,
};

pub const AstNode = struct {
    ast_type: AstType,
    has_align: bool = false,
    has_newline: bool = false,
    bracketed: bool = false,
    typeindex: usize = 0,
    children: std.ArrayList(*AstNode),
};

pub const ParseError = std.mem.Allocator.Error || error {
    OutOfTokens,
    TokenMismatch,
    Overflow
};

pub const ParseState = struct {
    buffer: *lexer.TokenBuffer,
    top: usize = 0,
    allocator: std.mem.Allocator,
    filebuf: []const u8,
    filename: []const u8,

    identifier_ops_decl: std.ArrayList(*AstOpDecl),

    pub fn loc(state: *ParseState, index: usize) lexer.TokenLocation {
        return state.buffer.locations.items[index];
    }

    pub fn name(state: *ParseState, l: lexer.TokenLocation) []const u8 {
        return state.filebuf[l.start..l.end];
    }

    pub fn tok(state: *ParseState, index: usize) lexer.Token {
        return state.buffer.tokens.items[index];
    }

    fn hasNTokens(state: *ParseState, N: usize) bool {
        return state.top + (N-1) < state.buffer.tokens.items.len;
    }

    fn hasTokens(state: *ParseState) bool {
        return state.hasNTokens(1);
    }

    fn expect(state: *ParseState, token: lexer.Token) ParseError!usize {
        const t = state.peek() catch {
            const index = if (state.top > 0) state.top - 1 else 0;
            log.errAtFmt(state.filename, state.filebuf,
                state.buffer.locations.items[index].start,
                "Out of tokens",
                "Expected expression to end in {}",
                .{token}
            );
            return error.TokenMismatch;
        };
        if (t != token) {
            log.errAtFmt(state.filename, state.filebuf,
                state.buffer.locations.items[state.top].start,
                "Token mismatch",
                "Expected {}, got {}",
                .{token, t}
            );
            std.debug.assert(false);
            return error.TokenMismatch;
        }
        return state.pop();
    }

    fn expectOr(state: *ParseState, comptime tokens: []const lexer.Token) ParseError!usize {
        const current_token = state.peek() catch {
            const index = if (state.top > 0) state.top - 1 else 0;
            log.errAtFmt(state.filename, state.filebuf,
                state.buffer.locations.items[index].start,
                "Out of tokens",
                "Expected expression to end in {any}",
                .{tokens}
            );
            return error.TokenMismatch;
        };

        inline for (tokens) |t| {
            if (current_token == t)
                return state.pop();
        }

        log.errAtFmt(state.filename, state.filebuf,
            state.buffer.locations.items[state.top].start,
            "Token mismatch",
            "Expected {any}, got {}",
            .{tokens, current_token}
        );

        return error.TokenMismatch;
    }

    fn pop(state: *ParseState) ParseError!usize {
        var result = if (hasTokens(state)) state.top else error.OutOfTokens;
        state.top += 1;
        return result;
    }

    fn peekLocAmount(state: *ParseState, amount: usize) ParseError!lexer.TokenLocation {
        if (!state.hasNTokens(amount))
            return error.OutOfTokens;
        return state.loc(state.top + (amount-1));
    }

    fn peekAmount(state: *ParseState, amount: usize) ParseError!lexer.Token {
        if (!state.hasNTokens(amount))
            return error.OutOfTokens;
        return state.tok(state.top + (amount-1));
    }

    fn peek(state: *ParseState) ParseError!lexer.Token {
        return state.peekAmount(1);
    }

    fn match(state: *ParseState, comptime N: usize, comptime tokens: [N]lexer.Token) ParseError![N]usize {
        if (!state.hasNTokens(N))
            return error.OutOfTokens;
        var result: [N]usize = undefined;
        inline for (tokens, 0..) |t,i| {
            result[i] = state.top + i;
            if (t != state.buffer.tokens.items[state.top + i])
                return error.TokenMismatch;
        }
        state.top += N;
        return result;
    }

    fn matchOrAndPop(state: *ParseState, comptime tokens: []const lexer.Token) ParseError!usize {
        if (try state.matchOrOffset(1, tokens))
            return state.pop();
        return error.TokenMismatch;
    }

    fn matchOrOffset(state: *ParseState, offset: usize, comptime tokens: []const lexer.Token) ParseError!bool {
        const current_token = try state.peekAmount(offset);
        inline for (tokens) |t| {
            if (current_token == t)
                return true;
        }
        return false;
    }

    fn makeNode(state: *ParseState) ParseError!*AstNode {
        var node = try state.allocator.create(AstNode);
        node.has_align = false;
        node.has_newline = false;
        node.typeindex = 0;
        node.children = std.ArrayList(*AstNode).init(state.allocator);
        return node;
    }
};

pub fn parse(state: *ParseState) ParseError!std.ArrayList(*AstNode) {
    var statement_list = std.ArrayList(*AstNode).init(state.allocator);
    while (state.hasTokens()) {
        try consumeNewline(state);
        if (!state.hasTokens())
            break;
        const statement = try parseStatement(state);
        try statement_list.append(statement);
    }
    return statement_list;
}

pub fn flattenBinOpSubtree(state: *ParseState, arr: []*AstNode, index: *usize, root: *AstNode) void {
    if (root.ast_type == .bin_op and state.tok(root.ast_type.bin_op.op) == .Comma) {
        flattenBinOpSubtree(state, arr, index, root.children.items[0]);
        flattenBinOpSubtree(state, arr, index, root.children.items[1]);
    } else {
        // leaf
        arr[index.*] = root;
        index.* += 1;
    }
}

fn consumeNewline(state: *ParseState) ParseError!void {
    while (state.hasTokens() and (try state.peek()) == .Newline)
        _ = try state.pop();
    return;
}

fn parseStatement(state: *ParseState) ParseError!*AstNode {
    while (state.hasTokens() and (try state.peek()) == .Newline)
        _ = try state.pop();
    if (!state.hasTokens())
        return error.TokenMismatch;

    const token = try state.peek();
    var node = switch (token) {
        .Header => parseHeader(state),
        .LeftBrace => parseBlock(state),
        .At => try parseIntrinsic(state),
        .Text => parseText(state),
        .SmallCode => parseSmallCode(state),
        .BigCode => parseBigCode(state),
        else => return error.TokenMismatch,
    };
    return node;
}

fn parseBlock(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.LeftBrace);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .block = i,
    };

    while (state.hasTokens()) {
        try consumeNewline(state);
        if (state.hasTokens() and (try state.peek()) == .RightBrace) {
            _ = try state.pop();
            break;
        }
        if (!state.hasTokens())
            break;
        const statement = try parseStatement(state);
        try node.children.append(statement);
    }

    return node;
}

fn parseHeader(state: *ParseState) ParseError!*AstNode {
    var depth: u32 = 0;
    while (try state.peek() == .Header) {
        depth += 1;
        _ = try state.pop();
    }
    const text = try parseText(state);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .header = AstHeader {
            .depth = depth,
        },
    };
    try node.children.append(text);
    return node;
}

fn parseText(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Text);
    var node = try state.makeNode();
    node.ast_type = AstType {
        .text = i,
    };
    return node;
}

fn parseIntrinsic(state: *ParseState) ParseError!*AstNode {
    _ = try state.expect(.At);
    const token = try state.peek();
    switch (token) {
        .Thm => return try parseThm(state),
        .Def => return try parseDef(state),
        .Pf  => return try parseProof(state),
        else => unreachable,
    }
}

fn parseThm(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Thm);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .thm = i,
    };

    _ = try state.expect(.LeftParen);
    const name = try parseIdentifier(state);
    _ = try state.expect(.RightParen);
    try node.children.append(name);

    switch (try state.peek()) {
        .Text => {
            const text = try parseText(state);
            try node.children.append(text);
        },
        else => {},
    }

    const body = try parseStatement(state);
    try node.children.append(body);

    return node;
}

fn parseDef(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Def);

    _ = try state.expect(.LeftParen);
    const name = try parseIdentifier(state);
    _ = try state.expect(.RightParen);

    const text = try parseText(state);

    const body = try parseStatement(state);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .def = i,
    };
    try node.children.append(name);
    try node.children.append(text);
    try node.children.append(body);
    return node;
}

fn parseProof(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Pf);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .pf = i,
    };

    _ = try state.expect(.LeftParen);
    const name = try parseIdentifier(state);
    _ = try state.expect(.RightParen);
    try node.children.append(name);

    switch (try state.peek()) {
        .Text => {
            const text = try parseText(state);
            try node.children.append(text);
        },
        else => {},
    }

    const body = try parseStatement(state);
    try node.children.append(body);

    return node;
}

fn parseImport(state: *ParseState) ParseError!*AstNode {
    _ = state;
    return error.TokenMismatch;
}

fn parseSmallCode(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.SmallCode);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .code = AstCode {
            .small = true,
        },
    };

    const token = try state.peek();
    var code = switch (token) {
        .Import => try parseImport(state),
        .Type => try parseTypeDecl(state),
        .Var  => try parseVarDecl(state),
        .PrefixUnaryOp => try parsePrefixUnaryOpDecl(state),
        .Op   => try parseOpDecl(state),
        .Func => try parseFuncDecl(state),
        else  => try parseExpression(state),
    };
    try node.children.append(code);

    _ = try state.expect(state.buffer.tokens.items[i]);

    return node;
}


fn parseBigCode(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.BigCode);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .code = AstCode {
            .small = false,
        },
    };

    while (true) {
        const token = try state.peek();
        var code = switch (token) {
            .BigCode => break,
            .Import => try parseImport(state),
            .Type => try parseTypeDecl(state),
            .Var  => try parseVarDecl(state),
            .PrefixUnaryOp => try parsePrefixUnaryOpDecl(state),
            .Op   => try parseOpDecl(state),
            .Func => try parseFuncDecl(state),
            else  => try parseExpression(state),
        };
        _ = try state.expect(.Semicolon);
        try node.children.append(code);
    }

    _ = try state.expect(state.buffer.tokens.items[i]);

    return node;
}

fn parseNumber(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Number);
    var node = try state.makeNode();
    node.ast_type = AstType {
        .number = i,
    };
    return node;
}

fn parseIdentifier(state: *ParseState) ParseError!*AstNode {
    const i = try state.expect(.Identifier);
    var node = try state.makeNode();
    node.ast_type = AstType {
        .var_name = i,
    };
    return node;
}

fn parseBinOpExpansion(state: *ParseState, comptime op_tokens: []const lexer.Token, comptime presedence: usize, comptime parse_op: fn (*ParseState) ParseError!*AstNode) ParseError!*AstNode {
    var a = try parse_op(state);

    var num_bin_ops: u32 = 0;

    while (true) {
        var peek_offset: usize = 1;

        const op_or_newline = try state.peekAmount(peek_offset);
        const has_newline = (op_or_newline == .Backslash);
        if (has_newline)
            peek_offset += 1;

        const op_or_align = try state.peekAmount(peek_offset);
        const has_align = (op_or_align == .Align);
        if (has_align)
            peek_offset += 1;

        const has_op = state.matchOrOffset(peek_offset, op_tokens) catch break;

        var identifier_op = false;
        if (!has_op and try state.peekAmount(peek_offset) == .Identifier) {
            identifier_op = true;
        } else if (!has_op) {
            break;
        }

        if (identifier_op) {
            const next_iden = state.name(try state.peekLocAmount(peek_offset));
            var found_op: ?*AstOpDecl = null;
            for (state.identifier_ops_decl.items) |op_decl| {
                std.debug.assert(op_decl.identifier_presedence != null);
                if (op_decl.identifier_presedence != presedence)
                    continue;
                const iden = state.name(state.loc(op_decl.op));
                if (std.mem.eql(u8, next_iden, iden)) {
                    found_op = op_decl;
                    break;
                }
            }

            if (found_op == null) {
                break;
            }
        }

        if (has_newline)
            _ = try state.pop();
        if (has_align)
            _ = try state.pop();

        const op = try state.pop();

        const b = try parse_op(state);

        num_bin_ops += 1;

        var node = try state.makeNode();
        node.ast_type = AstType {
            .bin_op = AstBinOp {
                .op = op,
                .num_args_in_subtree = num_bin_ops + 1,
            },
        };
        node.has_align = has_align;
        node.has_newline = has_newline;
        try node.children.append(a);
        try node.children.append(b);

        a = node;
    }

    return a;
}

fn parseCall(state: *ParseState) ParseError!*AstNode {
    const iden = try state.pop();
    _ = try state.expect(.LeftParen);
    const expr = try parseBinOpExpansion(state, &.{.Comma}, 6, parseEqual);
    _ = try state.expect(.RightParen);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .call_op = iden,
    };
    try node.children.append(expr);

    return node;
}

fn parseComma(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Comma}, 6, parseAdd);
}

pub fn getMatchingParen(state: *ParseState, index: usize) !lexer.Token {
    switch (state.buffer.tokens.items[index]) {
        .LeftParen         => return .RightParen,
        .LeftBracket       => return .RightBracket,
        .LeftAngleBracket  => return .RightAngleBracket,
        .LeftBrace         => return .RightBrace,
        .RightParen        => return .LeftParen,
        .RightBracket      => return .LeftBracket,
        .RightAngleBracket => return .LeftAngleBracket,
        .RightBrace        => return .LeftBrace,
        else               => return error.TokenMismatch,
    }
}

fn parseVectorOrMatrix(state: *ParseState) ParseError!*AstNode {
    const open_paren = try state.matchOrAndPop(&.{.LeftBracket, .LeftAngleBracket, .LeftBrace});
    const row = try parseBinOpExpansion(state, &.{.Semicolon}, 7, parseComma);
    _ = try state.expect(try getMatchingParen(state, open_paren));

    const num_cols = if (row.ast_type == .bin_op) row.ast_type.bin_op.num_args_in_subtree else 1;
    const num_rows = if (row.children.items.len > 1 and row.children.items[1].ast_type == .bin_op and state.buffer.tokens.items[row.children.items[1].ast_type.bin_op.op] == .Comma) row.children.items[1].ast_type.bin_op.num_args_in_subtree else 1;


    var node = try state.makeNode();
    node.ast_type = AstType {
        .mat = AstMatrix {
            .bracket = open_paren,
            .rows = num_rows,
            .cols = num_cols,
        },
    };
    try node.children.append(row);

    return node;
}

fn parsePrimary(state: *ParseState) ParseError!*AstNode {
    var t0 = try state.peek();

    const has_newline = (t0 == .Backslash);
    if (has_newline) {
        _ = try state.pop();
        t0 = try state.peek();
    }

    const has_align = (t0 == .Align);
    if (has_align) {
        _ = try state.pop();
        t0 = try state.peek();
    }

    var node: *AstNode = undefined;

    if (t0 == .Identifier) {
        const t1 = state.peekAmount(2) catch {
            node = try parseIdentifier(state);
            node.has_align = has_align;
            node.has_newline = has_newline;
            return node;
        };

        if (t1 == .LeftParen) {
            node = try parseCall(state);
        } else {
            node = try parseIdentifier(state);
        }
    } else if (t0 == .LeftBracket or
               t0 == .LeftAngleBracket or
               t0 == .LeftBrace) {
        node = try parseVectorOrMatrix(state);
    } else if (t0 == .Number) {
        node = try parseNumber(state);
    } else if (t0 == .LeftParen) {
        _ = try state.expect(.LeftParen);
        node = try parseAdd(state);
        _ = try state.expect(.RightParen);
        node.bracketed = true;
    } else {
        log.errAt(state.filename, state.filebuf,
            state.buffer.locations.items[state.top].start,
            "Unrecognized primary expression",
            "here"
            );
        return error.TokenMismatch;
    }

    node.has_align = has_align;
    node.has_newline = has_newline;

    return node;
}

fn parseUnary(state: *ParseState) ParseError!*AstNode {
    const op = state.matchOrAndPop(&.{.Add, .Sub}) catch {
        return try parsePrimary(state);
    };

    const primary = try parsePrimary(state);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .unary_op = op,
    };
    try node.children.append(primary);

    return node;
}

fn parsePower(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Superscript, .Subscript}, 0, parseUnary);
}

fn parseMul(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Mul, .Div, .Backslash, .Period}, 1, parsePower);
}

fn parseAdd(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Add, .Sub}, 2, parseMul);
}

fn parseIn(state: *ParseState) ParseError!*AstNode {
    return parseBinOpExpansion(state, &.{.In}, 3, parseAdd);
}

fn parseEqual(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Equal, .LeftAngleBracket, .RightAngleBracket, .LessThanEqual, .GreaterThanEqual}, 4, parseIn);
}

fn parseArrows(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.To, .RightImp, .LeftImp, .Eqv}, 5, parseEqual);
}

fn parseExpression(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Comma}, 6, parseArrows);
}

fn parseTypeDecl(state: *ParseState) ParseError!*AstNode {
    const i = try state.match(3, [_]lexer.Token{.Type, .Identifier, .LeftParen});
    const args = try parseBinOpExpansion(state, &.{.Comma}, 6, parseIdentifier);
    const j = try state.match(3, [_]lexer.Token{.RightParen, .Colon, .String});

    var node = try state.makeNode();
    node.ast_type = AstType {
        .type_decl = AstTypeDecl {
            .type_name = i[1],
            .latex_string = j[2],
        },
    };
    try node.children.append(args);

    return node;
}

fn parseArgDecl(state: *ParseState) ParseError!*AstNode {
    const i = try state.match(3, [_]lexer.Token{.Identifier, .Colon, .Identifier});

    var node = try state.makeNode();
    node.ast_type = AstType {
        .var_decl = AstVarDecl {
            .var_name = i[0],
            .type_name = i[2],
        },
    };

    return node;
}

fn parseVarDecl(state: *ParseState) ParseError!*AstNode {
    const i = try state.match(4, [_]lexer.Token{.Var, .Identifier, .Colon, .Identifier});

    var node = try state.makeNode();
    node.ast_type = AstType {
        .var_decl = AstVarDecl {
            .var_name = i[1],
            .type_name = i[3],
        },
    };

    return node;
}

fn parseOpArgs(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Comma}, 6, parseArgDecl);
}

fn parsePrefixUnaryOpDecl(state: *ParseState) ParseError!*AstNode {
    _ = try state.expect(.PrefixUnaryOp);
    const op = try state.pop();

    _ = try state.expect(.LeftParen);
    const args = try parseOpArgs(state);
    _ = try state.expect(.RightParen);
    _ = try state.expect(.To);
    const return_type = try state.expect(.Identifier);
    _ = try state.expect(.Colon);
    const string = try state.expect(.String);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .op_decl = AstOpDecl {
            .op = op,
            .return_type = return_type,
            .latex_string = string,
            .type = .PrefixUnaryOp,
        },
    };
    try node.children.append(args);

    return node;
}

fn mapTokenToPresedence(token: lexer.Token) usize {
    switch (token) {
        .Superscript,
        .Subscript
        => return 0,

        .Mul, .Div, .Period
        => return 1,

        .Add, .Sub
        => return 2,

        .In
        => return 3,

        .Equal, .LeftAngleBracket,
        .RightAngleBracket,
        .LessThanEqual, .GreaterThanEqual
        => return 4,

        .RightImp, .LeftImp, .Eqv
        => return 5,

        .Comma
        => return 6,
        .Semicolon
        => return 7,

        else => unreachable,
    }
}

fn parseOpDecl(state: *ParseState) ParseError!*AstNode {
    _ = try state.expect(.Op);

    const identifier_op = try state.peek() == .Identifier;

    const op = try state.pop();

    var identifier_presedence: ?usize = null;
    if (identifier_op) {
        _ = try state.expect(.LeftBracket);
        const presedence_op = try state.expectOr(&.{.Equal,.Add,.Sub,.Mul,.Div,.Backslash,.Superscript});
        _ = try state.expect(.RightBracket);

        identifier_presedence = mapTokenToPresedence(state.tok(presedence_op));
    }

    _ = try state.expect(.LeftParen);
    const args = try parseOpArgs(state);
    _ = try state.expect(.RightParen);
    _ = try state.expect(.To);
    const return_type = try state.expect(.Identifier);
    _ = try state.expect(.Colon);
    const string = try state.expect(.String);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .op_decl = AstOpDecl {
            .op = op,
            .identifier_presedence = identifier_presedence,
            .return_type = return_type,
            .latex_string = string,
            .type = .BinaryOp,
        },
    };
    try node.children.append(args);

    if (identifier_op) {
        try state.identifier_ops_decl.append(&node.ast_type.op_decl);
    }

    return node;
}

fn parseFuncDecl(state: *ParseState) ParseError!*AstNode {
    _ = try state.expect(.Func);

    const func = try state.pop();

    _ = try state.expect(.LeftParen);
    const args = try parseOpArgs(state);
    _ = try state.expect(.RightParen);
    _ = try state.expect(.To);
    const return_type = try state.expect(.Identifier);
    _ = try state.expect(.Colon);
    const string = try state.expect(.String);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .op_decl = AstOpDecl {
            .op = func,
            .return_type = return_type,
            .latex_string = string,
            .type = .Function,
        },
    };
    try node.children.append(args);

    return node;
}

