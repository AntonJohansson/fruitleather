const std = @import("std");
const lexer = @import("lexer.zig");
const log = @import("log.zig");

// latexString := ...
// argList  := Identifier | Identifier "," argList
// typeDecl := "type" Identifier "(" argList ")" ":" latexString
// varDecl  := Identifier ":" Identifier
// op       := Add | Sub | Mul | Div
// opDecl   := Define op "(" varDecl "," varDecl ")" latexString
//
// var := Identifier | Identifier '^' Expr | Identifier '_' Expr

pub const AstOpDecl = struct {
    op: usize,
    latex_string: usize,
};

pub const AstVarDecl = struct {
    var_name: usize,
    type_name: usize,
};

pub const AstTypeDecl = struct {
    type_name: usize,
    latex_string: usize,
};

pub const AstType = union(enum) {
    var_name: usize,
    number: usize,
    var_decl:  AstVarDecl,
    type_decl: AstTypeDecl,
    op_decl: AstOpDecl,
    bin_op: usize,
};

pub const AstNode = struct {
    ast_type: AstType,
    children: std.BoundedArray(*AstNode, 4),
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

    fn hasNTokens(state: *ParseState, comptime N: usize) bool {
        return state.top + (N-1) < state.buffer.tokens.items.len;
    }

    fn hasTokens(state: *ParseState) bool {
        return state.hasNTokens(1);
    }

    fn expect(state: *ParseState, token: lexer.Token) ParseError!usize {
        const t = try state.peek();
        if (t != token)
            return error.TokenMismatch;
        return state.pop();
    }

    fn pop(state: *ParseState) ParseError!usize {
        var result = if (hasTokens(state)) state.top else error.OutOfTokens;
        state.top += 1;
        return result;
    }

    fn peekAmount(state: *ParseState, comptime amount: usize) ParseError!lexer.Token {
        if (!state.hasNTokens(amount))
            return error.OutOfTokens;
        return state.buffer.tokens.items[state.top + (amount-1)];
    }

    fn peek(state: *ParseState) ParseError!lexer.Token {
        return state.peekAmount(1);
    }

    fn match(state: *ParseState, comptime N: usize, comptime tokens: [N]lexer.Token) ParseError![N]usize {
        if (!state.hasNTokens(N))
            return error.OutOfTokens;
        var result: [N]usize = undefined;
        inline for (tokens) |t,i| {
            result[i] = state.top + i;
            if (t != state.buffer.tokens.items[state.top + i])
                return error.TokenMismatch;
        }
        state.top += N;
        return result;
    }

    fn matchOr(state: *ParseState, comptime tokens: []const lexer.Token) ParseError!usize {
        const current_token = try state.peek();
        inline for (tokens) |t| {
            if (current_token == t)
                return state.pop();
        }
        return error.TokenMismatch;
    }

    fn makeNode(state: *ParseState) ParseError!*AstNode {
        var node = try state.allocator.create(AstNode);
        node.children = try std.BoundedArray(*AstNode, 4).init(0);
        return node;
    }
};

pub fn parse(state: *ParseState) ParseError!std.ArrayList(*AstNode) {
    var statement_list = std.ArrayList(*AstNode).init(state.allocator);
    while (state.hasTokens()) {
        const statement = try parseStatement(state);
        try statement_list.append(statement);
    }
    return statement_list;
}

fn parseStatement(state: *ParseState) ParseError!*AstNode {
    const token = try state.peek();
    switch (token) {
        .Type => return parseTypeDecl(state),
        .Var  => return parseVarDecl(state),
        .Op   => return parseOpDecl(state),
        else  => return parseExpression(state),
    }
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

fn parseBinOpExpansion(state: *ParseState, comptime op_tokens: []const lexer.Token, parse_op: fn (*ParseState) ParseError!*AstNode) ParseError!*AstNode {
    var a = try parse_op(state);

    while (true) {
        const op = state.matchOr(op_tokens) catch break;
        const b = try parse_op(state);

        var node = try state.makeNode();
        node.ast_type = AstType {
            .bin_op = op,
        };
        try node.children.append(a);
        try node.children.append(b);

        a = node;
    }

    return a;
}

fn parsePrimary(state: *ParseState) ParseError!*AstNode {
    const t = try state.peek();
    if (t == .Identifier) {
        return try parseIdentifier(state);
    } else if (t == .Number) {
        return try parseNumber(state);
    } else {
        log.errAt(state.filename, state.filebuf,
            state.buffer.locations.items[state.top].start,
            "Unrecognized primary expression",
            "here"
            );
        return error.TokenMismatch;
    }
}

//fn parseUnary(state: *ParseState) ParseError!*AstNode {
//    return try parseBinOpExpansion(state, &.{.Equal}, parsePrimary);
//}

fn parsePower(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Superscript, .Subscript}, parsePrimary);
}

fn parseMul(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Mul, .Div}, parsePower);
}

fn parseAdd(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Add, .Sub}, parseMul);
}

fn parseEqual(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Equal}, parseAdd);
}

fn parseExpression(state: *ParseState) ParseError!*AstNode {
    const token = try state.peek();
    if (token == .Identifier) {
        const next_token = try state.peekAmount(2);
        if (next_token == .Equal) {
            return parseEqual(state);
        } else {
            return parseIdentifier(state);
        }
    }

    return error.TokenMismatch;
}

fn parseArgs(state: *ParseState) ParseError!*AstNode {
    return try parseBinOpExpansion(state, &.{.Comma}, parseIdentifier);
}

fn parseTypeDecl(state: *ParseState) ParseError!*AstNode {
    const i = try state.match(3, .{.Type, .Identifier, .LeftParen});
    const args = try parseArgs(state);
    const j = try state.match(3, .{.RightParen, .Colon, .String});

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
    const i = try state.match(3, .{.Identifier, .Colon, .Identifier});

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
    const i = try state.match(4, .{.Var, .Identifier, .Colon, .Identifier});

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
    return try parseBinOpExpansion(state, &.{.Comma}, parseArgDecl);
}

fn parseOpDecl(state: *ParseState) ParseError!*AstNode {
    _ = try state.expect(.Op);

    const op = try state.matchOr(&.{.Add, .Sub, .Mul, .Div});

    _ = try state.expect(.LeftParen);
    const args = try parseOpArgs(state);
    _ = try state.expect(.RightParen);
    _ = try state.expect(.Colon);
    const string = try state.expect(.String);

    var node = try state.makeNode();
    node.ast_type = AstType {
        .op_decl = AstOpDecl {
            .op = op,
            .latex_string = string,
        },
    };
    try node.children.append(args);

    return node;
}
