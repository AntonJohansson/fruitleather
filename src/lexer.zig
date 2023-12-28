const std = @import("std");
const log = @import("log.zig");

const ziglyph = @import("ziglyph");

pub const Token = enum(u32) {
    // 1-char
    Comma,
    Period,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftAngleBracket,
    RightAngleBracket,
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    Superscript,
    Subscript,
    Backslash,
    Align,
    SmallCode,
    BigCode,
    Header,
    Newline,
    At,
    // ?
    LessThanEqual,
    GreaterThanEqual,
    // keywords
    Import,
    Type,
    Var,
    Op,
    PrefixUnaryOp,
    In,
    Sum,
    Prod,
    Def,
    Thm,
    Pf,
    Func,
    LeftImp,
    RightImp,
    Eqv,
    To,
    MapsTo,
    // multi-char
    Number,
    Identifier,
    String,
    Text,

    Unknown,
};

// ==>
// <==
// <=
// >=
// ->
// |->

pub const TokenLocation = struct {
    start: usize,
    end: usize,
};

pub const TokenBuffer = struct {
    tokens: std.ArrayList(Token),
    locations: std.ArrayList(TokenLocation),

    fn init(allocator: std.mem.Allocator) !TokenBuffer {
        return TokenBuffer {
            .tokens = std.ArrayList(Token).init(allocator),
            .locations = std.ArrayList(TokenLocation).init(allocator),
        };
    }

    fn add(buffer: *TokenBuffer, token: Token, start_index: usize, end_index: usize) !void {
        try buffer.tokens.append(token);
        try buffer.locations.append(TokenLocation{.start = start_index, .end = end_index});
    }
};

pub const LexerState = enum {
    Start,
    Code,
    BeginCodeDelim1,
    BeginCodeDelim2,
    EndCodeDelim1,
    EndCodeDelim2,
    Number,
    Identifier,
    String,
    Equal,
    RightArrow,
    LeftArrow,
    Minus,
    LessThan,
    GreaterThan,
    CommentOrDiv,
    Comment,
    IntrinState,
};

pub const LexError = error {
    InvalidToken,
};

pub fn lex(allocator: std.mem.Allocator, filename: []const u8, buf: []const u8) !TokenBuffer {
    var buffer = try TokenBuffer.init(allocator);

    var return_state: ?LexerState = null;
    var state: LexerState = .Start;
    var multi_char_token_start: usize = 0;

    const keywords = std.ComptimeStringMap(Token, .{
        .{"import", .Import},
        .{"type",   .Type},
        .{"var",    .Var},
        .{"op",     .Op},
        .{"prefixunaryop", .PrefixUnaryOp},
        .{"in",     .In},
        .{"def",    .Def},
        .{"thm",    .Thm},
        .{"pf",     .Pf},
        .{"func",   .Func},
    });

    var has_invalid_tokens = false;

    var i: usize = 0;
    while (i < buf.len) {
        const c = buf[i];
        switch (state) {
            .Start => switch (c) {
                '#' => {try buffer.add(.Header, i, i+1); i += 1; multi_char_token_start = i;},
                '\n',
                '\r' => {
                    if (i > multi_char_token_start) {
                        try buffer.add(.Text, multi_char_token_start, i);
                        multi_char_token_start = i;
                    }
                    try buffer.add(.Newline, i, i+1);
                    i += 1;
                    multi_char_token_start = i;
                },
                '`' => {
                    if (i > multi_char_token_start) {
                        try buffer.add(.Text, multi_char_token_start, i);
                        multi_char_token_start = i;
                    }
                    state = .BeginCodeDelim1;
                    i += 1;
                },
                '/' => {
                    if (i > multi_char_token_start) {
                        try buffer.add(.Text, multi_char_token_start, i);
                        multi_char_token_start = i;
                    }
                    state = .Comment;
                    i += 1;
                },
                '@' => {try buffer.add(.At, i, i+1); i += 1; state = .IntrinState;},
                '{' => {try buffer.add(.LeftBrace, i, i+1); i += 1; multi_char_token_start = i;},
                '}' => {try buffer.add(.RightBrace, i, i+1); i += 1; multi_char_token_start = i;},
                else => {
                    i += 1;
                }
            },
            .IntrinState => switch (c) {
                '(' => {try buffer.add(.LeftParen, i, i+1); i += 1;},
                ')' => {try buffer.add(.RightParen, i, i+1); i += 1; state = .Start; multi_char_token_start = i;},
                ',' => {try buffer.add(.Comma, i, i+1); i += 1;},
                'a' ... 'z','A' ... 'Z' => {
                    multi_char_token_start = i;
                    state = .Identifier;
                    return_state = .IntrinState;
                    i += 1;
                },
                else => {
                    unreachable;
                },
            },
            .BeginCodeDelim1 => switch (c) {
                '`' => {
                    state = .BeginCodeDelim2;
                    i += 1;
                },
                else => {
                    try buffer.add(.SmallCode, i, i+1);
                    state = .Code;
                    multi_char_token_start = i;
                }
            },
            .BeginCodeDelim2 => switch (c) {
                '`' => {
                    try buffer.add(.BigCode, i, i+1);
                    state = .Code;
                    i += 1;
                    multi_char_token_start = i;
                },
                else => {
                    unreachable;
                }
            },
            .EndCodeDelim1 => switch (c) {
                '`' => {
                    state = .EndCodeDelim2;
                    i += 1;
                },
                else => {
                    try buffer.add(.SmallCode, i, i+1);
                    state = .Start;
                    multi_char_token_start = i;
                }
            },
            .EndCodeDelim2 => switch (c) {
                '`' => {
                    try buffer.add(.BigCode, i, i+1);
                    state = .Start;
                    i += 1;
                    multi_char_token_start = i;
                },
                else => {
                    unreachable;
                }
            },
            .Comment => switch (c) {
                '/' => {
                    while (i < buf.len) {
                        switch (buf[i]) {
                            '\n', '\r' => {
                                i += 1;
                                break;
                            },
                            else => i += 1
                        }
                    }
                    multi_char_token_start = i;
                    state = .Start;
                },
                else => {
                    state = .Start;
                },
            },
            .CommentOrDiv => switch (c) {
                '/' => {
                    while (i < buf.len) {
                        switch (buf[i]) {
                            '\n', '\r' => {
                                i += 1;
                                break;
                            },
                            else => i += 1
                        }
                    }
                    multi_char_token_start = i;
                    state = .Code;
                },
                else => {
                    try buffer.add(.Div, i, i+1);
                    state = .Code;
                },
            },
            .Code => switch (c) {
                ' ', '\t', '\n', '\r' => {i += 1;},
                ',' =>  {try buffer.add(.Comma,             i, i+1); i += 1;},
                '.' =>  {try buffer.add(.Period,            i, i+1); i += 1;},
                ':' =>  {try buffer.add(.Colon,             i, i+1); i += 1;},
                ';' =>  {try buffer.add(.Semicolon,         i, i+1); i += 1;},
                '(' =>  {try buffer.add(.LeftParen,         i, i+1); i += 1;},
                ')' =>  {try buffer.add(.RightParen,        i, i+1); i += 1;},
                '[' =>  {try buffer.add(.LeftBracket,       i, i+1); i += 1;},
                ']' =>  {try buffer.add(.RightBracket,      i, i+1); i += 1;},
                '{' =>  {try buffer.add(.LeftBrace,         i, i+1); i += 1;},
                '}' =>  {try buffer.add(.RightBrace,        i, i+1); i += 1;},
                '&' =>  {try buffer.add(.Align,             i, i+1); i += 1;},
                '`' =>  {
                    state = .EndCodeDelim1;
                    i += 1;
                },
                '<' =>  {
                    multi_char_token_start = i;
                    state = .LessThan;
                    i += 1;
                },
                '>' =>  {
                    multi_char_token_start = i;
                    state = .GreaterThan;
                    i += 1;
                },
                '+' =>  {try buffer.add(.Add,               i, i+1); i += 1;},
                '-' =>  {
                    multi_char_token_start = i;
                    state = .Minus;
                    i += 1;
                },
                '*' =>  {try buffer.add(.Mul,               i, i+1); i += 1;},
                '/' =>  {
                    multi_char_token_start = i;
                    state = .CommentOrDiv;
                    i += 1;
                },
                '=' =>  {
                    multi_char_token_start = i;
                    state = .Equal;
                    i += 1;
                },
                '^' =>  {try buffer.add(.Superscript,       i, i+1); i += 1;},
                '_' =>  {try buffer.add(.Subscript,         i, i+1); i += 1;},
                '\\' => {try buffer.add(.Backslash,         i, i+1); i += 1;},
                '0' ... '9' => {
                    multi_char_token_start = i;
                    state = .Number;
                    i += 1;
                },
                'a' ... 'z','A' ... 'Z' => {
                    multi_char_token_start = i;
                    state = .Identifier;
                    return_state = .Code;
                    i += 1;
                },
                '"' => {
                    multi_char_token_start = i+1;
                    state = .String;
                    i += 1;
                },
                else => {
                    var codepoint: u21 = undefined;
                    var len: u3 = undefined;

                    if (buf[i] < 128) {
                        codepoint = @as(u21, buf[i]);
                        len = 1;
                    } else {
                        len = try std.unicode.utf8ByteSequenceLength(buf[i]);
                        // invalid utf8
                        if (i + len >= buf.len) {
                            unreachable;
                        }

                        const bytes = buf[i..(i+len)];
                        codepoint = switch (len) {
                            2 => try std.unicode.utf8Decode2(bytes),
                            3 => try std.unicode.utf8Decode3(bytes),
                            4 => try std.unicode.utf8Decode4(bytes),
                            else => unreachable,
                        };
                    }

                    if (ziglyph.isAlphabetic(codepoint)) {
                        multi_char_token_start = i;
                        state = .Identifier;
                        return_state = .Code;
                        i += len;
                    } else {
                        log.errAt(filename, buf, i, "", "Unrecognized token");
                        has_invalid_tokens = true;
                        i += 1;
                    }
                },
            },
            .Equal => switch (c) {
                '=' => {state = .RightArrow; i += 1;},
                else => {state = .Code; try buffer.add(.Equal, multi_char_token_start, i);}
            },
            .RightArrow => switch (c) {
                '>' => {state = .Code; try buffer.add(.RightImp, multi_char_token_start, i+1); i += 1;},
                else => {}
            },
            .LeftArrow => switch (c) {
                '=' => {state = .Code; try buffer.add(.LeftImp, multi_char_token_start, i+1); i += 1;},
                '>' => {state = .Code; try buffer.add(.Eqv, multi_char_token_start, i+1); i += 1;},
                else => {state = .Code; try buffer.add(.LessThanEqual, multi_char_token_start, i);},
            },
            .Minus => switch (c) {
                '>' => {state = .Code; try buffer.add(.To, multi_char_token_start, i+1); i += 1;},
                else => {state = .Code; try buffer.add(.Sub, multi_char_token_start, i);},
            },
            .LessThan => switch (c) {
                '=' => {state = .LeftArrow; i += 1;},
                else => {state = .Code; try buffer.add(.LeftAngleBracket, multi_char_token_start, i);},
            },
            .GreaterThan => switch (c) {
                '=' => {state = .Code; try buffer.add(.GreaterThanEqual, multi_char_token_start, i+1); i += 1;},
                else => {state = .Code; try buffer.add(.RightAngleBracket, multi_char_token_start, i);},
            },
            .Number => switch (c) {
                '0' ... '9' => {
                    i += 1;
                },
                else => {
                    try buffer.add(.Number, multi_char_token_start, i);
                    state = .Code;
                },
            },
            .Identifier => {
                var codepoint: u21 = undefined;
                var len: u3 = undefined;

                if (buf[i] < 128) {
                    codepoint = @as(u21, buf[i]);
                    len = 1;
                } else {
                    len = try std.unicode.utf8ByteSequenceLength(buf[i]);
                    // invalid utf8
                    if (i + len >= buf.len) {
                        unreachable;
                    }

                    const bytes = buf[i..(i+len)];
                    codepoint = switch (len) {
                        2 => try std.unicode.utf8Decode2(bytes),
                        3 => try std.unicode.utf8Decode3(bytes),
                        4 => try std.unicode.utf8Decode4(bytes),
                        else => unreachable,
                    };
                }

                if (ziglyph.isAlphaNum(codepoint)) {
                    i += len;
                } else {
                    const key = keywords.get(buf[multi_char_token_start..i]);
                    if (key != null) {
                        try buffer.add(key.?, multi_char_token_start, i);
                    } else {
                        try buffer.add(.Identifier, multi_char_token_start, i);
                    }
                    state = return_state.?;
                }
            },
            .String => switch (c) {
                '"' => {
                    try buffer.add(.String, multi_char_token_start, i);
                    i += 1;
                    state = .Code;
                },
                else => {
                    i += 1;
                }
            },
        }
    }

    if (has_invalid_tokens)
        return LexError.InvalidToken;

    return buffer;
}
