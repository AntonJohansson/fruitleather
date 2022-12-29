const std = @import("std");
const log = @import("log.zig");

pub const Token = enum {
    // 1-char
    Comma,
    Colon,
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
    Comment,
    // keywords
    Type,
    Var,
    Op,
    In,
    Sum,
    Prod,
    // multi-char
    Number,
    Identifier,
    String,

    Unknown,
};

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
    Number,
    Identifier,
    String,
};

pub const LexError = error {
    InvalidToken,
};

pub fn lex(allocator: std.mem.Allocator, filename: []const u8, buf: []const u8) !TokenBuffer {
    var buffer = try TokenBuffer.init(allocator);

    var state: LexerState = .Start;
    var multi_char_token_start: usize = 0;

    const keywords = std.ComptimeStringMap(Token, .{
        .{"type",   .Type},
        .{"var",    .Var},
        .{"op",     .Op},
        .{"in",     .In},
        .{"sum",    .Sum},
        .{"prod",   .Prod},
    });

    var has_invalid_tokens = false;

    var i: usize = 0;
    while (i < buf.len) {
        const c = buf[i];
        switch (state) {
            .Start => switch (c) {
                ' ', '\t', '\n', '\r' => {i += 1;},
                ',' =>  {try buffer.add(.Comma,             i, i+1); i += 1;},
                ':' =>  {try buffer.add(.Colon,             i, i+1); i += 1;},
                '(' =>  {try buffer.add(.LeftParen,         i, i+1); i += 1;},
                ')' =>  {try buffer.add(.RightParen,        i, i+1); i += 1;},
                '[' =>  {try buffer.add(.LeftBracket,       i, i+1); i += 1;},
                ']' =>  {try buffer.add(.RightBracket,      i, i+1); i += 1;},
                '{' =>  {try buffer.add(.LeftBrace,         i, i+1); i += 1;},
                '}' =>  {try buffer.add(.RightBrace,        i, i+1); i += 1;},
                '<' =>  {try buffer.add(.LeftAngleBracket,  i, i+1); i += 1;},
                '>' =>  {try buffer.add(.RightAngleBracket, i, i+1); i += 1;},
                '+' =>  {try buffer.add(.Add,               i, i+1); i += 1;},
                '-' =>  {try buffer.add(.Sub,               i, i+1); i += 1;},
                '*' =>  {try buffer.add(.Mul,               i, i+1); i += 1;},
                '/' =>  {try buffer.add(.Div,               i, i+1); i += 1;},
                '=' =>  {try buffer.add(.Equal,             i, i+1); i += 1;},
                '^' =>  {try buffer.add(.Superscript,       i, i+1); i += 1;},
                '_' =>  {try buffer.add(.Subscript,         i, i+1); i += 1;},
                '\\' => {try buffer.add(.Backslash,         i, i+1); i += 1;},
                '#' => {
                    while (i < buf.len) {
                        switch (buf[i]) {
                            '\n', '\r' => break,
                            else => i += 1
                        }
                    }
                },
                '0' ... '9' => {
                    multi_char_token_start = i;
                    state = .Number;
                    i += 1;
                },
                'a' ... 'z','A' ... 'Z' => {
                    multi_char_token_start = i;
                    state = .Identifier;
                    i += 1;
                },
                '"' => {
                    multi_char_token_start = i+1;
                    state = .String;
                    i += 1;
                },
                else => {
                    log.errAt(filename, buf, i, "", "Unrecognized token");
                    has_invalid_tokens = true;
                    i += 1;
                },
            },
            .Number => switch (c) {
                '0' ... '9' => {
                    i += 1;
                },
                else => {
                    try buffer.add(.Number, multi_char_token_start, i);
                    state = .Start;
                },
            },
            .Identifier => switch (c) {
                'a' ... 'z', 'A' ... 'Z', '0' ... '9' => {
                    i += 1;
                },
                else => {
                    const key = keywords.get(buf[multi_char_token_start..i]);
                    if (key != null) {
                        try buffer.add(key.?, multi_char_token_start, i);
                    } else {
                        try buffer.add(.Identifier, multi_char_token_start, i);
                    }
                    state = .Start;
                },
            },
            .String => switch (c) {
                '"' => {
                    try buffer.add(.String, multi_char_token_start, i);
                    i += 1;
                    state = .Start;
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
