/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

#[derive(Clone, PartialEq)]
enum StringMode {
    Normal,
    InString,
    InInterpolation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // End-of-file
    EOF,

    // Comments
    DocComment,   // /// doc comments
    LineComment,  // // line comments
    BlockComment, // /* block comments */

    // Literals
    Integer(i32),  // e.g. 123, 1000
    Fixed(i32),    // 16.16 fixed point format
    StringLiteral, // e.g. "\"hello\""
    StringStart,   // Start of interpolated string e.g. "hello {"
    StringMiddle,  // Middle of interpolated string e.g. "} world {"
    StringEnd,     // End of interpolated string e.g. "} world"
    Identifier,    // e.g. "some_identifier"

    // Single-char operators
    LParen,    // '('
    RParen,    // ')'
    LBrace,    // '{'
    RBrace,    // '}'
    LBracket,  // '['
    RBracket,  // ']'
    Hash,      // '#'
    Plus,      // '+'
    Minus,     // '-'
    Star,      // '*'
    Slash,     // '/'
    Percent,   // '%'
    Equal,     // '='
    Less,      // '<'
    Greater,   // '>'
    Bang,      // '!'
    Ampersand, // '&'
    Pipe,      // '|'
    Comma,     // ','
    Dot,       // '.'
    Semicolon, // ';'
    Colon,     // ':'

    // Compound operators
    PlusEqual,    // '+='
    MinusEqual,   // '-='
    StarEqual,    // '*='
    SlashEqual,   // '/='
    PercentEqual, // '%='
    EqualEqual,   // '=='
    BangEqual,    // '!='
    LessEqual,    // '<='
    GreaterEqual, // '>='

    // Two character:
    ThinArrow,          // '->'
    FatArrow,           // '=>'
    DotDot,             // '..'
    AmpersandAmpersand, // '&&'
    PipePipe,           // '||'
    QuestionQuestion,   // '??'
    Question,           // '?'
    ColonColon,         // '::'

    // Three character
    DotDotEqual, // '..='

    // Really an error token
    Unknown(char),
    Type,
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    pub start: u32,
    pub len: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub node: Node,
}

// Maximum number of tokens we can look ahead
const MAX_LOOKAHEAD: usize = 4;

pub struct Lexer<'a> {
    src: &'a [u8],
    len: usize,               // length of `src`
    pos: usize,               // current byte index into `src`
    token_cache: Vec<Token>,  // Dynamic cache for peeking, limited to MAX_LOOKAHEAD
    string_mode: StringMode,  // Support for string interpolation
    seen_interpolation: bool, // To determine if it is StringStart or StringMiddle
}

impl<'a> Lexer<'a> {
    #[inline]
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        let bytes = input.as_bytes();
        Lexer {
            src: bytes,
            len: bytes.len(),
            pos: 0,
            token_cache: Vec::with_capacity(MAX_LOOKAHEAD),
            string_mode: StringMode::Normal,
            seen_interpolation: false,
        }
    }
}

impl Lexer<'_> {
    #[must_use]
    pub fn get_string(&self, token: &Node) -> String {
        let slice = &self.src[token.start as usize..token.start as usize + token.len as usize];
        unsafe { std::str::from_utf8_unchecked(slice) }.to_owned()
    }

    /// Peek at the next token without consuming it.
    /// Subsequent calls to `peek()` return the same token.
    #[inline]
    pub fn peek(&mut self) -> &Token {
        self.peek_n(0)
    }

    /// Peek at the nth token ahead without consuming any tokens.
    /// n=0 means the next token
    /// Returns EOF token if peeking beyond the end of input.
    /// # Panics
    /// if n >= `MAX_LOOKAHEAD`
    #[inline]
    pub fn peek_n(&mut self, n: usize) -> &Token {
        assert!(
            n < MAX_LOOKAHEAD,
            "Cannot peek more than {MAX_LOOKAHEAD} tokens ahead"
        );

        while self.token_cache.len() <= n {
            let token = self.next_token_internal();
            self.token_cache.push(token);
        }

        &self.token_cache[n]
    }

    /// Return the next token, consuming it
    #[inline]
    pub fn next_token(&mut self) -> Token {
        if self.token_cache.is_empty() {
            self.next_token_internal()
        } else {
            self.token_cache.remove(0)
        }
    }

    // Helper function to convert float string to 16.16 fixed point
    fn parse_fixed_point(
        integer_part: &str,
        fractional_part: &str,
        is_negative: bool,
    ) -> Option<i32> {
        // Parse integer part (limited to 16 bits)
        let int_val = integer_part.parse::<i32>().ok()?;
        if !(-32768..=32767).contains(&int_val) {
            return None; // Integer part too large for 16.16
        }

        // Parse fractional part
        let mut frac_val = 0i32;
        let mut scale = 0x10000; // 2^16
        for digit in fractional_part.chars() {
            if !digit.is_ascii_digit() {
                continue; // Skip underscores
            }
            let digit_val = i32::from(digit as u8 - b'0');
            scale /= 10;
            frac_val += digit_val * scale;
        }

        let result = (int_val.abs() << 16) | frac_val;
        Some(if is_negative { -result } else { result })
    }

    /// Lex a normal string `"..."` with escape support
    /// TODO: more escape sequences
    fn lex_regular_string(&mut self, start: usize) -> Token {
        let mut escaped = false;
        self.pos += 1;

        while self.pos < self.len {
            let c = self.src[self.pos];
            if escaped {
                escaped = false;
            } else {
                if c == b'\\' {
                    escaped = true;
                    self.pos += 1;
                    continue;
                }
                if c == b'"' {
                    self.pos += 1; // consume closing quote
                    return Token {
                        kind: TokenKind::StringLiteral,
                        node: Node {
                            start: start as u32,
                            len: (self.pos - start) as u16,
                        },
                    };
                }
            }
            self.pos += 1;
        }

        // Unterminated string
        Token {
            kind: TokenKind::Unknown('"'),
            node: Node {
                start: start as u32,
                len: (self.pos - start) as u16,
            },
        }
    }

    fn next_token_internal(&mut self) -> Token {
        match self.string_mode {
            StringMode::InString => self.lex_inside_string(),
            StringMode::InInterpolation => self.lex_inside_interpolation(),
            StringMode::Normal => self.lex_normal(),
        }
    }

    #[inline]
    #[allow(clippy::too_many_lines, clippy::cognitive_complexity)]
    fn lex_normal(&mut self) -> Token {
        if let Some(end_token) = self.skip_whitespace() {
            return end_token;
        }

        let start = self.pos;
        let b = self.src[self.pos];

        // First match: Complex tokens that manage their own position
        match b {
            // Comments
            b'/' if self.pos + 1 < self.len => match self.src[self.pos + 1] {
                b'/' => return self.lex_line_comment(start),
                b'*' => return self.lex_block_comment(start),
                _ => {}
            },

            // String literals
            b'"' => return self.lex_regular_string(start),
            b'\'' => {
                self.pos += 1;
                self.string_mode = StringMode::InString;
                self.seen_interpolation = false;
                return self.lex_inside_string();
            }

            // Single character operators
            b'*' => {
                self.pos += 1;
                return Token {
                    kind: TokenKind::Star,
                    node: Node {
                        start: start as u32,
                        len: 1,
                    },
                };
            }

            // Numbers
            b'0'..=b'9' | b'-' => {
                // Handle standalone minus operator
                if b == b'-'
                    && (self.pos + 1 >= self.len || !self.src[self.pos + 1].is_ascii_digit())
                {
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Minus,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    };
                }

                let starts_with_minus = b == b'-';
                if starts_with_minus {
                    self.pos += 1;
                }

                // Parse digits before decimal point, allowing underscores
                while self.pos < self.len {
                    let c = self.src[self.pos];
                    if !c.is_ascii_digit() && c != b'_' && c != b'.' {
                        break;
                    }
                    if c == b'.' {
                        break;
                    }
                    self.pos += 1;
                }

                // Check for decimal point
                let is_float = self.pos < self.len && self.src[self.pos] == b'.';
                if is_float {
                    self.pos += 1; // consume the dot

                    // Must have at least one digit after the dot
                    if self.pos >= self.len || !self.src[self.pos].is_ascii_digit() {
                        // Invalid float, no digits after decimal point
                        return Token {
                            kind: TokenKind::Unknown('.'),
                            node: Node {
                                start: start as u32,
                                len: (self.pos - start) as u16,
                            },
                        };
                    }

                    // Parse digits after decimal point, allowing underscores
                    while self.pos < self.len {
                        let c = self.src[self.pos];
                        if !c.is_ascii_digit() && c != b'_' {
                            break;
                        }
                        self.pos += 1;
                    }
                }

                let slice = &self.src[start..self.pos];
                // Remove underscores `_` and create the final string
                let text: String = slice
                    .iter()
                    .filter(|&&c| c != b'_')
                    .map(|&c| c as char)
                    .collect();

                return Token {
                    kind: if is_float {
                        // Split into integer and fractional parts for fixed point 16.16
                        let parts: Vec<&str> = text.split('.').collect();
                        if parts.len() == 2 {
                            let is_negative = parts[0].starts_with('-');
                            let int_part = if is_negative {
                                &parts[0][1..]
                            } else {
                                parts[0]
                            };

                            Self::parse_fixed_point(int_part, parts[1], is_negative)
                                .map_or(TokenKind::Unknown('.'), TokenKind::Fixed)
                        } else {
                            TokenKind::Unknown('.')
                        }
                    } else {
                        // TODO: Maybe have actual error handling?
                        text.parse::<i32>()
                            .map_or(TokenKind::Unknown('0'), TokenKind::Integer)
                    },
                    node: Node {
                        start: start as u32,
                        len: (self.pos - start) as u16,
                    },
                };
            }

            // Identifiers
            b'_' | b'a'..=b'z' => {
                self.pos += 1;
                while self.pos < self.len {
                    let c = self.src[self.pos];
                    if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == b'_') {
                        break;
                    }
                    self.pos += 1;
                }
                return Token {
                    kind: TokenKind::Identifier,
                    node: Node {
                        start: start as u32,
                        len: (self.pos - start) as u16,
                    },
                };
            }

            // Types
            b'A'..=b'Z' => {
                self.pos += 1;
                while self.pos < self.len {
                    let c = self.src[self.pos];
                    if c.is_ascii_uppercase() {
                        // it is a constant
                        while self.pos < self.len {
                            let c = self.src[self.pos];
                            if !(c.is_ascii_uppercase() || c.is_ascii_digit() || c == b'_') {
                                break;
                            }
                            self.pos += 1;
                        }
                        return Token {
                            kind: TokenKind::Constant,
                            node: Node {
                                start: start as u32,
                                len: (self.pos - start) as u16,
                            },
                        };
                    } else if c.is_ascii_lowercase() {
                        // It is a type
                        while self.pos < self.len {
                            let c = self.src[self.pos];
                            if !(c.is_ascii_lowercase()
                                || c.is_ascii_uppercase()
                                || c.is_ascii_digit())
                            {
                                break;
                            }
                            self.pos += 1;
                        }
                        return Token {
                            kind: TokenKind::Type,
                            node: Node {
                                start: start as u32,
                                len: (self.pos - start) as u16,
                            },
                        };
                    }
                }
                return Token {
                    kind: TokenKind::Type,
                    node: Node {
                        start: start as u32,
                        len: (self.pos - start) as u16,
                    },
                };
            }

            _ => {}
        }

        // Second match: Simple 1-3 character tokens
        self.pos += 1; // Always consume at least one character for simple tokens
        match b {
            // Single-character tokens
            b'(' => Token {
                kind: TokenKind::LParen,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b')' => Token {
                kind: TokenKind::RParen,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b'{' => Token {
                kind: TokenKind::LBrace,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b'}' => Token {
                kind: TokenKind::RBrace,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b'[' => Token {
                kind: TokenKind::LBracket,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b']' => Token {
                kind: TokenKind::RBracket,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b'#' => Token {
                kind: TokenKind::Hash,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b',' => Token {
                kind: TokenKind::Comma,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
            b';' => Token {
                kind: TokenKind::Semicolon,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },

            // Potentially compound tokens
            b':' => {
                if self.pos < self.len && self.src[self.pos] == b':' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::ColonColon,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Colon,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'.' => {
                if self.pos < self.len && self.src[self.pos] == b'.' {
                    self.pos += 1;
                    if self.pos < self.len && self.src[self.pos] == b'=' {
                        self.pos += 1;
                        Token {
                            kind: TokenKind::DotDotEqual,
                            node: Node {
                                start: start as u32,
                                len: 3,
                            },
                        }
                    } else {
                        Token {
                            kind: TokenKind::DotDot,
                            node: Node {
                                start: start as u32,
                                len: 2,
                            },
                        }
                    }
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'<' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::LessEqual,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Less,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'>' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::GreaterEqual,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Greater,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'&' => {
                if self.pos < self.len && self.src[self.pos] == b'&' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::AmpersandAmpersand,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Ampersand,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'|' => {
                if self.pos < self.len && self.src[self.pos] == b'|' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::PipePipe,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Pipe,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'=' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::EqualEqual,
                                node: Node {
                                    start: start as u32,
                                    len: 2,
                                },
                            }
                        }
                        b'>' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::FatArrow,
                                node: Node {
                                    start: start as u32,
                                    len: 2,
                                },
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Equal,
                            node: Node {
                                start: start as u32,
                                len: 1,
                            },
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Equal,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'!' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::BangEqual,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Bang,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'?' => {
                if self.pos < self.len && self.src[self.pos] == b'?' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::QuestionQuestion,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Question,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'/' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::SlashEqual,
                        node: Node {
                            start: start as u32,
                            len: 2,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Slash,
                        node: Node {
                            start: start as u32,
                            len: 1,
                        },
                    }
                }
            }
            b'+' => Token {
                kind: TokenKind::Plus,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },

            // Unknown character
            _ => Token {
                kind: TokenKind::Unknown(b as char),
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            },
        }
    }

    /// When we are inside a string interpolation block `{...}`
    /// Should lex as normal until it finds the end `'`
    fn lex_inside_interpolation(&mut self) -> Token {
        if let Some(end_token) = self.skip_whitespace() {
            return end_token;
        }

        let start = self.pos;
        let b = self.src[self.pos];

        // If we see a closing brace, switch back to string mode
        if b == b'}' {
            self.pos += 1;
            self.string_mode = StringMode::InString;
            Token {
                kind: TokenKind::RBrace,
                node: Node {
                    start: start as u32,
                    len: 1,
                },
            }
        } else {
            self.lex_normal()
        }
    }

    /// When we are in a string part of the string interpolation
    /// The string parts between the interpolation string `'...'` and the `{...}` parts.
    /// Continues to build a string until it finds a starting `{` or and end `'`.
    fn lex_inside_string(&mut self) -> Token {
        let start = self.pos;
        let mut escaped = false;

        while self.pos < self.len {
            let c = self.src[self.pos];
            if escaped {
                escaped = false;
            } else {
                match c {
                    b'\\' => {
                        escaped = true;
                        self.pos += 1;
                        continue;
                    }
                    b'{' => {
                        self.string_mode = StringMode::InInterpolation;
                        let was_seen = self.seen_interpolation;
                        self.seen_interpolation = true;
                        return Token {
                            kind: if was_seen {
                                TokenKind::StringMiddle
                            } else {
                                TokenKind::StringStart
                            },
                            node: Node {
                                start: start as u32,
                                len: (self.pos - start) as u16,
                            },
                        };
                    }
                    b'\'' => {
                        self.pos += 1;
                        self.string_mode = StringMode::Normal;
                        self.seen_interpolation = false;
                        return Token {
                            kind: TokenKind::StringEnd,
                            node: Node {
                                start: start as u32,
                                len: (self.pos - start - 1) as u16,
                            }, // extra -1 to not contain the end `'`
                        };
                    }
                    _ => {}
                }
            }
            self.pos += 1;
        }

        Token {
            kind: TokenKind::Unknown('\''),
            node: Node {
                start: start as u32,
                len: (self.pos - start) as u16,
            },
        }
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        while self.pos < self.len && self.src[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
        if self.pos == self.len {
            Some(Token {
                kind: TokenKind::EOF,
                node: Node {
                    start: self.pos as u32,
                    len: 0,
                },
            })
        } else {
            None
        }
    }

    // Lex a line comment starting with //
    fn lex_line_comment(&mut self, start: usize) -> Token {
        self.pos += 2;

        let is_doc = self.pos < self.len && self.src[self.pos] == b'/';
        if is_doc {
            self.pos += 1;
        }

        // Read until newline or EOF
        while self.pos < self.len && self.src[self.pos] != b'\n' {
            self.pos += 1;
        }

        Token {
            kind: if is_doc {
                TokenKind::DocComment
            } else {
                TokenKind::LineComment
            },
            node: Node {
                start: start as u32,
                len: (self.pos - start) as u16,
            },
        }
    }

    // Lex a block comment `/* ... */`
    fn lex_block_comment(&mut self, start: usize) -> Token {
        self.pos += 2; // Skip `/*`
        while self.pos + 1 < self.len {
            // Find end
            if self.src[self.pos] == b'*' && self.src[self.pos + 1] == b'/' {
                self.pos += 2;
                return Token {
                    kind: TokenKind::BlockComment,
                    node: Node {
                        start: start as u32,
                        len: (self.pos - start) as u16,
                    },
                };
            }
            self.pos += 1;
        }

        // Unterminated block comment
        Token {
            kind: TokenKind::Unknown('*'),
            node: Node {
                start: start as u32,
                len: (self.pos - start) as u16,
            },
        }
    }
}
