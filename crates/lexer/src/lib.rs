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

    // Literals
    Integer(i32),          // e.g. 123, 1000
    Fixed(i32),            // 16.16 fixed point format
    StringLiteral(String), // e.g. "\"hello\""
    StringStart(String),   // Start of interpolated string e.g. "hello {"
    StringMiddle(String),  // Middle of interpolated string e.g. "} world {"
    StringEnd(String),     // End of interpolated string e.g. "} world"
    Identifier(String),    // e.g. "some_identifier"

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
    PlusEqual,          // '+='
    MinusEqual,         // '-='
    StarEqual,          // '*='
    SlashEqual,         // '/='
    PercentEqual,       // '%='
    AmpersandEqual,     // '&='
    PipeEqual,          // '|='
    CaretEqual,         // '^='
    EqualEqual,         // '=='
    BangEqual,          // '!='
    LessEqual,          // '<='
    GreaterEqual,       // '>='
    ThinArrow,          // '->'
    FatArrow,           // '=>'
    DotDot,             // '..'
    DotDotEqual,        // '..='
    AmpersandAmpersand, // '&&'
    PipePipe,           // '||'
    QuestionQuestion,   // '??'
    Question,           // '?'

    // Really an error token
    Unknown(char),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub start: u32,
    pub len: u16,
}

// Maximum number of tokens we can look ahead
const MAX_LOOKAHEAD: usize = 4;

pub struct Lexer<'a> {
    src: &'a [u8],
    len: usize,              // length of `src`
    pos: usize,              // current byte index into `src`
    token_cache: Vec<Token>, // Dynamic cache for peeking, limited to MAX_LOOKAHEAD
    string_mode: StringMode,
    seen_interpolation: bool,
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
                    let slice = &self.src[start..=self.pos];
                    let text = unsafe { std::str::from_utf8_unchecked(slice) }.to_owned();
                    self.pos += 1; // consume closing quote
                    return Token {
                        kind: TokenKind::StringLiteral(text),
                        start: start as u32,
                        len: (self.pos - start) as u16,
                    };
                }
            }
            self.pos += 1;
        }

        // Unterminated string
        Token {
            kind: TokenKind::Unknown('"'),
            start: start as u32,
            len: (self.pos - start) as u16,
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

        // Handle different token types
        match b {
            // String literals
            b'"' => self.lex_regular_string(start),
            b'\'' => {
                self.pos += 1;
                self.string_mode = StringMode::InString;
                self.seen_interpolation = false;
                self.lex_inside_string()
            }

            // Numbers
            b'0'..=b'9' | b'-' => {
                if b == b'-'
                    && (self.pos + 1 >= self.len || !self.src[self.pos + 1].is_ascii_digit())
                {
                    self.pos += 1;
                    return Token {
                        kind: TokenKind::Minus,
                        start: start as u32,
                        len: 1,
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
                            start: start as u32,
                            len: (self.pos - start) as u16,
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

                Token {
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
                    start: start as u32,
                    len: (self.pos - start) as u16,
                }
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
                let slice = &self.src[start..self.pos];
                // SAFETY: slice contains only ASCII chars, so it's valid UTF-8
                let text = unsafe { std::str::from_utf8_unchecked(slice) }.to_owned();
                Token {
                    kind: TokenKind::Identifier(text),
                    start: start as u32,
                    len: (self.pos - start) as u16,
                }
            }

            // Single-character tokens
            b'(' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::LParen,
                    start: start as u32,
                    len: 1,
                }
            }
            b')' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::RParen,
                    start: start as u32,
                    len: 1,
                }
            }
            b'{' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::LBrace,
                    start: start as u32,
                    len: 1,
                }
            }
            b'}' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::RBrace,
                    start: start as u32,
                    len: 1,
                }
            }
            b'[' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::LBracket,
                    start: start as u32,
                    len: 1,
                }
            }
            b']' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::RBracket,
                    start: start as u32,
                    len: 1,
                }
            }
            b'#' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Hash,
                    start: start as u32,
                    len: 1,
                }
            }
            b',' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Comma,
                    start: start as u32,
                    len: 1,
                }
            }
            b'.' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'.' => {
                            self.pos += 1;
                            if self.pos < self.len && self.src[self.pos] == b'=' {
                                self.pos += 1;
                                Token {
                                    kind: TokenKind::DotDotEqual,
                                    start: start as u32,
                                    len: 2,
                                }
                            } else {
                                Token {
                                    kind: TokenKind::DotDot,
                                    start: start as u32,
                                    len: 2,
                                }
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Dot,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Dot,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b';' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Semicolon,
                    start: start as u32,
                    len: 1,
                }
            }
            b':' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Colon,
                    start: start as u32,
                    len: 1,
                }
            }

            // Operators that might be compound
            b'+' => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Plus,
                    start: start as u32,
                    len: 1,
                }
            }
            b'?' => match self.src[self.pos] {
                b'?' => {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::QuestionQuestion,
                        start: start as u32,
                        len: 2,
                    }
                }
                _ => Token {
                    kind: TokenKind::Question,
                    start: start as u32,
                    len: 1,
                },
            },
            b'-' => match self.src[self.pos] {
                b'=' => {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::MinusEqual,
                        start: start as u32,
                        len: 2,
                    }
                }
                b'>' => {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::ThinArrow,
                        start: start as u32,
                        len: 2,
                    }
                }
                _ => Token {
                    kind: TokenKind::Minus,
                    start: start as u32,
                    len: 1,
                },
            },
            b'*' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::StarEqual,
                        start: start as u32,
                        len: 2,
                    }
                } else {
                    Token {
                        kind: TokenKind::Star,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'/' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::SlashEqual,
                        start: start as u32,
                        len: 2,
                    }
                } else {
                    Token {
                        kind: TokenKind::Slash,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'%' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::PercentEqual,
                        start: start as u32,
                        len: 2,
                    }
                } else {
                    Token {
                        kind: TokenKind::Percent,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'=' => {
                self.pos += 1;
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::EqualEqual,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        b'>' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::FatArrow,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Equal,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Equal,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'!' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    Token {
                        kind: TokenKind::BangEqual,
                        start: start as u32,
                        len: 2,
                    }
                } else {
                    Token {
                        kind: TokenKind::Bang,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'<' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::LessEqual,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Less,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Less,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'>' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::GreaterEqual,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Greater,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Greater,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'&' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::AmpersandEqual,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        b'&' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::AmpersandAmpersand,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Ampersand,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Ampersand,
                        start: start as u32,
                        len: 1,
                    }
                }
            }
            b'|' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::PipeEqual,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        b'|' => {
                            self.pos += 1;
                            Token {
                                kind: TokenKind::PipePipe,
                                start: start as u32,
                                len: 2,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Pipe,
                            start: start as u32,
                            len: 1,
                        },
                    }
                } else {
                    Token {
                        kind: TokenKind::Pipe,
                        start: start as u32,
                        len: 1,
                    }
                }
            }

            // Unknown character
            _ => {
                self.pos += 1;
                Token {
                    kind: TokenKind::Unknown(b as char),
                    start: start as u32,
                    len: 1,
                }
            }
        }
    }

    /// When we are inside a string interpolation block `{...}`
    /// Should lex as normal
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
                start: start as u32,
                len: 1,
            }
        } else {
            self.lex_normal()
        }
    }

    /// When we are in a string part of the string interpolation
    /// The string parts between the interpolation string `'...'` and the `{...}` parts.
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
                        let slice = &self.src[start..self.pos];
                        let text = String::from_utf8_lossy(slice).to_string();
                        self.string_mode = StringMode::InInterpolation;
                        let was_seen = self.seen_interpolation;
                        self.seen_interpolation = true;
                        return Token {
                            kind: if was_seen {
                                TokenKind::StringMiddle(text)
                            } else {
                                TokenKind::StringStart(text)
                            },
                            start: start as u32,
                            len: (self.pos - start) as u16,
                        };
                    }
                    b'\'' => {
                        let slice = &self.src[start..self.pos];
                        let text = String::from_utf8_lossy(slice).to_string();
                        self.pos += 1;
                        self.string_mode = StringMode::Normal;
                        self.seen_interpolation = false;
                        return Token {
                            kind: TokenKind::StringEnd(text),
                            start: start as u32,
                            len: (self.pos - start) as u16,
                        };
                    }
                    _ => {}
                }
            }
            self.pos += 1;
        }

        Token {
            kind: TokenKind::Unknown('\''),
            start: start as u32,
            len: (self.pos - start) as u16,
        }
    }

    fn skip_whitespace(&mut self) -> Option<Token> {
        while self.pos < self.len && self.src[self.pos].is_ascii_whitespace() {
            self.pos += 1;
        }
        if self.pos == self.len {
            Some(Token {
                kind: TokenKind::EOF,
                start: self.pos as u32,
                len: 0,
            })
        } else {
            None
        }
    }
}
