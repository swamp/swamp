#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // End-of-file
    EOF,

    // Literals
    Integer(i32),          // e.g. 123, 1000
    Fixed(i32),            // 16.16 fixed point format
    StringLiteral(String), // e.g. "\"hello\""
    Identifier(String),    // e.g. "some_identifier"

    // Single-char operators
    LParen,    // '('
    RParen,    // ')'
    LBrace,    // '{'
    RBrace,    // '}'
    LBracket,  // '['
    RBracket,  // ']'
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
    Caret,     // '^'
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
    EqualGreater,       // '=>'
    MinusGreater,       // '->'
    AmpersandAmpersand, // '&&'
    PipePipe,           // '||'

    // Really an error token
    Unknown(char),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: u32,
    pub len: u16,
}

pub struct Lexer<'a> {
    src: &'a [u8],
    len: usize, // length of `src`
    pos: usize, // current byte index into `src`
}

impl<'a> Lexer<'a> {
    #[inline]
    #[must_use]
    pub const fn new(input: &'a str) -> Self {
        let bytes = input.as_bytes();
        Lexer {
            src: bytes,
            len: bytes.len(),
            pos: 0,
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

    /// Return the next token (advancing `self.pos`).
    #[inline]
    #[allow(clippy::too_many_lines)]
    pub fn next_token(&mut self) -> Token {
        while self.pos < self.len {
            match self.src[self.pos] {
                b' ' | b'\t' | b'\r' | b'\n' => self.pos += 1,
                _ => break,
            }
        }

        if self.pos >= self.len {
            return Token {
                kind: TokenKind::EOF,
                start: self.pos as u32,
                len: 0,
            };
        }

        let start = self.pos;
        let b = self.src[self.pos];

        // Handle numbers (both integer and float)
        if b.is_ascii_digit()
            || (b == b'-' && self.pos + 1 < self.len && self.src[self.pos + 1].is_ascii_digit())
        {
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
                start: start as u32,
                len: (self.pos - start) as u16,
            };
        }

        // Identifier
        // TODO: is `contains` faster than `is_ascii_lowercase` and `is_ascii_digit`?
        if b == b'_' || b.is_ascii_lowercase() {
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
            return Token {
                kind: TokenKind::Identifier(text),
                start: start as u32,
                len: (self.pos - start) as u16,
            };
        }

        // String literal
        if b == b'"' {
            self.pos += 1; // consume the opening quote
            let mut escaped = false;
            while self.pos < self.len {
                let c = self.src[self.pos];
                if !escaped && c == b'\\' {
                    // TODO: Handle escape characters
                    escaped = true;
                    self.pos += 1;
                    // If next char exists, treat it as data (we'll decode later or let the AST do it).
                    if self.pos < self.len {
                        self.pos += 1;
                    }
                    escaped = false;
                    continue;
                }
                if c == b'"' && !escaped {
                    let slice = &self.src[start..=self.pos];
                    let text = unsafe { std::str::from_utf8_unchecked(slice) }.to_owned();
                    self.pos += 1; // consume closing quote
                    return Token {
                        kind: TokenKind::StringLiteral(text),
                        start: start as u32,
                        len: slice.len() as u16,
                    };
                }
                self.pos += 1;
            }

            // TODO: Maybe better error handling
            return Token {
                kind: TokenKind::Unknown('"'),
                start: start as u32,
                len: (self.pos - start) as u16,
            };
        }

        // Single-character tokens
        self.pos += 1;
        let kind = match b {
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrace,
            b'}' => TokenKind::RBrace,
            b'[' => TokenKind::LBracket,
            b']' => TokenKind::RBracket,
            b',' => TokenKind::Comma,
            b'.' => TokenKind::Dot,
            b';' => TokenKind::Semicolon,
            b':' => TokenKind::Colon,

            // Operators that might be compound
            b'+' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::PlusEqual
                } else {
                    TokenKind::Plus
                }
            }
            b'-' => match self.src[self.pos] {
                b'=' => {
                    self.pos += 1;
                    TokenKind::MinusEqual
                }
                b'>' => {
                    self.pos += 1;
                    TokenKind::MinusGreater
                }
                _ => TokenKind::Minus,
            },
            b'*' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::StarEqual
                } else {
                    TokenKind::Star
                }
            }
            b'/' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::SlashEqual
                } else {
                    TokenKind::Slash
                }
            }
            b'%' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::PercentEqual
                } else {
                    TokenKind::Percent
                }
            }
            b'=' => match self.src[self.pos] {
                b'=' => {
                    self.pos += 1;
                    TokenKind::EqualEqual
                }
                b'>' => {
                    self.pos += 1;
                    TokenKind::EqualGreater
                }
                _ => TokenKind::Equal,
            },
            b'!' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                }
            }
            b'<' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::LessEqual
                        }
                        _ => TokenKind::Less,
                    }
                } else {
                    TokenKind::Less
                }
            }
            b'>' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::GreaterEqual
                        }

                        _ => TokenKind::Greater,
                    }
                } else {
                    TokenKind::Greater
                }
            }
            b'&' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::AmpersandEqual
                        }
                        b'&' => {
                            self.pos += 1;
                            TokenKind::AmpersandAmpersand
                        }
                        _ => TokenKind::Ampersand,
                    }
                } else {
                    TokenKind::Ampersand
                }
            }
            b'|' => {
                if self.pos < self.len {
                    match self.src[self.pos] {
                        b'=' => {
                            self.pos += 1;
                            TokenKind::PipeEqual
                        }
                        b'|' => {
                            self.pos += 1;
                            TokenKind::PipePipe
                        }
                        _ => TokenKind::Pipe,
                    }
                } else {
                    TokenKind::Pipe
                }
            }
            b'^' => {
                if self.pos < self.len && self.src[self.pos] == b'=' {
                    self.pos += 1;
                    TokenKind::CaretEqual
                } else {
                    TokenKind::Caret
                }
            }
            other => {
                let ch = other as char;
                TokenKind::Unknown(ch)
            }
        };

        Token {
            kind,
            start: start as u32,
            len: (self.pos - start) as u16,
        }
    }
}
