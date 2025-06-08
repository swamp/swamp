use lexer::{Lexer, Token, TokenKind};
use source_map_node::Node;
use swamp_ast::{
    Expression, ExpressionKind, FieldExpression, FieldName, LiteralKind,
    QualifiedConstantIdentifier,
};

#[derive(Debug)]
pub enum ErrorKind {
    ExpectedIdentifier,
    ExpectedIdentifierOrRest,
    ExpectedCommaOrRBrace,
    ExpectedColon,
    UnknownTerm,
    RestMustBeLast,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub node: swamp_ast::Node,
}

pub struct Parser {
    pub errors: Vec<Error>,
    pub doc_comments: Vec<Token>,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    #[must_use]
    pub fn new() -> Self {
        Self {
            errors: Vec::default(),
            doc_comments: Vec::default(),
        }
    }
}

pub struct Term {
    pub node: Node,
}

impl Parser {
    fn next_token(&mut self, lexer: &mut Lexer) -> Token {
        let token = lexer.next_token();
        match token.kind {
            TokenKind::DocComment => {
                self.doc_comments.push(token);
                self.next_token(lexer)
            }
            TokenKind::BlockComment | TokenKind::LineComment => self.next_token(lexer),
            _ => token,
        }
    }

    pub fn parse_term(&mut self, lexer: &mut Lexer) -> Expression {
        let peek_kind = Self::peek(lexer);

        match peek_kind {
            TokenKind::EOF => todo!(),

            TokenKind::Integer(_value) => {
                let token = self.next_token(lexer);
                self.expr(ExpressionKind::Literal(LiteralKind::Int), &token, &token)
            }
            TokenKind::Fixed(_) => {
                let token = self.next_token(lexer);
                self.expr(ExpressionKind::Literal(LiteralKind::Float), &token, &token)
            }
            TokenKind::StringLiteral => {
                let token = self.next_token(lexer);
                self.expr(
                    ExpressionKind::Literal(LiteralKind::String(lexer.get_string(&token.node))),
                    &token,
                    &token,
                )
            }
            TokenKind::Constant => {
                let token = self.next_token(lexer);
                self.expr(
                    ExpressionKind::ConstantReference(QualifiedConstantIdentifier::new(
                        Self::to_node(&token),
                        None,
                    )),
                    &token,
                    &token,
                )
            }
            TokenKind::LBrace => self.parse_struct_literal(lexer),
            _ => {
                let token = self.next_token(lexer);
                self.err(ErrorKind::UnknownTerm, &token)
            }
        }
    }

    const fn to_node(token: &Token) -> swamp_ast::Node {
        swamp_ast::Node {
            span: swamp_ast::SpanWithoutFileId {
                offset: token.node.start,
                length: token.node.len,
            },
        }
    }

    pub fn parse_struct_literal(&mut self, lexer: &mut Lexer) -> Expression {
        let mut field_expressions = Vec::new();
        let mut found_rest = false;

        let mut tok = self.next_token(lexer); // consume `{`
        let start = tok;
        loop {
            tok = self.next_token(lexer);
            if tok.kind == TokenKind::RBrace {
                break;
            }
            if found_rest {
                self.error(ErrorKind::RestMustBeLast, &tok);
                break;
            }
            if tok.kind == TokenKind::DotDot {
                found_rest = true;
            } else if tok.kind == TokenKind::Identifier {
                tok = self.next_token(lexer);
                if tok.kind != TokenKind::Colon {
                    self.error(ErrorKind::ExpectedColon, &tok);
                    break;
                }
                field_expressions.push(FieldExpression {
                    field_name: FieldName(Self::to_node(&tok)),
                    expression: self.parse_expression(lexer),
                });
            } else {
                self.error(ErrorKind::ExpectedIdentifierOrRest, &tok);
                break;
            }
            let next_kind = Self::peek(lexer);
            if next_kind == TokenKind::RBrace {
            } else if next_kind == TokenKind::Comma {
                self.bump(lexer);
            } else {
                self.error(ErrorKind::ExpectedCommaOrRBrace, &tok);
                break;
            }
        }

        let kind = ExpressionKind::AnonymousStructLiteral(field_expressions, found_rest);

        self.expr(kind, &start, &tok)
    }

    fn error(&mut self, error_kind: ErrorKind, token: &Token) {
        self.errors.push(Error {
            kind: error_kind,
            node: Self::to_node(token),
        });
    }

    pub fn err(&mut self, error_kind: ErrorKind, token: &Token) -> Expression {
        self.errors.push(Error {
            kind: error_kind,
            node: Self::to_node(token),
        });

        self.expr(ExpressionKind::Error, token, token)
    }

    fn parse_expression(&mut self, lexer: &mut Lexer) -> Expression {
        // TODO: For now just parse terms
        self.parse_term(lexer)
    }

    const fn expr(
        &self,
        expression_kind: ExpressionKind,
        start_token: &Token,
        _end_token: &Token, // TODO: use end_token as well to get range
    ) -> Expression {
        Expression {
            kind: expression_kind,
            node: Self::to_node(start_token), // TODO: use end_token as well to get range
        }
    }

    fn peek(lexer: &mut Lexer) -> TokenKind {
        let token = lexer.peek();

        match &token.kind {
            TokenKind::BlockComment | TokenKind::LineComment | TokenKind::DocComment => {
                Self::peek(lexer)
            }
            _ => token.kind.clone(),
        }
    }

    fn bump(&mut self, lexer: &mut Lexer) {
        let _ = self.next_token(lexer);
    }
}
