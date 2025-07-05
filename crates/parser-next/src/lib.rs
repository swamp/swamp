/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use lexer::{Lexer, Token, TokenKind};
use swamp_ast::{
    BinaryOperator, BinaryOperatorKind, Expression, ExpressionKind, FieldExpression, FieldName,
    LiteralKind, QualifiedConstantIdentifier, SpanWithoutFileId, UnaryOperator, Variable,
};
use tracing::info;

#[derive(Debug)]
pub enum ErrorKind {
    ExpectedIdentifier,
    ExpectedIdentifierOrRest,
    ExpectedCommaOrRBrace,
    ExpectedColon,
    UnknownTerm,
    RestMustBeLast,
    ExpectedExpression,
    UnexpectedToken,
    ExpectedRParen,
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

    const fn get_precedence(kind: &TokenKind) -> u8 {
        match kind {
            TokenKind::PipePipe => 1,                          // ||
            TokenKind::AmpersandAmpersand => 2,                // &&
            TokenKind::EqualEqual | TokenKind::BangEqual => 3, // == !=
            TokenKind::Less
            | TokenKind::Greater
            | TokenKind::LessEqual
            | TokenKind::GreaterEqual => 4, // < > <= >=
            TokenKind::Plus | TokenKind::Minus => 5,           // + -
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => 6, // * / %
            // Unary: 7
            _ => 0,
        }
    }

    const fn get_binary_op_kind(kind: &TokenKind) -> Option<BinaryOperatorKind> {
        match kind {
            TokenKind::Plus => Some(BinaryOperatorKind::Add),
            TokenKind::Minus => Some(BinaryOperatorKind::Subtract),
            TokenKind::Star => Some(BinaryOperatorKind::Multiply),
            TokenKind::Slash => Some(BinaryOperatorKind::Divide),
            TokenKind::Percent => Some(BinaryOperatorKind::Modulo),
            TokenKind::PipePipe => Some(BinaryOperatorKind::LogicalOr),
            TokenKind::AmpersandAmpersand => Some(BinaryOperatorKind::LogicalAnd),
            TokenKind::EqualEqual => Some(BinaryOperatorKind::Equal),
            TokenKind::BangEqual => Some(BinaryOperatorKind::NotEqual),
            TokenKind::Less => Some(BinaryOperatorKind::LessThan),
            TokenKind::LessEqual => Some(BinaryOperatorKind::LessEqual),
            TokenKind::Greater => Some(BinaryOperatorKind::GreaterThan),
            TokenKind::GreaterEqual => Some(BinaryOperatorKind::GreaterEqual),
            _ => None,
        }
    }

    const fn get_unary_op(kind: &TokenKind, token: &Token) -> Option<UnaryOperator> {
        match kind {
            TokenKind::Bang => Some(UnaryOperator::Not(Self::to_node(token))),
            TokenKind::Minus => Some(UnaryOperator::Negate(Self::to_node(token))),
            _ => None,
        }
    }

    pub fn parse_expression(&mut self, lexer: &mut Lexer) -> Expression {
        self.parse_expression_bp(lexer, 0)
    }

    fn parse_expression_bp(&mut self, lexer: &mut Lexer, min_bp: u8) -> Expression {
        info!(?min_bp, "parse_expression_bp");

        let mut lhs = self.parse_prefix(lexer);
        info!(?lhs, "initial lhs");

        loop {
            let peek_token = lexer.peek();
            let peek_kind = peek_token.kind.clone();
            info!(?peek_kind, "peeked token");

            if peek_kind == TokenKind::EOF {
                break;
            }

            // Help out if called from a LParen
            if peek_kind == TokenKind::RParen {
                break;
            }

            let bp = Self::get_precedence(&peek_kind);
            if bp < min_bp {
                info!(?bp, ?min_bp, "bp is lower than min. breaking out");
                break;
            }

            let op_token = self.next_token(lexer);
            info!(?op_token, "operator token");

            if let Some(op_kind) = Self::get_binary_op_kind(&op_token.kind) {
                info!(?op_kind, "found binary operator");
                let rhs = self.parse_expression_bp(lexer, bp + 1);
                info!(?rhs, "parsed rhs");

                let op = BinaryOperator {
                    kind: op_kind,
                    node: Self::to_node(&op_token),
                };
                lhs = Self::expr(
                    ExpressionKind::BinaryOp(Box::new(lhs), op, Box::new(rhs)),
                    &op_token,
                    &op_token,
                );
                info!(?lhs, "updated lhs");
            } else {
                self.errors.push(Error {
                    kind: ErrorKind::UnexpectedToken,
                    node: Self::to_node(&op_token),
                });
                break;
            }
        }

        info!(?lhs, "final result");

        lhs
    }

    fn parse_prefix(&mut self, lexer: &mut Lexer) -> Expression {
        info!("parse_prefix");
        let peek_kind = Self::peek(lexer);

        if let Some(unary_op) = Self::get_unary_op(&peek_kind, lexer.peek()) {
            let op_token = self.next_token(lexer);
            let expr = self.parse_expression_bp(lexer, 7);
            return Self::expr(
                ExpressionKind::UnaryOp(unary_op, Box::new(expr)),
                &op_token,
                &op_token,
            );
        }

        self.parse_term(lexer)
    }

    fn next_token(&mut self, lexer: &mut Lexer) -> Token {
        let token = lexer.next_token();
        match token.kind {
            TokenKind::DocComment => {
                self.doc_comments.push(token);
                self.next_token(lexer)
            }
            TokenKind::BlockComment | TokenKind::LineComment => self.next_token(lexer),
            _ => {
                info!(?token, "next token");
                token
            }
        }
    }

    pub fn parse_term(&mut self, lexer: &mut Lexer) -> Expression {
        let peek_kind = Self::peek(lexer);

        match peek_kind {
            TokenKind::EOF => {
                let token = self.next_token(lexer);
                self.err(ErrorKind::ExpectedExpression, &token)
            }

            TokenKind::Integer(_value) => {
                let token = self.next_token(lexer);
                Self::expr(ExpressionKind::Literal(LiteralKind::Int), &token, &token)
            }
            TokenKind::Fixed(_) => {
                let token = self.next_token(lexer);
                Self::expr(ExpressionKind::Literal(LiteralKind::Float), &token, &token)
            }
            TokenKind::StringLiteral => {
                let token = self.next_token(lexer);
                Self::expr(
                    ExpressionKind::Literal(LiteralKind::String(lexer.get_string(&token.node))),
                    &token,
                    &token,
                )
            }
            TokenKind::Constant => {
                let token = self.next_token(lexer);
                Self::expr(
                    ExpressionKind::ConstantReference(QualifiedConstantIdentifier::new(
                        Self::to_node(&token),
                        None,
                    )),
                    &token,
                    &token,
                )
            }
            TokenKind::LBrace => self.parse_struct_literal(lexer),
            TokenKind::LParen => {
                self.bump(lexer);
                let expr = self.parse_expression(lexer);
                let rparen = self.next_token(lexer);

                if rparen.kind != TokenKind::RParen {
                    self.errors.push(Error {
                        kind: ErrorKind::ExpectedRParen,
                        node: Self::to_node(&rparen),
                    });
                }

                expr
            }
            TokenKind::Identifier => {
                let token = self.next_token(lexer);
                Self::expr(
                    ExpressionKind::VariableReference(Variable {
                        name: Self::to_node(&token),
                        is_mutable: None,
                    }),
                    &token,
                    &token,
                )
            }
            _ => {
                let token = self.next_token(lexer);
                self.err(ErrorKind::UnknownTerm, &token)
            }
        }
    }

    const fn to_node(token: &Token) -> swamp_ast::Node {
        swamp_ast::Node {
            span: SpanWithoutFileId {
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
                info!("before reading colon");
                let colon_tok = self.next_token(lexer);
                info!("after reading colon");
                if colon_tok.kind != TokenKind::Colon {
                    info!("could not find colon");
                    self.error(ErrorKind::ExpectedColon, &colon_tok);
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
            info!("peek for `}}` or `,`");
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

        Self::expr(kind, &start, &tok)
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

        Self::expr(ExpressionKind::Literal(LiteralKind::Int), token, token)
    }

    const fn expr(
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
        Self::peek_n(lexer, 0)
    }

    fn peek_n(lexer: &mut Lexer, i: usize) -> TokenKind {
        let mut current_i = i;
        loop {
            let token = lexer.peek_n(current_i);
            match &token.kind {
                TokenKind::BlockComment | TokenKind::LineComment | TokenKind::DocComment => {
                    current_i += 1;
                    if current_i > 3 {
                        // TODO: This is a hack
                        return TokenKind::EOF;
                    }
                }
                _ => {
                    info!("peeking {current_i}: {token:?}");
                    return token.kind.clone();
                }
            }
        }
    }

    fn bump(&mut self, lexer: &mut Lexer) {
        let _ = self.next_token(lexer);
    }
}
