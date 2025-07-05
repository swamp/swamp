/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use lexer::Lexer;
use swamp_ast::{BinaryOperatorKind, Expression, ExpressionKind, LiteralKind};
use swamp_parser_next::Parser;
use tracing::info;

#[must_use]
pub fn lex(str: &str) -> (Lexer, Parser) {
    let lex = Lexer::new(str);
    let parser = Parser::new();
    (lex, parser)
}

fn parse_expression(input: &str) -> Expression {
    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new();
    parser.parse_expression(&mut lexer)
}

// #[test_log::test] // TODO: Test not working yet
fn anon_struct() {
    let (mut lexer, mut parser) = lex(r#"
        {

           field_name: 23, another: "hello",
              another: CONSTANT_SOMETHING_3,..
           }
    "#);

    let expression = parser.parse_struct_literal(&mut lexer);
    let ExpressionKind::AnonymousStructLiteral(fields, rest) = expression.kind else {
        panic!("must be anonymous")
    };

    eprintln!("expr: {fields:?} rest:{rest} errors: {:?}", parser.errors);

    assert!(parser.errors.is_empty());
    assert!(rest);
    assert_eq!(fields.len(), 3);
}

#[test_log::test]
fn incomplete_expression() {
    let (mut lexer, mut parser) = lex("2 + ");
    let expression = parser.parse_expression(&mut lexer);
    info!(?expression, "incomplete expr");
    info!(errors=?parser.errors, "errors");

    assert!(!parser.errors.is_empty());
}

#[test_log::test]
fn basic_term_parsing() {
    let expr = parse_expression("42");
    assert!(matches!(
        expr.kind,
        ExpressionKind::Literal(LiteralKind::Int)
    ));

    let expr = parse_expression("variable");
    assert!(matches!(expr.kind, ExpressionKind::VariableReference(_)));

    let expr = parse_expression("(42)");
    assert!(matches!(
        expr.kind,
        ExpressionKind::Literal(LiteralKind::Int)
    ));
}

#[test_log::test]
fn test_operator_precedence() {
    // Should parse as 2 + (3 * 4)
    let expr = parse_expression("2 + 3 * 4");
    if let ExpressionKind::BinaryOp(lhs, op, rhs) = expr.kind {
        assert!(matches!(op.kind, BinaryOperatorKind::Add));
        assert!(matches!(
            lhs.kind,
            ExpressionKind::Literal(LiteralKind::Int)
        ));
        if let ExpressionKind::BinaryOp(lhs2, op2, rhs2) = rhs.kind {
            assert!(matches!(op2.kind, BinaryOperatorKind::Multiply));
            assert!(matches!(
                lhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
            assert!(matches!(
                rhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
        } else {
            panic!("Expected BinaryOp for rhs");
        }
    } else {
        panic!("Expected BinaryOp");
    }
}

#[test_log::test]
fn unary_operators() {
    let expr = parse_expression("!true");
    assert!(matches!(expr.kind, ExpressionKind::UnaryOp(_, _)));
}

#[test_log::test]
fn complex_expression() {
    // Should parse as: ((2 + 3) * 4) - (10 / 2)
    let expr = parse_expression("(2 + 3) * 4 - (10 / 2)");

    if let ExpressionKind::BinaryOp(lhs, op, rhs) = expr.kind {
        assert!(matches!(op.kind, BinaryOperatorKind::Subtract));

        // Check left side: (2 + 3) * 4
        if let ExpressionKind::BinaryOp(lhs2, op2, rhs2) = lhs.kind {
            assert!(matches!(op2.kind, BinaryOperatorKind::Multiply));
            if let ExpressionKind::BinaryOp(lhs3, op3, rhs3) = lhs2.kind {
                assert!(matches!(op3.kind, BinaryOperatorKind::Add));
                assert!(matches!(
                    lhs3.kind,
                    ExpressionKind::Literal(LiteralKind::Int)
                ));
                assert!(matches!(
                    rhs3.kind,
                    ExpressionKind::Literal(LiteralKind::Int)
                ));
            } else {
                panic!("Expected BinaryOp for inner expression");
            }
            assert!(matches!(
                rhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
        } else {
            panic!("Expected BinaryOp for left side");
        }

        // Verify right side: 10 / 2
        if let ExpressionKind::BinaryOp(lhs2, op2, rhs2) = rhs.kind {
            assert!(matches!(op2.kind, BinaryOperatorKind::Divide));
            assert!(matches!(
                lhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
            assert!(matches!(
                rhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
        } else {
            panic!("Expected BinaryOp for right side");
        }
    } else {
        panic!("Expected BinaryOp");
    }
}

#[test_log::test]
fn operator_associativity() {
    // Should be parsed as (2 - 3) - 4
    let expr = parse_expression("2 - 3 - 4");

    if let ExpressionKind::BinaryOp(lhs, op, rhs) = expr.kind {
        assert!(matches!(op.kind, BinaryOperatorKind::Subtract));
        assert!(matches!(
            rhs.kind,
            ExpressionKind::Literal(LiteralKind::Int)
        ));

        // The left side should be another subtraction
        if let ExpressionKind::BinaryOp(lhs2, op2, rhs2) = lhs.kind {
            assert!(matches!(op2.kind, BinaryOperatorKind::Subtract));
            assert!(matches!(
                lhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
            assert!(matches!(
                rhs2.kind,
                ExpressionKind::Literal(LiteralKind::Int)
            ));
        } else {
            panic!("Expected BinaryOp for left side");
        }
    } else {
        panic!("Expected BinaryOp");
    }
}
