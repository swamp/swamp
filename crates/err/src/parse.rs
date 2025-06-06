/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{Builder, Report, build_and_print};
use eira::Kind;
use source_map_cache::SourceMap;
use source_map_node::Span;
use std::path::Path;
use swamp_dep_loader::ParserError;
use swamp_parser::SpecificError;

#[must_use]
pub fn build_parse_error(err: &SpecificError, span: &Span) -> Builder<usize> {
    let mut b = match err {
        SpecificError::General(general) => Report::build(
            Kind::Error,
            1,
            &format!("General Parse Error: {general}"),
            span,
        ),

        SpecificError::MissingTypeName => Report::build(Kind::Error, 1, "missing type name", span),
        SpecificError::UnknownEscapeCharacter(char) => Report::build(
            Kind::Error,
            1,
            &format!("Unknown escape character '{char}'"),
            span,
        ),
        SpecificError::UnfinishedEscapeSequence => {
            Report::build(Kind::Error, 1, "Unfinished escape sequence", span)
        }
        SpecificError::InvalidUnicodeEscape => {
            Report::build(Kind::Error, 1, "invalid unicode escape sequence", span)
        }
        SpecificError::InvalidHexEscape => {
            Report::build(Kind::Error, 1, "invalid hex escape sequence", span)
        }
        SpecificError::InvalidUtf8Sequence => {
            Report::build(Kind::Error, 1, "invalid utf-8 escape sequence", span)
        }
        SpecificError::ExpectingTypeIdentifier => todo!(),
        SpecificError::ExpectingInnerPair => todo!(),
        SpecificError::UnexpectedTypeRule(wrong_rule) => {
            Report::build(Kind::Error, 1, "unexpected type rule", span)
                .with_note(&format!("rule: {wrong_rule}"))
        }
        SpecificError::ExpectedTypeIdentifier(_) => todo!(),
        SpecificError::ExpectedLocalTypeIdentifier(_) => todo!(),
        SpecificError::UnexpectedRuleInParseScript(rule) => Report::build(
            Kind::Error,
            1,
            &format!("unexpected type rule {rule}"),
            span,
        ),
        SpecificError::ExpectedControlStatement(_) => todo!(),
        SpecificError::ExpectedStatement(_) => todo!(),
        SpecificError::ExpectedIfOrElse(_) => todo!(),
        SpecificError::MissingFunctionSignature => todo!(),
        SpecificError::MissingFunctionBody => todo!(),
        SpecificError::ExpectedStatementBlock => todo!(),
        SpecificError::ExpectedFunctionDefinition => todo!(),
        SpecificError::ExpectedParameter => todo!(),
        SpecificError::ExpectedImplItem => todo!(),
        SpecificError::ExpectedMemberSignature => todo!(),
        SpecificError::ExpectedBlockInWhileLoop => todo!(),
        SpecificError::UnexpectedExpressionType(_) => todo!(),
        SpecificError::UnexpectedAccessType(_) => todo!(),
        SpecificError::UnknownAssignmentOperator(_) => todo!(),
        SpecificError::CompoundOperatorCanNotContainMut => todo!(),
        SpecificError::InvalidAssignmentTarget => {
            Report::build(Kind::Error, 1, "invalid assignment target", span)
        }
        SpecificError::CompoundOperatorCanNotHaveMultipleVariables => todo!(),
        SpecificError::ExpectedExpressionAfterPrefixOperator => todo!(),
        SpecificError::UnknownOperator(_) => {
            Report::build(Kind::Error, 1, "unknown operator", span)
        }
        SpecificError::UnexpectedPostfixOperator => todo!(),
        SpecificError::UnexpectedUnaryOperator(_) => todo!(),
        SpecificError::InvalidMemberCall => todo!(),
        SpecificError::UnknownMatchType => todo!(),
        SpecificError::UnexpectedElementInPatternList => todo!(),
        SpecificError::InvalidPrecisionValue => todo!(),
        SpecificError::InvalidPrecisionType => todo!(),
        SpecificError::ExpectedTypeIdentifierAfterPath => todo!(),
        SpecificError::UnexpectedPatternListElement(_) => todo!(),
        SpecificError::MustHaveAtLeastOneArm => todo!(),
        SpecificError::UnexpectedMatchArmRule(_) => todo!(),
        SpecificError::UnknownEnumVariant(_) => todo!(),
        SpecificError::UnknownLiteral => todo!(),
        SpecificError::UnknownPrimary(_) => todo!(),
        SpecificError::InvalidFormatSpecifier => todo!(),
        SpecificError::UnexpectedVariantField => {
            Report::build(Kind::Error, 321, "unexpected variant field", span)
        }
        SpecificError::MutOnlyForVariables => todo!(),
        SpecificError::UnexpectedTokenInFunctionCall => todo!(),
        SpecificError::ExpectedExpressionInInterpolation => todo!(),
        SpecificError::UnexpectedRuleInInterpolation => todo!(),
        SpecificError::ExpectedForPattern => todo!(),
        SpecificError::ExpectedBlock => Report::build(Kind::Error, 321, "ExpectedBlock", span),
        SpecificError::InvalidForPattern => todo!(),
        SpecificError::UnexpectedRuleInElse(_) => todo!(),
        SpecificError::ExpectedLocationExpression => todo!(),
        SpecificError::ExpectedImportPath => todo!(),
        SpecificError::ExpectedIdentifier => todo!(),
        SpecificError::ExpectedIdentifierAfterPath => todo!(),
        SpecificError::ExpectedFieldOrRest => todo!(),
        SpecificError::UnknownTerm(term) => Report::build(Kind::Error, 32241, "UnknownTerm", span)
            .with_note(&format!("term {term}")),
        SpecificError::UnknownExpr(_) => todo!(),
        SpecificError::CouldNotMoveDown
        | SpecificError::CouldNotMoveRight
        | SpecificError::UnexpectedTokenInMutableExpression => todo!(),
    };

    b.error_module = "P".to_string();
    b
}

pub fn show_parse_error(
    err: &SpecificError,
    span: &Span,
    source_map: &SourceMap,
    current_dir: &Path,
) {
    let builder = build_parse_error(err, span);
    build_and_print(builder, source_map, current_dir);
}

#[must_use]
pub fn build_parser_error(err: &ParserError) -> Builder<usize> {
    build_parse_error(
        &err.specific,
        &Span {
            file_id: err.file_id,
            offset: err.node.span.offset,
            length: err.node.span.length,
        },
    )
}

pub fn show_parser_error(err: &ParserError, source_map: &SourceMap, current_dir: &Path) {
    let builder = build_parser_error(err);
    build_and_print(builder, source_map, current_dir);
}
