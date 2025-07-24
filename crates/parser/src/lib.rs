/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use pest::error::{Error, ErrorVariant, InputLocation};
use pest::iterators::{Pair, Pairs};
use pest::{Parser, Position};
use pest_derive::Parser;
use std::iter::Peekable;
use std::str::Chars;
use swamp_ast::{
    AssignmentOperatorKind, BinaryOperatorKind, CompoundOperator, CompoundOperatorKind,
    ConcretePattern, DestructuringPattern, EnumVariantLiteral, EnumVariantLiteralKind,
    ExpressionKind, FieldExpression, FieldName, ForPattern, ForVar, ImportItems,
    IterableExpression, LocalConstantIdentifier, LocalTypeIdentifierWithOptionalTypeVariables, Mod,
    NamedStructDef, PatternVariableOrWildcard, QualifiedIdentifier, RangeMode, SpanWithoutFileId,
    StructTypeField, TypeForParameter, TypeVariable, VariableBinding, prelude::*,
};
use swamp_ast::{AttributeLiteralKind, Function};
use swamp_ast::{GenericParameter, LiteralKind};
use swamp_ast::{Postfix, PostfixChain};
use tracing::error;

pub struct ParseResult<'a> {
    #[allow(dead_code)]
    script: String, // Pairs are referencing the script
    pairs: pest::iterators::Pairs<'a, Rule>,
}

pub struct GeneralError {
    pub description: String,
}

#[derive(Debug)]
pub enum SpecificError {
    CouldNotMoveDown,
    CouldNotMoveRight,
    General(String),
    ExpectingTypeIdentifier,
    ExpectingInnerPair,
    UnexpectedTypeRule(String),
    ExpectedTypeIdentifier(String),
    ExpectedLocalTypeIdentifier(String),
    UnexpectedRuleInParseScript(String),
    ExpectedControlStatement(String),
    ExpectedStatement(String),
    ExpectedIfOrElse(String),
    MissingFunctionSignature,
    MissingFunctionBody,
    ExpectedStatementBlock,
    ExpectedFunctionDefinition,
    ExpectedParameter,
    ExpectedImplItem,
    ExpectedMemberSignature,
    ExpectedBlockInWhileLoop,
    UnexpectedExpressionType(String),
    UnexpectedAccessType(String),
    UnknownAssignmentOperator(String),
    CompoundOperatorCanNotContainMut,
    InvalidAssignmentTarget,
    CompoundOperatorCanNotHaveMultipleVariables,
    ExpectedExpressionAfterPrefixOperator,
    UnknownOperator(String),
    UnexpectedPostfixOperator,
    UnexpectedUnaryOperator(String),
    InvalidMemberCall,
    UnknownMatchType,
    UnexpectedElementInPatternList,
    InvalidPrecisionValue,
    InvalidPrecisionType,
    ExpectedTypeIdentifierAfterPath,
    UnexpectedPatternListElement(String),
    MustHaveAtLeastOneArm,
    UnexpectedMatchArmRule(String),
    UnknownEnumVariant(String),
    UnknownLiteral,
    UnknownPrimary(String),
    InvalidFormatSpecifier,
    UnexpectedVariantField,
    MutOnlyForVariables,
    UnexpectedTokenInFunctionCall,
    ExpectedExpressionInInterpolation,
    UnexpectedRuleInInterpolation,
    ExpectedForPattern,
    ExpectedBlock,
    InvalidForPattern,
    UnexpectedRuleInElse(String),
    ExpectedLocationExpression,
    ExpectedImportPath,
    ExpectedIdentifier,
    ExpectedIdentifierAfterPath,
    ExpectedFieldOrRest,
    UnknownEscapeCharacter(char),
    UnfinishedEscapeSequence,
    InvalidUnicodeEscape,
    InvalidHexEscape,
    InvalidUtf8Sequence,
    MissingTypeName,
    UnknownTerm(String),
    UnknownExpr(String),
    UnexpectedTokenInMutableExpression,
}

#[derive(Debug)]
pub struct ParseError {
    pub span: SpanWithoutFileId,
    pub specific: SpecificError,
}

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ScriptParser;

pub const UNKNOWN_FILE_ID: u16 = 0xffff;

pub struct AstParser;

impl From<Error<Rule>> for ParseError {
    fn from(value: Error<Rule>) -> Self {
        let span = match value.location {
            InputLocation::Pos(pos) => SpanWithoutFileId {
                offset: pos as u32,
                length: 1,
            },
            InputLocation::Span((start, end)) => SpanWithoutFileId {
                offset: start as u32,
                length: (end - start) as u16,
            },
        };
        Self {
            span,
            specific: SpecificError::General(value.variant.to_string()),
        }
    }
}

impl AstParser {
    fn next_pair<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<Pair<'a, Rule>, ParseError> {
        Ok(pairs.next().ok_or_else(|| {
            Error::new_from_pos(
                ErrorVariant::CustomError {
                    message: "Expected more tokens".into(),
                },
                Position::from_start(""),
            )
        })?)
    }

    fn expect_next<'a>(
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
        expected_rule: Rule,
    ) -> Result<Pair<'a, Rule>, ParseError> {
        let pair = Self::next_pair(pairs)?;
        if pair.as_rule() != expected_rule {
            return Err(Error::new_from_span(
                ErrorVariant::CustomError {
                    message: format!("Expected {:?}, found {:?}", expected_rule, pair.as_rule()),
                },
                pair.as_span(),
            ))?;
        }
        Ok(pair)
    }

    fn expect_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::identifier)?;
        Ok(LocalIdentifier::new(self.to_node(&pair)))
    }

    fn expect_function_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::function_identifier)?;
        Ok(LocalIdentifier::new(self.to_node(&pair)))
    }

    fn expect_constant_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalConstantIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::constant_identifier)?;
        Ok(LocalConstantIdentifier(self.to_node(&pair)))
    }

    fn _expect_variable_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<Variable, ParseError> {
        let identifier = self.expect_identifier_next(pairs)?;
        Ok(Variable {
            name: identifier.0,
            is_mutable: None,
        })
    }

    fn expect_field_label_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<FieldName, ParseError> {
        let field_label_pair = Self::expect_next(pairs, Rule::field_label)?;
        let mut inner = field_label_pair.clone().into_inner();
        let ident_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(SpecificError::ExpectedIdentifier, &field_label_pair)
        })?;

        Ok(FieldName(self.to_node(&ident_pair)))
    }

    fn parse_dot_identifier<'a>(&self, pair: &Pair<Rule>) -> Result<FieldName, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::dot_identifier);
        let mut inner = pair.clone().into_inner();
        let ident_pair = inner
            .next()
            .ok_or_else(|| self.create_error_pair(SpecificError::ExpectedIdentifier, pair))?;

        Ok(FieldName(self.to_node(&ident_pair)))
    }

    fn expect_local_type_identifier_next<'a>(
        &self,
        pairs: &mut impl Iterator<Item = Pair<'a, Rule>>,
    ) -> Result<LocalTypeIdentifier, ParseError> {
        let pair = Self::expect_next(pairs, Rule::type_identifier)?;
        Ok(LocalTypeIdentifier::new(self.to_node(&pair)))
    }

    fn convert_into_iterator<'a>(pair: &'a Pair<'a, Rule>) -> impl Iterator<Item = Pair<'a, Rule>> {
        pair.clone().into_inner()
    }

    fn create_error_pair(&self, kind: SpecificError, pair: &Pair<Rule>) -> ParseError {
        ParseError {
            span: self.to_span(pair.as_span()),
            specific: kind,
        }
    }

    fn to_err(kind: SpecificError, pair: &Pair<Rule>) -> ParseError {
        ParseError {
            span: Self::span(pair.as_span()),
            specific: kind,
        }
    }

    fn next_inner_pair<'a>(&self, pair: &Pair<'a, Rule>) -> Result<Pair<'a, Rule>, ParseError> {
        let _span = pair.as_span();
        pair.clone()
            .into_inner()
            .next()
            .ok_or_else(move || self.create_error_pair(SpecificError::ExpectingInnerPair, pair))
    }

    pub fn parse(rule: Rule, raw_script: &str) -> Result<ParseResult<'static>, ParseError> {
        let pairs = unsafe {
            std::mem::transmute::<pest::iterators::Pairs<'_, Rule>, pest::iterators::Pairs<'_, Rule>>(
                ScriptParser::parse(rule, raw_script)?,
            )
        };
        Ok(ParseResult {
            script: raw_script.to_string(),
            pairs,
        })
    }

    pub fn parse_item(&self, pair: &Pair<Rule>) -> Result<Definition, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::item);

        let mut inner = pair.clone().into_inner();
        let mut attributes = Vec::new();

        while let Some(attr_pair) = inner.peek() {
            if attr_pair.as_rule() == Rule::attribute {
                let attr = self.parse_attribute(&inner.next().unwrap())?;
                attributes.push(attr);
            } else {
                break;
            }
        }
        // The next should be the definition
        if let Some(def_pair) = inner.next() {
            let definition_kind = self.parse_definition(&def_pair, &attributes)?;
            let definition = Definition {
                node: self.to_node(&def_pair),
                kind: definition_kind,
                attributes,
            };
            Ok(definition)
        } else {
            panic!("must be definition after attributes")
        }
    }

    pub fn parse_module(&self, raw_script: &str) -> Result<Module, ParseError> {
        let result = Self::parse(Rule::program, raw_script)?;

        let mut pairs = result.pairs;

        let program_pair = Self::next_pair(&mut pairs)?;

        let mut expressions = Vec::new();
        let mut definitions = Vec::new();
        for pair in Self::convert_into_iterator(&program_pair) {
            match pair.as_rule() {
                Rule::item => {
                    let def = self.parse_item(&pair)?;
                    definitions.push(def);
                }
                Rule::expression => {
                    let expr = self.parse_expression(&pair)?;
                    expressions.push(expr);
                }
                Rule::EOI => {} // End of Input - do nothing
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript(Self::pair_to_rule(&pair)),
                        &pair,
                    ));
                }
            }
        }

        let maybe_expression = match expressions.len() {
            0 => None,
            1 => Some(expressions.into_iter().next().unwrap()),
            _ => Some(Expression {
                kind: ExpressionKind::Block(expressions),
                node: Node {
                    span: SpanWithoutFileId::default(),
                },
            }),
        };

        Ok(Module::new(definitions, maybe_expression))
    }

    fn parse_definition(
        &self,
        pair: &Pair<Rule>,
        attributes: &[Attribute],
    ) -> Result<DefinitionKind, ParseError> {
        let inner_pair = self.next_inner_pair(pair)?;
        match inner_pair.as_rule() {
            Rule::impl_def => self.parse_impl_def(&inner_pair),
            Rule::const_def => self.parse_const_definition(&inner_pair),
            Rule::struct_def => self.parse_struct_def(&inner_pair),
            Rule::type_def => self.parse_type_def(&inner_pair),
            Rule::function_def => self.parse_function_def(&inner_pair, attributes),
            Rule::use_def => self.parse_use(&inner_pair),
            Rule::mod_def => self.parse_mod(&inner_pair),
            Rule::enum_def => self.parse_enum_def(&inner_pair),
            _ => todo!(),
        }
    }

    fn parse_const_definition(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        Ok(DefinitionKind::Constant(self.parse_const_info(pair)?))
    }

    fn parse_const_info(&self, pair: &Pair<Rule>) -> Result<ConstantInfo, ParseError> {
        let mut inner = pair.clone().into_inner(); // Self::convert_into_iterator(pair);

        let constant_identifier = self.expect_constant_identifier_next(&mut inner)?;

        let maybe_annotation = self.parse_maybe_annotation(&mut inner)?;

        let expr_pair = Self::next_pair(&mut inner)?;
        let expression = self.parse_expression(&expr_pair)?;

        Ok(ConstantInfo {
            constant_identifier,
            annotation: maybe_annotation,
            expression: Box::new(expression),
        })
    }

    fn module_path_and_items(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(Vec<Node>, ImportItems), ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let import_path = Self::next_pair(&mut inner)?;

        let mut segments = Vec::new();
        for pair in import_path.into_inner() {
            segments.push(self.to_node(&pair));
        }

        let items = match inner.next() {
            Some(found_rule) => match found_rule.as_rule() {
                Rule::all_imports => ImportItems::All,
                Rule::import_list => {
                    let mut imported_items = Vec::new();
                    for list_item in found_rule.into_inner() {
                        let item = Self::next_pair(&mut list_item.into_inner())?;

                        let import_item = match item.as_rule() {
                            Rule::identifier => {
                                ImportItem::Identifier(LocalIdentifier::new(self.to_node(&item)))
                            }
                            Rule::type_identifier => {
                                ImportItem::Type(LocalTypeIdentifier::new(self.to_node(&item)))
                            }
                            _ => {
                                return Err(self
                                    .create_error_pair(SpecificError::ExpectedIdentifier, &item));
                            }
                        };

                        imported_items.push(import_item);
                    }
                    if imported_items.is_empty() {
                        ImportItems::Nothing
                    } else {
                        ImportItems::Items(imported_items)
                    }
                }
                _ => panic!("was not all_imports or import_list"),
            },
            None => ImportItems::Nothing,
        };

        Ok((segments, items))
    }

    fn parse_use(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let (segments, items) = self.module_path_and_items(pair)?;

        Ok(DefinitionKind::Use(Use {
            module_path: ModulePath(segments),
            items,
        }))
    }

    fn parse_mod(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let (segments, items) = self.module_path_and_items(pair)?;

        Ok(DefinitionKind::Mod(Mod {
            module_path: ModulePath(segments),
            items,
        }))
    }

    fn pair_to_rule(rule: &Pair<Rule>) -> String {
        format!("{:?}", rule.as_rule())
    }

    fn parse_block(&self, block_pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        if block_pair.as_rule() != Rule::block {
            return Err(self.create_error_pair(SpecificError::ExpectedBlock, block_pair));
        }

        let mut expressions = Vec::new();

        for pair in Self::convert_into_iterator(block_pair) {
            if pair.as_rule() != Rule::expression {
                return Err(self.create_error_pair(
                    SpecificError::UnexpectedRuleInParseScript(format!(
                        "Expected expression_in_block, got: {:?}",
                        pair.as_rule()
                    )),
                    block_pair,
                ));
            }

            match pair.as_rule() {
                Rule::expression => {
                    let expr = self.parse_expression(&pair)?;
                    expressions.push(expr);
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInParseScript(format!(
                            "Unexpected rule in parse_block: {:?}",
                            pair.as_rule()
                        )),
                        &pair,
                    ));
                }
            }
        }

        let block_expr = self.create_expr(ExpressionKind::Block(expressions), block_pair);
        Ok(block_expr)
    }

    fn parse_with_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let binding_list_pair = inner.next().expect("variable list missing");
        let binding_list = self.parse_variable_binding_list(&binding_list_pair)?;

        let expr_pair = inner.next().expect("block missing");
        let expr = self.parse_expression(&expr_pair)?;

        let with_expr = self.create_expr(ExpressionKind::With(binding_list, Box::from(expr)), pair);
        Ok(with_expr)
    }

    fn parse_when_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let binding_list =
            self.parse_variable_binding_list(&inner.next().expect("variable list missing"))?;
        let expr = self.parse_expression(&inner.next().expect("block missing"))?;

        let next = inner.next();
        let else_expr = if let Some(found_else) = next {
            Some(Box::new(self.parse_expression(&found_else)?))
        } else {
            None
        };

        Ok(self.create_expr(
            ExpressionKind::When(binding_list, Box::from(expr), else_expr),
            pair,
        ))
    }

    fn parse_when_variable_binding(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<VariableBinding, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let variable = self.parse_variable_item(&inner.next().expect("variable missing"))?;

        let expression = match inner.next() {
            Some(expr_pair) => Some(self.parse_arg_expression(&expr_pair)?),
            _ => None,
        };

        Ok(VariableBinding {
            variable,
            expression,
        })
    }

    fn parse_variable_binding_list(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<VariableBinding>, ParseError> {
        let inner = Self::convert_into_iterator(pair);
        let mut bindings = Vec::new();

        // Each item in inner will be a variable_binding
        for binding_pair in inner {
            if binding_pair.as_rule() == Rule::variable_binding {
                bindings.push(self.parse_when_variable_binding(&binding_pair)?);
            }
        }

        Ok(bindings)
    }
    fn parse_if_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let then_branch = self.parse_expression(&Self::next_pair(&mut inner)?)?;
        let else_branch = inner
            .next()
            .map(|p| {
                match p.as_rule() {
                    Rule::if_expr => self.parse_if_expression(&p), // Recursively handle `else if`
                    _ => self.parse_expression(&p),                // Inline or block `else`
                }
            })
            .transpose()?;

        Ok(self.create_expr(
            ExpressionKind::If(
                Box::new(condition),
                Box::new(then_branch),
                else_branch.map(Box::new),
            ),
            pair,
        ))
    }

    #[allow(clippy::too_many_lines)]
    fn parse_postfix_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        assert_eq!(pair.as_rule(), Rule::postfix);
        let mut inner = pair.clone().into_inner();

        let primary_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(SpecificError::UnexpectedPostfixOperator, pair)
        })?;
        let start_expr = self.parse_term(&primary_pair)?;
        //info!(?start_expr, "start");
        let mut postfixes = Vec::new();
        if inner.len() == 0 {
            return Ok(start_expr);
        }

        for op_pair in inner.clone() {
            //info!(rule=?op_pair.as_rule(), "..continuing chain");
            match op_pair.as_rule() {
                Rule::postfix_op => {
                    let mut sub_inner = op_pair.clone().into_inner();
                    let child = sub_inner.next().ok_or_else(|| {
                        self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &op_pair)
                    })?;

                    match child.as_rule() {
                        Rule::unwrap_postfix => {
                            postfixes
                                .push(Postfix::OptionalChainingOperator(self.to_node(&op_pair)));
                        }

                        Rule::function_call_postfix => {
                            let (maybe_generics, args) =
                                self.parse_function_call_postfix(&child)?;
                            let node = self.to_node(&op_pair);
                            postfixes.push(Postfix::FunctionCall(node, maybe_generics, args));
                        }

                        Rule::member_call_postfix => {
                            let (member_identifier, maybe_generics, args) =
                                self.parse_member_call_postfix(&child)?;

                            postfixes.push(Postfix::MemberCall(
                                member_identifier.0,
                                maybe_generics,
                                args,
                            ));
                        }

                        Rule::member_access_postfix => {
                            let mut inner = child.into_inner();
                            let dot_id = Self::next_pair(&mut inner)?;
                            let identifier = self.parse_dot_identifier(&dot_id)?;
                            postfixes.push(Postfix::FieldAccess(identifier.0));
                        }

                        Rule::subscript_postfix => {
                            let mut arr_inner = child.clone().into_inner();

                            let first_expr_pair = arr_inner.next().ok_or_else(|| {
                                self.create_error_pair(
                                    SpecificError::UnexpectedPostfixOperator,
                                    &child,
                                )
                            })?;
                            let first_expression = self.parse_expression(&first_expr_pair)?;

                            let second_expr_pair = arr_inner.next();
                            match second_expr_pair {
                                Some(pair) => {
                                    let second_expression = self.parse_expression(&pair)?;
                                    postfixes.push(Postfix::SubscriptTuple(
                                        first_expression,
                                        second_expression,
                                    ));
                                }
                                None => {
                                    postfixes.push(Postfix::Subscript(first_expression));
                                }
                            }
                        }

                        _ => {
                            return Err(self.create_error_pair(
                                SpecificError::UnexpectedPostfixOperator,
                                &child,
                            ));
                        }
                    }
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::UnexpectedPostfixOperator, &op_pair)
                    );
                }
            }
        }

        Ok(self.create_expr(
            ExpressionKind::PostfixChain(PostfixChain {
                base: Box::from(start_expr),
                postfixes,
            }),
            pair,
        ))
    }

    fn parse_member_call_postfix(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(FieldName, Option<Vec<GenericParameter>>, Vec<Expression>), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::member_call_postfix);

        let mut inner = pair.clone().into_inner();

        let member_access = Self::next_pair(&mut inner)?;
        debug_assert_eq!(member_access.as_rule(), Rule::member_access_postfix);
        let mut ma_inner = member_access.into_inner();
        let dot_id = Self::next_pair(&mut ma_inner)?;
        let member_identifier = self.parse_dot_identifier(&dot_id)?;

        let mut generic_args: Option<Vec<GenericParameter>> = None;
        // Peek at the next rule without consuming it
        if let Some(peeked_pair) = inner.peek() {
            if peeked_pair.as_rule() == Rule::generic_arguments {
                let generic_args_pair = Self::next_pair(&mut inner)?;
                generic_args = Some(self.parse_generic_arguments(&generic_args_pair)?);
            }
        } else {
            panic!("shouldn't happen in member_call_postfix")
        }

        let args_pair = Self::next_pair(&mut inner)?;
        let args = self.parse_function_call_arguments(&args_pair)?;

        Ok((member_identifier, generic_args, args))
    }

    fn parse_type_def(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let alias_name = self.expect_local_type_identifier_next(&mut inner)?;
        let referenced_type = self.parse_type(inner.next().expect("should work"))?;

        let alias_type = AliasType {
            identifier: alias_name,
            referenced_type,
        };

        Ok(DefinitionKind::AliasDef(alias_type))
    }

    fn parse_struct_type_field(&self, pair: &Pair<Rule>) -> Result<StructTypeField, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::struct_type_field);

        let mut field_inner = Self::convert_into_iterator(pair);
        let field_name = self.expect_field_label_next(&mut field_inner)?;
        let field_type = self.parse_type(Self::next_pair(&mut field_inner)?)?;
        let struct_type_field = StructTypeField {
            field_name,
            field_type,
        };

        Ok(struct_type_field)
    }

    fn parse_struct_type_fields(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<StructTypeField>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::struct_type_fields);
        let mut fields = Vec::new();
        for field_def in Self::convert_into_iterator(pair) {
            let anonymous_struct_field = self.parse_struct_type_field(&field_def)?;

            fields.push(anonymous_struct_field);
        }
        Ok(fields)
    }

    fn parse_struct_type(&self, pair: &Pair<Rule>) -> Result<AnonymousStructType, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::struct_type);
        let fields = Self::right_alternative(pair)?;
        let fields = self.parse_struct_type_fields(&fields)?;
        let struct_type = AnonymousStructType::new(fields);
        Ok(struct_type)
    }

    fn parse_tuple_type_elements(&self, pair: &Pair<Rule>) -> Result<Vec<Type>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::tuple_type);
        let mut types = Vec::new();
        for type_pair in pair.clone().into_inner() {
            let type_value = self.parse_type(type_pair)?;
            types.push(type_value);
        }
        Ok(types)
    }

    fn parse_struct_def(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair).peekable();

        let name_with_optional_type_params =
            self.parse_local_type_identifier_with_optional_type_variables(&inner.next().unwrap())?;

        // struct_type is optional
        // it is valid syntax to just do:
        // `struct SomeStruct`
        let struct_type_pair_option = inner.next();
        let struct_type_result = match struct_type_pair_option {
            Some(struct_type_pair) => Some(self.parse_struct_type(&struct_type_pair)?),
            None => None,
        };

        let struct_type = struct_type_result.map_or_else(
            || AnonymousStructType::new(vec![]),
            |found_result| found_result,
        );

        Ok(DefinitionKind::NamedStructDef(NamedStructDef {
            identifier: name_with_optional_type_params,
            struct_type,
        }))
    }

    fn parse_function_def(
        &self,
        pair: &Pair<Rule>,
        attributes: &[Attribute],
    ) -> Result<DefinitionKind, ParseError> {
        let function_pair = self.next_inner_pair(pair)?;

        match function_pair.as_rule() {
            Rule::normal_function => {
                let mut inner = function_pair.clone().into_inner();
                let signature_pair = inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingFunctionSignature, &function_pair)
                })?;

                let signature = self.parse_function_signature(&signature_pair)?;

                let body = self.parse_block(&inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingFunctionBody, &function_pair)
                })?)?;

                Ok(DefinitionKind::FunctionDef(Function::Internal(
                    FunctionWithBody {
                        declaration: signature,
                        attributes: attributes.to_vec(),
                        body,
                    },
                )))
            }
            Rule::external_function => {
                let mut inner = function_pair.clone().into_inner();
                let id = inner.next().unwrap();
                let signature_pair = inner.next().ok_or_else(|| {
                    self.create_error_pair(
                        SpecificError::MissingFunctionSignature,
                        &function_pair.clone(),
                    )
                })?;

                let signature = self.parse_function_signature(&signature_pair)?;
                Ok(DefinitionKind::FunctionDef(Function::External(
                    self.to_node(&id),
                    signature,
                )))
            }
            _ => {
                Err(self
                    .create_error_pair(SpecificError::ExpectedFunctionDefinition, &function_pair))
            }
        }
    }
    fn parse_function_signature(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<FunctionDeclaration, ParseError> {
        if pair.as_rule() != Rule::function_signature {
            return Err(self.create_error_pair(SpecificError::MissingFunctionSignature, pair));
        }

        let mut inner = pair.clone().into_inner();

        let function_name = self.expect_function_identifier_next(&mut inner)?;

        let mut maybe_next_token = inner.next();
        if let Some(next_rule) = &maybe_next_token
            && next_rule.as_rule() == Rule::generic_type_variables
        {
            //generic_types = self.parse_generic_type_variables(next_rule)?;
            maybe_next_token = inner.next();
        }

        let (parameters, return_type) = match maybe_next_token {
            Some(token) if token.as_rule() == Rule::parameter_list => {
                let params = self.parse_parameters(&token)?;

                let ret_type = if let Some(return_type_pair) = inner.next() {
                    Some(self.parse_return_type(&return_type_pair)?)
                } else {
                    None
                };

                (params, ret_type)
            }

            Some(token) if token.as_rule() == Rule::return_type => {
                (Vec::new(), Some(self.parse_return_type(&token)?))
            }
            _ => (Vec::new(), None),
        };

        Ok(FunctionDeclaration {
            name: function_name.0,
            params: parameters,
            self_parameter: None,
            return_type,
        })
    }

    fn parse_return_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let inner_pair = self.next_inner_pair(pair)?;
        self.parse_type(inner_pair)
    }

    pub fn parse_parameters(&self, pair: &Pair<Rule>) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();

        for param_pair in Self::convert_into_iterator(pair) {
            match param_pair.as_rule() {
                Rule::parameter => {
                    let mut iterator = Self::convert_into_iterator(&param_pair);
                    let may_mut_pair = iterator.next().unwrap();
                    let var = self.parse_maybe_mut_identifier(&may_mut_pair)?;
                    let type_pair = iterator.next().unwrap();
                    let param_type = self.parse_type(type_pair.clone())?;

                    parameters.push(Parameter {
                        variable: var,
                        param_type,
                    });
                }
                Rule::self_parameter => {
                    panic!("should have been handled before parsing parameters")
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::ExpectedParameter, &param_pair)
                    );
                }
            }
        }

        Ok(parameters)
    }

    fn parse_impl_def(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let name_with_optional_type_params =
            self.parse_local_type_identifier_with_optional_type_variables(&inner.next().unwrap())?;

        let mut functions = Vec::new();

        for item_pair in inner {
            if item_pair.as_rule() == Rule::impl_item {
                let inner_item = self.next_inner_pair(&item_pair)?;
                match inner_item.as_rule() {
                    Rule::external_member_function => {
                        let mut inner_inner_item = inner_item.into_inner();
                        let id = inner_inner_item.next().unwrap();
                        let signature =
                            self.parse_member_signature(&inner_inner_item.next().unwrap())?;
                        functions.push(Function::External(self.to_node(&id), signature));
                    }
                    Rule::normal_member_function => {
                        let function_data = self.parse_member_data(&inner_item)?;
                        functions.push(Function::Internal(function_data));
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::ExpectedImplItem, &inner_item)
                        );
                    }
                }
            }
        }

        Ok(DefinitionKind::ImplDef(
            name_with_optional_type_params,
            functions,
        ))
    }

    fn parse_member_signature(&self, pair: &Pair<Rule>) -> Result<FunctionDeclaration, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::member_signature);

        let mut inner = pair.clone().into_inner();

        let name = self.expect_function_identifier_next(&mut inner)?;

        // TODO: Remove the parsing of generic type variables
        let maybe_next_token = inner.peek();
        if let Some(next_rule) = &maybe_next_token
            && next_rule.as_rule() == Rule::generic_type_variables
        {
            // self.parse_generic_type_variables(next_rule)?;
            let _ = inner.next();
        }

        let mut parameters = Vec::new();
        let mut self_parameter = None;
        let mut return_type = None;

        for next_pair in inner {
            match next_pair.as_rule() {
                Rule::self_parameter => {
                    let mut mut_keyword_node = None;
                    let mut self_node = None;

                    for pair in next_pair.into_inner() {
                        match pair.as_rule() {
                            Rule::mut_keyword => {
                                mut_keyword_node = Some(self.to_node(&pair));
                            }
                            Rule::self_identifier => {
                                self_node = Some(self.to_node(&pair));
                            }
                            _ => unreachable!("Unexpected rule in self_parameter"),
                        }
                    }

                    self_parameter = Some(SelfParameter {
                        is_mutable: mut_keyword_node,
                        self_node: self_node.expect("self node must exist"),
                    });
                }
                Rule::parameter_list => {
                    parameters = self.parse_parameters(&next_pair)?;
                }
                Rule::return_type => {
                    return_type = Some(self.parse_return_type(&next_pair)?);
                }
                _ => {}
            }
        }

        Ok(FunctionDeclaration {
            name: name.0,
            params: parameters,
            self_parameter,
            return_type,
        })
    }

    fn parse_member_data(&self, pair: &Pair<Rule>) -> Result<FunctionWithBody, ParseError> {
        if pair.as_rule() != Rule::normal_member_function {
            return Err(self.create_error_pair(SpecificError::ExpectedMemberSignature, pair));
        }

        let mut inner = Self::convert_into_iterator(pair).peekable();

        let mut attributes = Vec::new();
        while let Some(next) = inner.peek() {
            if next.as_rule() == Rule::attribute {
                let attr_pair = inner.next().unwrap();
                let attr = self.parse_attribute(&attr_pair)?;
                attributes.push(attr);
            } else {
                break;
            }
        }

        let signature_pair = Self::next_pair(&mut inner)?;
        let signature = self.parse_member_signature(&signature_pair)?;

        let block_pair = Self::next_pair(&mut inner)?;
        let body = self.parse_block(&block_pair)?;

        Ok(FunctionWithBody {
            attributes,
            declaration: signature,
            body,
        })
    }

    fn parse_for_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let pattern_pair = Self::next_pair(&mut inner)?;
        if pattern_pair.as_rule() != Rule::for_pattern {
            return Err(self.create_error_pair(SpecificError::ExpectedForPattern, &pattern_pair));
        }

        let inner_pattern = self.next_inner_pair(&pattern_pair)?;
        let pattern = match inner_pattern.as_rule() {
            Rule::maybe_mut_identifier => {
                let mut inner_iter = inner_pattern.clone().into_inner();
                let is_mutable = inner_iter
                    .peek()
                    .is_some_and(|p| p.as_rule() == Rule::mut_keyword);

                let is_mut = if is_mutable {
                    let mut_node = self.to_node(&inner_iter.next().unwrap());
                    Some(mut_node)
                } else {
                    None
                };

                let identifier = if is_mutable {
                    self.expect_identifier_next(&mut inner_iter)?.0
                } else {
                    self.to_node(&inner_pattern)
                };

                ForPattern::Single(ForVar { identifier, is_mut })
            }
            Rule::for_pair => {
                let mut vars = Self::convert_into_iterator(&inner_pattern);

                // Parse first variable in the pair
                let first_var_pair = Self::next_pair(&mut vars)?;
                let mut first_inner_iter = first_var_pair.clone().into_inner();
                let first_is_mut = if first_inner_iter
                    .peek()
                    .is_some_and(|p| p.as_rule() == Rule::mut_keyword)
                {
                    Some(self.to_node(&first_inner_iter.next().unwrap()))
                } else {
                    None
                };

                let first_identifier = if first_is_mut.is_some() {
                    self.expect_identifier_next(&mut first_inner_iter)?.0
                } else {
                    self.to_node(&first_var_pair)
                };

                // Parse second variable in the pair
                let second_var_pair = Self::next_pair(&mut vars)?;
                let mut second_inner_iter = second_var_pair.clone().into_inner();
                let second_is_mut = if second_inner_iter
                    .peek()
                    .is_some_and(|p| p.as_rule() == Rule::mut_keyword)
                {
                    Some(self.to_node(&second_inner_iter.next().unwrap()))
                } else {
                    None
                };

                let second_identifier = if second_is_mut.is_some() {
                    self.expect_identifier_next(&mut second_inner_iter)?.0
                } else {
                    self.to_node(&second_var_pair)
                };

                ForPattern::Pair(
                    ForVar {
                        identifier: first_identifier,
                        is_mut: first_is_mut,
                    },
                    ForVar {
                        identifier: second_identifier,
                        is_mut: second_is_mut,
                    },
                )
            }
            _ => {
                return Err(
                    self.create_error_pair(SpecificError::InvalidForPattern, &inner_pattern)
                );
            }
        };

        let next_pair = Self::next_pair(&mut inner)?;
        let iterable_expression = self.parse_arg_expression(&next_pair)?;

        let mut_expression = IterableExpression {
            expression: Box::new(iterable_expression),
        };

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        // Return the ForLoop statement with MutExpression
        Ok(self.create_expr(
            ExpressionKind::ForLoop(pattern, mut_expression, Box::from(body)),
            pair,
        ))
    }

    fn parse_while_loop(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let condition = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        let body = self.parse_expression(&Self::next_pair(&mut inner)?)?;

        Ok(self.create_expr(
            ExpressionKind::WhileLoop(Box::from(condition), Box::from(body)),
            pair,
        ))
    }

    fn parse_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let sub = &Self::right_alternative(pair)?;
        match sub.as_rule() {
            /*
            // TODO: verify that this block is not needed
            Rule::expression => {
                let inner = self.next_inner_pair(sub)?;

                self.parse_expression(&inner)
            }
             */
            Rule::qualified_identifier => Ok(self.create_expr(
                ExpressionKind::VariableReference(Variable::new(self.to_node(sub), None)),
                sub,
            )),
            Rule::block => self.parse_block(sub),

            Rule::assignment => self.parse_assignment_expression(sub),
            Rule::destructuring_assignment => self.parse_destructuring_assignment(sub),
            Rule::variable_definition => self.parse_variable_definition(sub),

            Rule::addition => self.parse_addition(sub),
            Rule::range => self.parse_range(sub),
            Rule::logical => self.parse_logical(sub),
            Rule::none_coalesce => self.parse_none_coalesce(sub),
            Rule::comparison => self.parse_comparison(sub),
            Rule::multiplication => self.parse_multiplication(sub),

            Rule::prefix => self.parse_prefix(sub),

            Rule::match_expr => self.parse_match_expr(sub),
            Rule::initializer_list => self.parse_initializer_list_literal(sub),
            Rule::initializer_pair_list => self.parse_initializer_pair_list(sub),
            Rule::guard_expr => self.parse_guard_expr_list(sub),
            Rule::with_expr => self.parse_with_expr(sub),
            Rule::when_expr => self.parse_when_expr(sub),
            Rule::if_expr => self.parse_if_expression(sub),
            Rule::for_loop => self.parse_for_loop(sub),
            Rule::while_loop => self.parse_while_loop(sub),

            //            Rule::expression | Rule::literal => self.parse_expr(pair),
            Rule::prefix_op | Rule::op_neg | Rule::op_not | Rule::op_borrow_mut_ref => {
                // TODO: maybe not called?
                let op = self.parse_unary_operator(sub)?;
                let expr = self.parse_postfix_expression(&self.next_inner_pair(sub)?)?;
                Ok(self.create_expr(ExpressionKind::UnaryOp(op, Box::new(expr)), sub))
            }

            Rule::postfix => self.parse_postfix_expression(sub), // TODO: maybe not called
            _ => {
                error!(rule=?sub.as_rule(), "rule");
                Err(self.create_error_pair(
                    SpecificError::UnexpectedExpressionType(Self::pair_to_rule(sub)),
                    sub,
                ))
            }
        }
    }

    fn parse_at_least_two_variable_list(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<Variable>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::at_least_two_variables_list);
        let mut variables = Vec::new();
        for item_pair in pair.clone().into_inner() {
            variables.push(self.parse_variable_item(&item_pair)?);
        }
        Ok(variables)
    }

    fn parse_optional_variable_list(&self, pair: &Pair<Rule>) -> Result<Vec<Variable>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::optional_variable_list);
        let mut variables = Vec::new();
        for item_pair in pair.clone().into_inner() {
            variables.push(self.parse_variable_item(&item_pair)?);
        }
        Ok(variables)
    }

    fn parse_maybe_mut_identifier(&self, pair: &Pair<Rule>) -> Result<Variable, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::maybe_mut_identifier);
        let mut inner = pair.clone().into_inner();
        let mut_node = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::mut_keyword {
                // Convert 'mut' to a Node
                let node = self.to_node(&peeked);
                inner.next(); // consume the 'mut' token
                Some(node)
            } else {
                None
            }
        } else {
            None
        };

        let name_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript(
                    "Expected identifier in variable_item".into(),
                ),
                pair,
            )
        })?;

        if name_pair.as_rule() != Rule::identifier {
            return Err(self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript(format!(
                    "Expected identifier, found {:?}",
                    name_pair.as_rule()
                )),
                &name_pair,
            ));
        }

        let variable = Variable {
            name: self.to_node(&name_pair),
            is_mutable: mut_node,
        };

        Ok(variable)
    }

    fn parse_variable_item(&self, pair: &Pair<Rule>) -> Result<Variable, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::variable_item);
        let mut inner = pair.clone().into_inner();
        self.parse_maybe_mut_identifier(&inner.next().unwrap())
    }

    fn parse_assignment_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut iterator = pair.clone().into_inner();
        let lhs_logical =
            self.parse_logical(&iterator.next().expect("parse_assignment_expression"))?;
        if let Some(assignment_op_pair) = iterator.peek().clone() {
            iterator.next();
            let assignment_op = self.parse_assignment_op(&assignment_op_pair)?;
            let rhs_expr = self.parse_expression(&iterator.next().unwrap())?;
            let kind = match assignment_op {
                AssignmentOperatorKind::Assign => {
                    ExpressionKind::Assignment(Box::new(lhs_logical), Box::from(rhs_expr))
                }
                AssignmentOperatorKind::Compound(compound) => {
                    let op = CompoundOperator {
                        node: Self::node_ex(&assignment_op_pair),
                        kind: compound,
                    };
                    ExpressionKind::CompoundAssignment(
                        Box::from(lhs_logical),
                        op,
                        Box::from(rhs_expr),
                    )
                }
            };

            Ok(self.create_expr(kind, pair))
        } else {
            Ok(lhs_logical)
        }
    }

    fn parse_assignment_op(&self, pair: &Pair<Rule>) -> Result<AssignmentOperatorKind, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::assign_op);
        let sub = Self::right_alternative(pair)?;
        let op = match sub.as_rule() {
            Rule::compound_assign_op => {
                AssignmentOperatorKind::Compound(Self::parse_compound_assign_op(&sub)?)
            }
            Rule::normal_assign_op => AssignmentOperatorKind::Assign,
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownAssignmentOperator("strange".to_string()),
                    &sub,
                ));
            }
        };

        Ok(op)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_destructuring_assignment(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::destructuring_assignment);
        let mut inner = pair.clone().into_inner();

        let var_list_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript("missing variable_list".to_string()),
                pair,
            )
        })?;

        let variables = self.parse_at_least_two_variable_list(&var_list_pair)?;

        let rhs_pair = inner.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::UnexpectedRuleInParseScript("missing RHS expression".to_string()),
                pair,
            )
        })?;
        let rhs_expr = self.parse_expression(&rhs_pair)?;

        Ok(self.create_expr(
            ExpressionKind::DestructuringAssignment(variables, Box::new(rhs_expr)),
            &rhs_pair,
        ))
    }

    fn right_alternative<'a>(pair: &Pair<'a, Rule>) -> Result<Pair<'a, Rule>, ParseError> {
        pair.clone()
            .into_inner()
            .next()
            .ok_or_else(|| Self::to_err(SpecificError::CouldNotMoveRight, pair))
    }

    pub fn parse_compound_assign_op(
        op_pair: &Pair<Rule>,
    ) -> Result<CompoundOperatorKind, ParseError> {
        debug_assert_eq!(op_pair.as_rule(), Rule::compound_assign_op);

        let kind = match Self::right_alternative(op_pair)?.as_rule() {
            Rule::add_assign_op => CompoundOperatorKind::Add,
            Rule::sub_assign_op => CompoundOperatorKind::Sub,
            Rule::mul_assign_op => CompoundOperatorKind::Mul,
            Rule::div_assign_op => CompoundOperatorKind::Div,
            Rule::modulo_assign_op => CompoundOperatorKind::Modulo,
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownOperator(format!(
                        "Found unexpected operator rule: {:?}",
                        op_pair.as_rule()
                    )),
                    op_pair,
                ));
            }
        };

        Ok(kind)
    }

    fn parse_maybe_annotation(&self, inner: &mut Pairs<Rule>) -> Result<Option<Type>, ParseError> {
        let result = if let Some(peeked) = inner.peek() {
            if peeked.as_rule() == Rule::type_coerce {
                let type_coerce_pair = inner.next().unwrap();
                let mut type_inner = type_coerce_pair.clone().into_inner();
                let type_name_pair = type_inner.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::MissingTypeName, &type_coerce_pair)
                })?;
                Some(self.parse_type(type_name_pair)?)
            } else {
                None
            }
        } else {
            None
        };
        Ok(result)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_variable_definition(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::variable_definition);
        let mut inner = pair.clone().into_inner();
        let variable_item = Self::next_pair(&mut inner)?;
        let found_var = self.parse_variable_item(&variable_item)?;

        let maybe_annotation = self.parse_maybe_annotation(&mut inner)?;

        let rhs_expr = self.parse_expression(&inner.next().unwrap())?;

        if maybe_annotation.is_some() || found_var.is_mutable.is_some() {
            Ok(self.create_expr(
                ExpressionKind::VariableDefinition(
                    found_var,
                    maybe_annotation,
                    Box::from(rhs_expr),
                ),
                pair,
            ))
        } else {
            Ok(self.create_expr(
                ExpressionKind::VariableAssignment(found_var, Box::from(rhs_expr)),
                pair,
            ))
        }
    }
    fn parse_prefix(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::prefix);
        let _span = pair.as_span();
        let inner = Self::convert_into_iterator(pair);
        let mut expr = None;
        let mut prefix_ops = Vec::new();

        for part in inner {
            match part.as_rule() {
                Rule::prefix_op | Rule::op_neg | Rule::op_not => {
                    let op = self.parse_unary_operator(&part)?;
                    prefix_ops.push(op);
                }
                _ => {
                    expr = Some(self.parse_postfix_expression(&part)?);
                    break;
                }
            }
        }

        let mut final_expr = expr.ok_or_else(|| {
            self.create_error_pair(SpecificError::ExpectedExpressionAfterPrefixOperator, pair)
        })?;

        for op in prefix_ops.into_iter().rev() {
            final_expr = self.create_expr(ExpressionKind::UnaryOp(op, Box::new(final_expr)), pair);
        }

        Ok(final_expr)
    }

    fn parse_binary_operator(&self, pair: &Pair<Rule>) -> Result<BinaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::prefix_op => self.next_inner_pair(pair)?,
            _ => pair.clone(),
        };

        let kind = match op.as_rule() {
            Rule::op_add => BinaryOperatorKind::Add,
            Rule::op_sub => BinaryOperatorKind::Subtract,
            Rule::op_mul => BinaryOperatorKind::Multiply,
            Rule::op_div => BinaryOperatorKind::Divide,
            Rule::op_mod => BinaryOperatorKind::Modulo,
            Rule::op_eq => BinaryOperatorKind::Equal,
            Rule::op_neq => BinaryOperatorKind::NotEqual,
            Rule::op_lt => BinaryOperatorKind::LessThan,
            Rule::op_lte => BinaryOperatorKind::LessEqual,
            Rule::op_gt => BinaryOperatorKind::GreaterThan,
            Rule::op_gte => BinaryOperatorKind::GreaterEqual,
            Rule::op_and => BinaryOperatorKind::LogicalAnd,
            Rule::op_or => BinaryOperatorKind::LogicalOr,
            Rule::op_none_coalesce => BinaryOperatorKind::NoneCoalescingOperator,
            _ => {
                panic!("unknown operator")
            }
        };

        Ok(BinaryOperator {
            kind,
            node: self.to_node(pair),
        })
    }

    fn parse_unary_operator(&self, pair: &Pair<Rule>) -> Result<UnaryOperator, ParseError> {
        let op = match pair.as_rule() {
            Rule::prefix_op => &self.next_inner_pair(pair)?,
            _ => pair,
        };

        let node = self.to_node(op);
        match op.as_rule() {
            Rule::op_neg => Ok(UnaryOperator::Negate(node)),
            Rule::op_not => Ok(UnaryOperator::Not(node)),
            Rule::op_borrow_mut_ref => Ok(UnaryOperator::BorrowMutRef(node)),
            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedUnaryOperator(Self::pair_to_rule(op)),
                op,
            )),
        }
    }

    fn parse_module_segments(&self, pair: Pair<Rule>) -> Vec<Node> {
        pair.into_inner()
            .filter_map(|segment| {
                if segment.as_rule() == Rule::identifier {
                    Some(self.to_node(&segment))
                } else {
                    None
                }
            })
            .collect()
    }

    fn parse_qualified_type_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<QualifiedTypeIdentifier, ParseError> {
        let mut inner_pairs = pair.clone().into_inner();
        let mut generic_types = Vec::new();

        let first = inner_pairs.next().ok_or_else(|| {
            self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(pair)),
                pair,
            )
        })?;

        match first.as_rule() {
            Rule::module_segments => {
                let module_path = self.parse_module_segments(first.clone());
                let type_id = inner_pairs.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::ExpectedTypeIdentifierAfterPath, &first)
                })?;

                let type_identifier = self.parse_local_type_identifier(&type_id)?;

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next()
                    && generic_params.as_rule() == Rule::generic_arguments
                {
                    generic_types = self.parse_generic_arguments(&generic_params)?; // TODO: maybe not used?
                }

                Ok(QualifiedTypeIdentifier::new_with_generics(
                    type_identifier,
                    module_path,
                    generic_types,
                ))
            }
            Rule::type_identifier => {
                let type_identifier = LocalTypeIdentifier(self.to_node(&first));

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next()
                    && generic_params.as_rule() == Rule::generic_arguments
                {
                    generic_types = self.parse_generic_arguments(&generic_params)?;
                }

                Ok(QualifiedTypeIdentifier::new_with_generics(
                    type_identifier,
                    Vec::new(),
                    generic_types,
                ))
            }
            _ => Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(Self::pair_to_rule(&first)),
                &first,
            )),
        }
    }

    fn parse_qualified_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<QualifiedIdentifier, ParseError> {
        let mut inner_pairs = pair.clone().into_inner();
        let mut generic_types = Vec::new();

        let first = inner_pairs
            .next()
            .ok_or_else(|| self.create_error_pair(SpecificError::ExpectedIdentifier, pair))?;

        match first.as_rule() {
            Rule::module_segments => {
                let module_path = self.parse_module_segments(first.clone());
                let id = inner_pairs.next().ok_or_else(|| {
                    self.create_error_pair(SpecificError::ExpectedIdentifierAfterPath, &first)
                })?;

                let identifier = self.to_node(&id);

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next()
                    && generic_params.as_rule() == Rule::generic_arguments
                {
                    // TODO: maybe not used?
                    generic_types = self.parse_generic_arguments(&generic_params)?;
                }

                Ok(QualifiedIdentifier::new_with_generics(
                    identifier,
                    module_path,
                    generic_types,
                ))
            }
            Rule::identifier => {
                let type_identifier = self.to_node(&first);

                // TODO: Maybe loop and check for generic params
                if let Some(generic_params) = inner_pairs.next()
                    && generic_params.as_rule() == Rule::generic_arguments
                {
                    // TODO: maybe not used
                    generic_types = self.parse_generic_arguments(&generic_params)?;
                }

                Ok(QualifiedIdentifier::new_with_generics(
                    type_identifier,
                    Vec::new(),
                    generic_types,
                ))
            }
            _ => Err(self.create_error_pair(SpecificError::ExpectedIdentifier, &first)),
        }
    }

    fn parse_qualified_identifier_expression(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Expression, ParseError> {
        let qualified_identifier = self.parse_qualified_identifier(pair)?;
        Ok(self.create_expr(
            ExpressionKind::IdentifierReference(qualified_identifier),
            pair,
        ))
    }

    fn parse_generic_arguments(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<GenericParameter>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::generic_arguments);

        let inner_pairs = pair.clone().into_inner();
        let mut generic_types = Vec::new();

        for generic_parameter_pair in inner_pairs {
            let generic_parameter = match generic_parameter_pair.as_rule() {
                Rule::type_name => GenericParameter::Type(self.parse_type(generic_parameter_pair)?),
                Rule::generic_argument_int_tuple => {
                    let mut pairs = generic_parameter_pair.clone().into_inner();
                    let first = pairs.next().unwrap();
                    let second = pairs.next().unwrap();
                    let first_node = self.to_node(&first);
                    let second_node = self.to_node(&second);
                    GenericParameter::UnsignedTupleInt(first_node, second_node)
                }
                Rule::unsigned_int_lit => {
                    GenericParameter::UnsignedInt(self.to_node(&generic_parameter_pair))
                }
                _ => panic!("unknown generic parameter"),
            };

            generic_types.push(generic_parameter);
        }

        Ok(generic_types)
    }

    fn parse_local_type_identifier_node(&self, pair: &Pair<Rule>) -> Result<Node, ParseError> {
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(format!("{:?}", pair.as_rule())),
                pair,
            ));
        }
        Ok(self.to_node(pair))
    }

    fn parse_generic_type_variables(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<TypeVariable>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::generic_type_variables);
        let mut type_params = Vec::new();

        let inner = Self::convert_into_iterator(pair);
        for type_variable in inner {
            let mut inner_type_var = type_variable.into_inner();
            let type_identifier_pair = inner_type_var.next().unwrap();

            type_params.push(TypeVariable(
                self.parse_local_type_identifier_node(&type_identifier_pair)?,
            ));
        }
        Ok(type_params)
    }

    fn parse_local_type_identifier_with_optional_type_variables(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<LocalTypeIdentifierWithOptionalTypeVariables, ParseError> {
        debug_assert_eq!(
            pair.as_rule(),
            Rule::type_identifier_optional_type_variables
        );

        let mut inner = pair.clone().into_inner();
        let name = self.expect_local_type_identifier_next(&mut inner)?;

        let type_variables = if let Some(generic_params_pair) = inner.peek() {
            // Peek to see if generic params exist
            if generic_params_pair.as_rule() == Rule::generic_type_variables {
                let _ = inner.next().unwrap(); // Consume the generic_type_params pair
                self.parse_generic_type_variables(&generic_params_pair)?
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };

        Ok(LocalTypeIdentifierWithOptionalTypeVariables {
            name: name.0,
            type_variables,
        })
    }

    fn parse_struct_fields_expressions<'a>(
        &self,
        field_list_pair: &Pair<Rule>,
    ) -> Result<(Vec<FieldExpression>, bool), ParseError> {
        let mut fields = Vec::new();
        let mut has_rest = false;

        for field_pair in field_list_pair.clone().into_inner() {
            match field_pair.as_rule() {
                Rule::struct_field => {
                    let mut field_inner = field_pair.into_inner();
                    let ident = self.expect_field_label_next(&mut field_inner)?;
                    let field_name = FieldName(ident.0);
                    let field_value = self.parse_expression(&field_inner.next().unwrap())?;

                    fields.push(FieldExpression {
                        field_name,
                        expression: field_value,
                    });
                }
                Rule::rest_fields => {
                    has_rest = true;
                }
                _ => {
                    return Err(
                        self.create_error_pair(SpecificError::ExpectedFieldOrRest, &field_pair)
                    );
                }
            }
        }

        Ok((fields, has_rest))
    }

    fn parse_anonymous_struct_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let (fields, has_rest) = self.parse_anonymous_struct_literal_fields(pair)?;
        Ok(self.create_expr(
            ExpressionKind::AnonymousStructLiteral(fields, has_rest),
            pair,
        ))
    }

    fn parse_anonymous_struct_literal_fields(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(Vec<FieldExpression>, bool), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::anonymous_struct_literal);
        let mut inner = Self::convert_into_iterator(pair);
        let (field_expressions, detected_rest) =
            self.parse_struct_fields_expressions(&inner.next().unwrap())?;

        Ok((field_expressions, detected_rest))
    }

    fn parse_struct_literal_optional_fields(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(Vec<FieldExpression>, bool), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::struct_literal_optional_field_list);
        let mut inner = Self::convert_into_iterator(pair);
        let (field_expressions, detected_rest) = if let Some(field_list) = inner.next() {
            self.parse_struct_fields_expressions(&field_list)?
        } else {
            (vec![], false)
        };

        Ok((field_expressions, detected_rest))
    }

    fn parse_struct_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let type_pair = inner.next().unwrap();

        let struct_name = self.parse_qualified_type_identifier(&type_pair)?;

        let anon_fields = inner.next().unwrap();

        let (fields, has_rest) = self.parse_struct_literal_optional_fields(&anon_fields)?;

        Ok(self.create_expr(
            ExpressionKind::NamedStructLiteral(struct_name, fields, has_rest),
            pair,
        ))
    }

    fn parse_context_access(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        Ok(self.create_expr(ExpressionKind::ContextAccess, pair))
    }

    fn parse_static_member_reference(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();

        let type_identifier = self.parse_qualified_type_identifier(&inner.next().unwrap())?;
        let member_name = self.expect_identifier_next(&mut inner)?;

        Ok(self.create_expr(
            ExpressionKind::StaticMemberFunctionReference(type_identifier, member_name.0),
            pair,
        ))
    }

    fn parse_constant_reference(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::constant_reference);
        let mut inner_pairs = pair.clone().into_inner();

        let mut first = inner_pairs.next().unwrap();

        let module_path = if first.as_rule() == Rule::module_segments {
            let path = self.parse_module_segments(first.clone());
            first = inner_pairs.next().unwrap();
            Some(ModulePath(path))
        } else {
            None
        };

        let identifier = QualifiedConstantIdentifier::new(self.to_node(&first), module_path);

        Ok(self.create_expr(ExpressionKind::ConstantReference(identifier), pair))
    }

    fn parse_term(&self, pair2: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair2.as_rule(), Rule::term);
        let sub = &Self::right_alternative(pair2)?;
        match sub.as_rule() {
            Rule::qualified_identifier => self.parse_qualified_identifier_expression(sub),
            Rule::static_member_reference => self.parse_static_member_reference(sub),

            Rule::enum_literal => {
                Ok(self.create_expr(ExpressionKind::Literal(self.parse_enum_literal(sub)?), sub))
            }
            Rule::constant_reference => self.parse_constant_reference(sub),
            Rule::parenthesized => {
                let inner = self.next_inner_pair(sub)?;
                self.parse_expression(&inner)
            }
            Rule::basic_literal => {
                let (literal, node) = self.parse_basic_literal(sub)?;
                Ok(self.create_expr_span(ExpressionKind::Literal(literal), node))
            }
            Rule::struct_literal => self.parse_struct_literal(sub),
            Rule::anonymous_struct_literal => self.parse_anonymous_struct_literal(sub),
            Rule::initializer_list => self.parse_initializer_list_literal(sub),
            Rule::initializer_pair_list => self.parse_initializer_pair_list(sub),

            Rule::interpolated_string => self.parse_interpolated_string(sub),

            Rule::lambda => self.parse_lambda(sub),
            Rule::context_access => self.parse_context_access(sub),

            _ => {
                Err(self
                    .create_error_pair(SpecificError::UnknownTerm(Self::pair_to_rule(sub)), sub))
            }
        }
    }

    fn parse_interpolated_string(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut parts = Vec::new();

        for part_pair in Self::convert_into_iterator(pair) {
            match part_pair.as_rule() {
                Rule::text => {
                    parts.push(StringPart::Literal(
                        self.to_node(&part_pair),
                        self.unescape_string(&part_pair, false)?,
                    ));
                }
                Rule::interpolation => {
                    let inner = self.next_inner_pair(&part_pair.clone())?;
                    let expr = match inner.as_rule() {
                        Rule::expression => self.parse_expression(&inner)?,
                        _ => {
                            return Err(self.create_error_pair(
                                SpecificError::ExpectedExpressionInInterpolation,
                                &inner,
                            ));
                        }
                    };

                    let format = match Self::convert_into_iterator(&part_pair).nth(1) {
                        Some(fmt) => {
                            if fmt.as_rule() == Rule::format_specifier {
                                Some(self.parse_format_specifier(&fmt)?)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };

                    parts.push(StringPart::Interpolation(Box::new(expr), format));
                }
                _ => {
                    return Err(self.create_error_pair(
                        SpecificError::UnexpectedRuleInInterpolation,
                        &part_pair,
                    ));
                }
            }
        }

        Ok(self.create_expr(ExpressionKind::InterpolatedString(parts), pair))
    }

    fn parse_format_specifier(&self, pair: &Pair<Rule>) -> Result<FormatSpecifier, ParseError> {
        let node = self.to_node(pair);
        match pair.as_str() {
            "x" => Ok(FormatSpecifier::LowerHex(node)),
            "X" => Ok(FormatSpecifier::UpperHex(node)),
            "b" => Ok(FormatSpecifier::Binary(node)),
            "f" => Ok(FormatSpecifier::Float(node)),
            s if s.starts_with("..") => {
                let precision: u32 = s[2..s.len() - 1].parse().map_err(|_| {
                    self.create_error_pair(SpecificError::InvalidPrecisionValue, pair)
                })?;
                let typ = match s.chars().last().unwrap() {
                    'f' => PrecisionType::Float(node),
                    's' => PrecisionType::String(node),
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidPrecisionType, pair)
                        )?;
                    }
                };
                Ok(FormatSpecifier::Precision(
                    precision,
                    self.to_node(pair),
                    typ,
                ))
            }
            _ => Err(self.create_error_pair(SpecificError::InvalidFormatSpecifier, pair)),
        }
    }

    fn parse_enum_literal(&self, pair: &Pair<Rule>) -> Result<LiteralKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        // Parse the optional qualified type identifier and type identifier
        let first_pair = inner.next().unwrap();
        let (enum_type, variant_type_identifier) =
            if first_pair.as_rule() == Rule::qualified_type_identifier {
                // We have a qualified type identifier, expect type_identifier next
                let enum_type = Some(self.parse_qualified_type_identifier(&first_pair)?);
                let variant_pair = inner.next().unwrap(); // This should be the type_identifier
                let variant_type_identifier = LocalTypeIdentifier::new(self.to_node(&variant_pair));
                (enum_type, variant_type_identifier)
            } else if first_pair.as_rule() == Rule::type_identifier {
                // No qualified type identifier, just the type identifier
                let variant_type_identifier = LocalTypeIdentifier::new(self.to_node(&first_pair));
                (None, variant_type_identifier)
            } else {
                panic!("internal parse err");
            };

        let enum_variant_literal_kind = match inner.next() {
            Some(fields_pair) => match fields_pair.as_rule() {
                Rule::struct_literal_optional_field_list => {
                    let (field_expressions, detected_rest) =
                        self.parse_struct_literal_optional_fields(&fields_pair)?;
                    EnumVariantLiteralKind::Struct(field_expressions, detected_rest)
                }
                Rule::tuple_fields => {
                    let mut expressions = vec![];
                    for field in Self::convert_into_iterator(&fields_pair) {
                        let field_value = self.parse_expression(&field)?;
                        expressions.push(field_value);
                    }
                    EnumVariantLiteralKind::Tuple(expressions)
                }
                _ => {
                    error!("{:?}, {}", fields_pair.as_rule(), "strange");
                    return Err(
                        self.create_error_pair(SpecificError::UnexpectedVariantField, &fields_pair)
                    );
                }
            },
            _ => EnumVariantLiteralKind::Simple,
        };
        let enum_variant_literal = EnumVariantLiteral {
            qualified_enum_type_name: enum_type,
            name: variant_type_identifier,
            kind: enum_variant_literal_kind,
        };

        Ok(LiteralKind::EnumVariant(enum_variant_literal))
    }

    fn unescape_unicode(
        &self,
        chars: &mut Peekable<Chars>,
        octets: &mut Vec<u8>,
        pair: &Pair<Rule>,
    ) -> Result<(), ParseError> {
        match chars.next() {
            Some('(') => {
                let mut hex_digits = String::new();

                while let Some(&c) = chars.peek() {
                    if c == ')' {
                        break;
                    }
                    if c.is_ascii_hexdigit() && hex_digits.len() < 6 {
                        hex_digits.push(c);
                        chars.next();
                    } else {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        );
                    }
                }

                match chars.next() {
                    Some(')') => {
                        if hex_digits.is_empty() {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }

                        let code = u32::from_str_radix(&hex_digits, 16).map_err(|_| {
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        })?;

                        if code > 0x0010_FFFF {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }

                        if let Some(c) = std::char::from_u32(code) {
                            let mut buf = [0; 4];
                            let encoded = c.encode_utf8(&mut buf);
                            octets.extend_from_slice(encoded.as_bytes());
                        } else {
                            return Err(
                                self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                            );
                        }
                    }
                    _ => {
                        return Err(
                            self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair)
                        );
                    }
                }
            }
            _ => {
                return Err(self.create_error_pair(SpecificError::InvalidUnicodeEscape, pair));
            }
        }
        Ok(())
    }

    fn unescape_hex(
        &self,
        chars: &mut Peekable<Chars>,
        pair: &Pair<Rule>,
    ) -> Result<u8, ParseError> {
        let mut hex_digits = String::new();
        for _ in 0..2 {
            match chars.next() {
                Some(h) if h.is_ascii_hexdigit() => {
                    hex_digits.push(h);
                }
                _ => {
                    return Err(self.create_error_pair(SpecificError::InvalidHexEscape, pair));
                }
            }
        }
        u8::from_str_radix(&hex_digits, 16)
            .map_err(|_| self.create_error_pair(SpecificError::InvalidHexEscape, pair))
    }

    fn unescape_string(&self, pair: &Pair<Rule>, is_literal: bool) -> Result<String, ParseError> {
        let mut octets = Vec::new();

        let raw = if is_literal {
            &pair.as_str()[1..pair.as_str().len() - 1]
        } else {
            pair.as_str()
        };

        let mut chars = raw.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                let Some(next_ch) = chars.next() else {
                    return Err(
                        self.create_error_pair(SpecificError::UnfinishedEscapeSequence, pair)
                    );
                };
                match next_ch {
                    'n' => {
                        octets.push(b'\n');
                    }
                    't' => {
                        octets.push(b'\t');
                    }
                    '\\' => {
                        octets.push(b'\\');
                    }
                    '"' => {
                        octets.push(b'"');
                    }
                    '\'' => {
                        octets.push(b'\'');
                    }
                    // Two hexadecimal digits that result in an `u8`
                    'x' => {
                        let code = self.unescape_hex(&mut chars, pair)?;
                        octets.push(code);
                    }
                    // Unicode character
                    'u' => {
                        self.unescape_unicode(&mut chars, &mut octets, pair)?;
                    }

                    other => {
                        return Err(self.create_error_pair(
                            SpecificError::UnknownEscapeCharacter(other),
                            pair,
                        ));
                    }
                }
            } else {
                let mut buf = [0; 4];
                let utf8_bytes = ch.encode_utf8(&mut buf);
                octets.extend_from_slice(utf8_bytes.as_bytes());
            }
        }

        let output = String::from_utf8(octets)
            .map_err(|_| self.create_error_pair(SpecificError::InvalidUtf8Sequence, pair))?;

        Ok(output)
    }

    fn parse_basic_literal(&self, pair: &Pair<Rule>) -> Result<(LiteralKind, Node), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::basic_literal);
        let inner = self.next_inner_pair(pair)?;
        let literal_kind = match inner.as_rule() {
            Rule::int_lit => LiteralKind::Int,
            Rule::float_lit => LiteralKind::Float,
            Rule::byte_lit => LiteralKind::Byte,
            Rule::string_lit => {
                let processed_string = self.unescape_string(&inner, true)?;
                LiteralKind::String(processed_string)
            }
            Rule::bool_lit => LiteralKind::Bool,
            Rule::none_lit => LiteralKind::None,
            Rule::tuple_lit => {
                let mut expressions = Vec::new();
                for expr_pair in Self::convert_into_iterator(&inner) {
                    expressions.push(self.parse_expression(&expr_pair)?);
                }
                LiteralKind::Tuple(expressions)
            }
            _ => return Err(self.create_error_pair(SpecificError::UnknownLiteral, &inner)),
        };
        Ok((literal_kind, self.to_node(&inner)))
    }

    fn parse_initializer_list_literal(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut elements = Vec::new();
        for element in Self::convert_into_iterator(pair) {
            elements.push(self.parse_expression(&element)?);
        }
        Ok(self.create_expr(
            ExpressionKind::Literal(LiteralKind::InternalInitializerList(elements)),
            pair,
        ))
    }

    fn parse_initializer_pair_list(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut entries = Vec::new();

        for entry_pair in Self::convert_into_iterator(pair) {
            if entry_pair.as_rule() == Rule::map_entry {
                let mut entry_inner = Self::convert_into_iterator(&entry_pair);
                let key = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                let value = self.parse_expression(&Self::next_pair(&mut entry_inner)?)?;
                entries.push((key, value));
            }
        }

        Ok(self.create_expr(
            ExpressionKind::Literal(LiteralKind::InternalInitializerPairList(entries)),
            pair,
        ))
    }

    fn assert_end(pairs: &mut Pairs<Rule>) {
        assert!(pairs.next().is_none());
    }

    fn parse_function_call_postfix(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(Option<Vec<GenericParameter>>, Vec<Expression>), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::function_call_postfix);
        let mut inner = pair.clone().into_inner();

        let mut generic_args: Option<Vec<GenericParameter>> = None;
        let args_pair: Pair<Rule>; // To hold the function_call_args pair

        if let Some(first_inner) = inner.peek() {
            if first_inner.as_rule() == Rule::generic_arguments {
                let generic_args_pair = Self::next_pair(&mut inner)?;
                generic_args = Some(self.parse_generic_arguments(&generic_args_pair)?);

                args_pair = Self::next_pair(&mut inner)?;
            } else {
                args_pair = Self::next_pair(&mut inner)?;
            }
        } else {
            panic!("problem in function_call_postfix");
        }

        debug_assert_eq!(args_pair.as_rule(), Rule::function_call_args);

        let regular_args = self.parse_function_call_arguments(&args_pair)?;

        Self::assert_end(&mut inner);

        Ok((generic_args, regular_args))
    }

    fn parse_arg_expression(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::arg_expression);
        let mut inner = pair.clone().into_inner();
        self.parse_logical(&inner.next().unwrap())
    }

    fn parse_function_call_arguments(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<Expression>, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::function_call_args);
        let inner = pair.clone().into_inner();
        let mut args = Vec::new();

        // Parse arguments
        for arg_pair in inner {
            let expr = self.parse_arg_expression(&arg_pair)?;
            args.push(expr);
        }

        Ok(args)
    }

    #[allow(clippy::too_many_lines)]
    fn parse_type(&self, pair: Pair<Rule>) -> Result<Type, ParseError> {
        match pair.as_rule() {
            Rule::type_name => {
                let mut inner = pair.clone().into_inner();
                let base_type = if let Some(inner_pair) = inner.next() {
                    self.parse_type(inner_pair)?
                } else {
                    panic!("shouldn't get to here")
                };

                let optional_marker = inner
                    .find(|p| p.as_rule() == Rule::optional_marker)
                    .map(|marker_pair| self.to_node(&marker_pair));
                if let Some(found_optional_marker) = optional_marker {
                    Ok(Type::Optional(Box::new(base_type), found_optional_marker))
                } else {
                    Ok(base_type)
                }
            }

            Rule::base_type => {
                let mut inner = pair.into_inner();
                let first = inner.next().unwrap();
                let base_type = self.parse_type(first)?;

                Ok(base_type)
            }
            Rule::function_type => {
                let mut function_inner = pair.into_inner();

                // Parse parameter types
                let param_types = if let Some(params) = function_inner
                    .next()
                    .filter(|p| p.as_rule() == Rule::function_params)
                {
                    params
                        .into_inner()
                        .map(|param| {
                            Ok(TypeForParameter {
                                ast_type: self.parse_type(param).unwrap(),
                                is_mutable: false,
                            })
                        })
                        .collect::<Result<Vec<_>, ParseError>>()?
                } else {
                    Vec::new()
                };

                // Parse return type
                let return_type = self.parse_type(function_inner.next().unwrap())?;

                Ok(Type::Function(param_types, Box::new(return_type)))
            }

            Rule::qualified_type_identifier => {
                let qualified_id = self.parse_qualified_type_identifier(&pair)?;
                Ok(Type::Named(qualified_id))
            }
            Rule::tuple_type => {
                let elements = self.parse_tuple_type_elements(&pair)?;
                Ok(Type::Tuple(elements))
            }
            Rule::fixed_capacity_array_type => self.parse_fixed_capacity_array_type(&pair),
            Rule::slice_view_type => self.parse_slice_view_type(&pair),

            Rule::fixed_capacity_map_type => self.parse_fixed_capacity_map_type(&pair),
            Rule::dynamic_map_type => self.parse_dynamic_map_type(&pair),

            Rule::struct_type => {
                let element_type = self.parse_struct_type(&pair)?;
                Ok(Type::AnonymousStruct(element_type))
            }

            Rule::unit_type => Ok(Type::Unit),
            Rule::never_type => Ok(Type::Never),

            _ => Err(self.create_error_pair(
                SpecificError::UnexpectedTypeRule(format!("{:?}", pair.as_rule())),
                &pair,
            )),
        }
    }

    fn parse_fixed_capacity_map_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let mut inner = pair.clone().into_inner();
        let key_type = self.parse_type(Self::next_pair(&mut inner)?)?;
        let value_type = self.parse_type(Self::next_pair(&mut inner)?)?;
        let size_pair = inner.next().unwrap();
        let size_node = self.to_node(&size_pair);

        Ok(Type::FixedCapacityMap(
            Box::new(key_type),
            Box::new(value_type),
            size_node,
        ))
    }

    fn parse_dynamic_map_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let mut inner = pair.clone().into_inner();
        let key_type = self.parse_type(Self::next_pair(&mut inner)?)?;
        let value_type = self.parse_type(Self::next_pair(&mut inner)?)?;

        Ok(Type::DynamicLengthMap(
            Box::new(key_type),
            Box::new(value_type),
        ))
    }

    fn parse_fixed_capacity_array_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let mut inner = pair.clone().into_inner();
        let element_type = self.parse_type(inner.next().unwrap())?;
        let size_pair = inner.next().unwrap();
        let size_node = self.to_node(&size_pair);
        Ok(Type::FixedCapacityArray(Box::new(element_type), size_node))
    }

    fn parse_slice_view_type(&self, pair: &Pair<Rule>) -> Result<Type, ParseError> {
        let mut inner = pair.clone().into_inner();
        let element_type = self.parse_type(inner.next().unwrap())?;
        Ok(Type::Slice(Box::new(element_type)))
    }

    #[allow(unused)] // TODO: Use this again
    fn parse_local_type_identifier(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<LocalTypeIdentifier, ParseError> {
        if pair.as_rule() != Rule::type_identifier {
            return Err(self.create_error_pair(
                SpecificError::ExpectedTypeIdentifier(format!("{:?}", pair.as_rule())),
                pair,
            ));
        }
        Ok(LocalTypeIdentifier::new(self.to_node(pair)))
    }

    fn parse_enum_def(&self, pair: &Pair<Rule>) -> Result<DefinitionKind, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);

        let name_with_optional_type_params =
            self.parse_local_type_identifier_with_optional_type_variables(&inner.next().unwrap())?;

        let mut variants = Vec::new();

        if let Some(variants_pair) = inner.next()
            && variants_pair.as_rule() == Rule::enum_variants
        {
            for variant_pair in Self::convert_into_iterator(&variants_pair) {
                if variant_pair.as_rule() == Rule::enum_variant {
                    let variant = self.parse_enum_variant(&self.next_inner_pair(&variant_pair)?)?;

                    variants.push(variant);
                }
            }
        }

        Ok(DefinitionKind::EnumDef(
            name_with_optional_type_params,
            variants,
        ))
    }

    fn parse_enum_variant(&self, pair: &Pair<Rule>) -> Result<EnumVariantType, ParseError> {
        let enum_variant = match pair.as_rule() {
            Rule::simple_variant => EnumVariantType::Simple(self.to_node(pair)),
            Rule::direct_variant => {
                let mut inner = Self::convert_into_iterator(pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;
                let type_name = self.parse_type(inner.next().unwrap())?;
                EnumVariantType::Direct(name.0, type_name)
            }
            Rule::tuple_variant => {
                let mut inner = Self::convert_into_iterator(pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let tuple_elements = self.parse_tuple_type_elements(&inner.next().unwrap())?;

                // Convert single-element tuples to direct variants for consistency
                if tuple_elements.len() == 1 {
                    EnumVariantType::Direct(name.0, tuple_elements.into_iter().next().unwrap())
                } else {
                    EnumVariantType::Tuple(name.0, tuple_elements)
                }
            }
            Rule::struct_variant => {
                let mut inner = Self::convert_into_iterator(pair);
                let name = self.expect_local_type_identifier_next(&mut inner)?;

                let struct_type = self.parse_struct_type(&inner.next().unwrap())?;
                EnumVariantType::Struct(name.0, struct_type)
            }
            _ => {
                return Err(self.create_error_pair(
                    SpecificError::UnknownEnumVariant(Self::pair_to_rule(pair)),
                    pair,
                ));
            }
        };

        Ok(enum_variant)
    }

    fn parse_match_expr(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let value = self.parse_arg_expression(&Self::next_pair(&mut inner)?)?;
        let arms_pair = Self::next_pair(&mut inner)?;
        let mut arms = Vec::new();

        for arm_pair in Self::convert_into_iterator(&arms_pair) {
            if arm_pair.as_rule() == Rule::match_arm {
                let mut arm_inner = Self::convert_into_iterator(&arm_pair);
                let pattern = self.parse_match_pattern(&Self::next_pair(&mut arm_inner)?)?;

                // Handle both block and direct expression cases
                let expr = match Self::next_pair(&mut arm_inner)? {
                    block if block.as_rule() == Rule::block => self.parse_block(&block)?,
                    expr => self.parse_expression(&expr)?,
                };

                arms.push(MatchArm {
                    pattern,
                    expression: expr,
                });
            }
        }

        if arms.is_empty() {
            return Err(self.create_error_pair(SpecificError::MustHaveAtLeastOneArm, pair));
        }

        Ok(self.create_expr(ExpressionKind::Match(Box::new(value), arms), pair))
    }

    fn parse_match_pattern(&self, pair: &Pair<Rule>) -> Result<Pattern, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let pattern_inside = inner.next().expect("should have inner");
        match pattern_inside.as_rule() {
            Rule::normal_pattern => {
                let (concrete_pattern, pattern_node) =
                    self.parse_normal_match_pattern(&pattern_inside)?;
                let inner_pairs: Vec<_> = pattern_inside.clone().into_inner().collect();
                let has_guard = inner_pairs
                    .get(1)
                    .is_some_and(|p| p.as_rule() == Rule::guard_clause);

                let guard_clause = if has_guard {
                    Some(self.parse_guard_clause(&inner_pairs[1])?)
                } else {
                    None
                };
                Ok(Pattern::ConcretePattern(
                    pattern_node,
                    concrete_pattern,
                    guard_clause,
                ))
            }
            Rule::wildcard_pattern => Ok(Pattern::Wildcard(self.to_node(pair))),
            _ => Err(self.create_error_pair(SpecificError::MustHaveAtLeastOneArm, pair)),
        }
    }

    fn parse_guard_clause(&self, pair: &Pair<Rule>) -> Result<GuardClause, ParseError> {
        let inner = Self::right_alternative(pair)?;
        let clause = match inner.as_rule() {
            Rule::wildcard_pattern => GuardClause::Wildcard(Self::node_ex(pair)),
            Rule::expression => {
                let mut iterator = inner.into_inner();
                let result = self.parse_expression(&Self::next_pair(&mut iterator)?)?;
                GuardClause::Expression(result)
            }
            _ => {
                return Err(Self::to_err(
                    SpecificError::UnknownExpr("guard_clause".to_string()),
                    pair,
                ))?;
            }
        };

        Ok(clause)
    }

    fn parse_guard_expr_list(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut guard_exprs = Vec::new();

        for expr_pair in Self::convert_into_iterator(pair) {
            match expr_pair.as_rule() {
                Rule::guard_item => {
                    let mut guard_inner = Self::convert_into_iterator(&expr_pair);
                    let guard_clause = Self::next_pair(&mut guard_inner)?;
                    let condition = self.parse_guard_clause(&guard_clause)?;
                    let result = self.parse_expression(&Self::next_pair(&mut guard_inner)?)?;
                    guard_exprs.push(GuardExpr {
                        clause: condition,
                        result,
                    });
                }

                _ => {
                    panic!("Unexpected rule: {:?}", expr_pair.as_rule());
                }
            }
        }

        Ok(self.create_expr(ExpressionKind::Guard(guard_exprs), pair))
    }

    fn parse_enum_pattern(
        &self,
        pattern_type: &Pair<Rule>,
    ) -> Result<(ConcretePattern, Node), ParseError> {
        let mut inner = pattern_type.clone().into_inner(); // Clone first, then use into_inner()
        let variant = self.expect_local_type_identifier_next(&mut inner)?;

        // Check for the optional destructuring_pattern
        let destructuring = if let Some(destructuring_node) = inner.next() {
            self.parse_destructuring_pattern(&destructuring_node)?
        } else {
            // No payload, it's a unit-like enum like `Red`, `Green`, `Blue`
            DestructuringPattern::Unit
        };

        Ok((
            ConcretePattern::EnumPattern(variant.0, destructuring),
            self.to_node(pattern_type),
        ))
    }

    fn parse_destructuring_pattern(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<DestructuringPattern, ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let destructuring_type = inner.next().expect("should have inner");

        match destructuring_type.as_rule() {
            Rule::struct_destruct => {
                let fields = self.parse_struct_destructuring_fields(&destructuring_type)?;
                Ok(DestructuringPattern::Struct { fields })
            }
            Rule::tuple_destruct => {
                let elements = self.parse_tuple_destructuring_elements(&destructuring_type)?;
                if elements.is_empty() {
                    Ok(DestructuringPattern::Unit)
                }
                // Never create tuples with one element - convert to direct destructuring
                else if elements.len() == 1 {
                    match &elements[0] {
                        PatternVariableOrWildcard::Variable(var) => {
                            Ok(DestructuringPattern::None {
                                variable: var.clone(),
                            })
                        }
                        PatternVariableOrWildcard::Wildcard(_) => {
                            // For wildcards, we still need tuple destructuring
                            Ok(DestructuringPattern::Tuple { elements })
                        }
                    }
                } else {
                    assert!(!elements.is_empty(), "tuples can not be zero");
                    assert!(elements.len() > 1, "tuples must be at least two");
                    Ok(DestructuringPattern::Tuple { elements })
                }
            }
            Rule::maybe_mut_identifier => {
                let variable = self.parse_maybe_mut_identifier(&destructuring_type)?;
                Ok(DestructuringPattern::None { variable })
            }
            _ => Err(self.create_error_pair(SpecificError::UnknownMatchType, &destructuring_type)),
        }
    }

    fn parse_struct_destructuring_fields(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<Variable>, ParseError> {
        let mut fields = Vec::new();
        for field_pair in Self::convert_into_iterator(pair) {
            match field_pair.as_rule() {
                Rule::pattern_variable => {
                    let mut inner = Self::convert_into_iterator(&field_pair);
                    let variable_pair = inner.next().expect("should have inner");
                    let variable = self.parse_maybe_mut_identifier(&variable_pair)?;
                    fields.push(variable);
                }
                _ => {} // Skip other rules like commas
            }
        }
        Ok(fields)
    }

    fn parse_tuple_destructuring_elements(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<Vec<PatternVariableOrWildcard>, ParseError> {
        let mut elements = Vec::new();
        for element_pair in Self::convert_into_iterator(pair) {
            match element_pair.as_rule() {
                Rule::pattern_variable_or_wildcard => {
                    let mut inner = Self::convert_into_iterator(&element_pair);
                    let element_inner = inner.next().expect("should have inner");

                    match element_inner.as_rule() {
                        Rule::maybe_mut_identifier => {
                            let variable = self.parse_maybe_mut_identifier(&element_inner)?;
                            elements.push(PatternVariableOrWildcard::Variable(variable));
                        }
                        _ => {
                            // Handle wildcard "_"
                            if element_inner.as_str() == "_" {
                                elements.push(PatternVariableOrWildcard::Wildcard(
                                    self.to_node(&element_inner),
                                ));
                            } else {
                                return Err(self.create_error_pair(
                                    SpecificError::UnknownMatchType,
                                    &element_inner,
                                ));
                            }
                        }
                    }
                }
                _ => {} // Skip other rules like commas
            }
        }
        Ok(elements)
    }

    fn parse_normal_match_pattern(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(ConcretePattern, Node), ParseError> {
        let mut inner = Self::convert_into_iterator(pair);
        let pattern = inner.next().expect("should have inner");

        match pattern.as_rule() {
            Rule::pattern => {
                let mut pattern_inner = Self::convert_into_iterator(&pattern);
                let pattern_type = pattern_inner.next().expect("should have inner");

                match pattern_type.as_rule() {
                    Rule::enum_pattern => self.parse_enum_pattern(&pattern_type),
                    Rule::basic_literal => {
                        let (literal, node) = self.parse_basic_literal(&pattern_type)?;
                        Ok((ConcretePattern::Literal(literal), node))
                    }
                    _ => {
                        Err(self.create_error_pair(SpecificError::UnknownMatchType, &pattern_type))
                    }
                }
            }
            _ => Err(self.create_error_pair(SpecificError::UnknownMatchType, &pattern)),
        }
    }

    fn to_node(&self, pair: &Pair<Rule>) -> Node {
        let pair_span = pair.as_span();
        let span = SpanWithoutFileId {
            offset: pair_span.start() as u32,
            length: (pair_span.end() - pair_span.start()) as u16,
        };

        Node { span }
    }

    fn node_ex(pair: &Pair<Rule>) -> Node {
        let pair_span = pair.as_span();
        let span = SpanWithoutFileId {
            offset: pair_span.start() as u32,
            length: (pair_span.end() - pair_span.start()) as u16,
        };

        Node { span }
    }

    fn to_span(&self, pest_span: pest::Span) -> SpanWithoutFileId {
        SpanWithoutFileId {
            offset: pest_span.start() as u32,
            length: (pest_span.end() - pest_span.start()) as u16,
        }
    }

    fn span(pest_span: pest::Span) -> SpanWithoutFileId {
        SpanWithoutFileId {
            offset: pest_span.start() as u32,
            length: (pest_span.end() - pest_span.start()) as u16,
        }
    }

    fn create_expr(&self, kind: ExpressionKind, pair: &Pair<Rule>) -> Expression {
        self.create_expr_span(kind, self.to_node(pair))
    }

    const fn create_expr_span(&self, kind: ExpressionKind, node: Node) -> Expression {
        //info!(?kind, ?node, "create_expr()");
        Expression { kind, node }
    }

    fn parse_multiplication(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_prefix(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            // Expect the next token to be a multiplication operator, then the next operand.
            let operator = self.parse_binary_operator(&op)?; // op_mul, op_div, or op_mod
            let right = self.parse_prefix(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_addition(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_multiplication(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // op_add or op_sub
            let right = self.parse_multiplication(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_comparison(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_addition(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // e.g. op_lt, op_eq, etc.
            let right = self.parse_addition(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_range(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        let mut inner = pair.clone().into_inner();
        let left = self.parse_comparison(&inner.next().unwrap())?;
        if let Some(op) = inner.next() {
            let right = self.parse_comparison(&inner.next().unwrap())?;
            match op.as_rule() {
                Rule::exclusive_range_op => {
                    return Ok(self.create_expr(
                        ExpressionKind::Range(
                            Box::new(left),
                            Box::new(right),
                            RangeMode::Exclusive,
                        ),
                        pair,
                    ));
                }
                Rule::inclusive_range_op => {
                    return Ok(self.create_expr(
                        ExpressionKind::Range(
                            Box::new(left),
                            Box::new(right),
                            RangeMode::Inclusive,
                        ),
                        pair,
                    ));
                }
                _ => {}
            }
            let operator = self.parse_binary_operator(&op)?; // inclusive_range_op or exclusive_range_op
            Ok(self.create_expr(
                ExpressionKind::BinaryOp(Box::new(left), operator, Box::new(right)),
                pair,
            ))
        } else {
            Ok(left)
        }
    }

    fn parse_none_coalesce(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::none_coalesce);
        let mut inner = pair.clone().into_inner();
        let mut expr = self.parse_range(&inner.next().unwrap())?;
        while let Some(op) = inner.next() {
            let operator = self.parse_binary_operator(&op)?; // op_and or op_or

            assert!(
                !(operator.kind != BinaryOperatorKind::NoneCoalescingOperator),
                "expected ?? in none_coalesce, got {operator:?}"
            );

            let right = self.parse_range(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }
        Ok(expr)
    }

    fn parse_logical(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::logical);

        let mut inner = pair.clone().into_inner();
        // first, parse a full coalesce-expression (so "a ?? b" binds tighter than "&&")
        let mut expr = self.parse_none_coalesce(&inner.next().unwrap())?;

        while let Some(op_pair) = inner.next() {
            let operator = self.parse_binary_operator(&op_pair)?; // && or ||
            let right = self.parse_none_coalesce(&inner.next().unwrap())?;
            expr = self.create_expr(
                ExpressionKind::BinaryOp(Box::new(expr), operator, Box::new(right)),
                pair,
            );
        }

        Ok(expr)
    }

    fn parse_lambda(&self, pair: &Pair<Rule>) -> Result<Expression, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::lambda);
        let mut inner = pair.clone().into_inner();
        let variable_list_pair = inner.next().unwrap();
        let variable_list = self.parse_optional_variable_list(&variable_list_pair)?;
        let expression_pair = inner.next().unwrap();
        let expression = self.parse_expression(&expression_pair)?;

        Ok(self.create_expr(
            ExpressionKind::Lambda(variable_list, Box::new(expression)),
            pair,
        ))
    }

    pub fn parse_attribute(&self, pair: &Pair<Rule>) -> Result<Attribute, ParseError> {
        let inner = pair.clone().into_inner().next().unwrap();
        let is_inner = match inner.as_rule() {
            Rule::outer_attribute => false,
            Rule::inner_attribute => true,
            _ => panic!("must be attribute"),
        };
        let meta_item = inner.into_inner().next().unwrap();
        let (path, args) = self.parse_meta_item(&meta_item)?;

        Ok(Attribute {
            is_inner,
            path,
            args,
            node: self.to_node(pair),
        })
    }

    fn parse_any_meta_item_to_arg(&self, pair: &Pair<Rule>) -> Result<AttributeArg, ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::meta_item);
        let matched_alternative = pair.clone().into_inner().next().unwrap();

        match matched_alternative.as_rule() {
            Rule::meta_path => {
                let path_pair = matched_alternative.clone().into_inner().next().unwrap();
                let path = self.parse_qualified_identifier(&path_pair)?;
                Ok(AttributeArg::Path(path))
            }
            Rule::meta_key_value => {
                let mut inner_items = matched_alternative.clone().into_inner();
                let key_pair = inner_items.next().unwrap();
                let value_pair = inner_items.next().unwrap();
                let key = self.parse_qualified_identifier(&key_pair)?;
                let value_arg = self.parse_meta_value(&value_pair)?;
                Ok(AttributeArg::Function(key, vec![value_arg]))
            }
            Rule::meta_list => {
                let mut inner_items = matched_alternative.clone().into_inner();
                let path_pair = inner_items.next().unwrap();
                let path = self.parse_qualified_identifier(&path_pair)?;
                let args = if let Some(list_pair) = inner_items.next() {
                    self.parse_meta_item_list(&list_pair)?
                } else {
                    vec![]
                };
                Ok(AttributeArg::Function(path, args))
            }
            _ => panic!("unexpected rule inside meta_item"),
        }
    }

    fn parse_meta_item(
        &self,
        pair: &Pair<Rule>,
    ) -> Result<(QualifiedIdentifier, Vec<AttributeArg>), ParseError> {
        debug_assert_eq!(pair.as_rule(), Rule::meta_item);
        let arg = self.parse_any_meta_item_to_arg(pair)?;

        match arg {
            AttributeArg::Path(path) => Ok((path, vec![])),
            AttributeArg::Function(path, args) => Ok((path, args)),
            AttributeArg::Literal(_) => panic!(),
        }
    }

    fn parse_meta_item_list(&self, pair: &Pair<Rule>) -> Result<Vec<AttributeArg>, ParseError> {
        let mut args = Vec::new();
        for item in pair.clone().into_inner() {
            args.push(self.parse_meta_item_arg(&item)?);
        }
        Ok(args)
    }

    fn parse_meta_item_arg(&self, pair: &Pair<Rule>) -> Result<AttributeArg, ParseError> {
        self.parse_any_meta_item_to_arg(pair)
    }

    fn parse_meta_value(&self, pair: &Pair<Rule>) -> Result<AttributeArg, ParseError> {
        let matched_alternative = self.next_inner_pair(pair)?;
        match matched_alternative.as_rule() {
            Rule::basic_literal => {
                let (kind, node) = self.parse_basic_literal(&matched_alternative)?;
                Ok(AttributeArg::Literal(match kind {
                    LiteralKind::Int => AttributeValue::Literal(node, AttributeLiteralKind::Int),
                    LiteralKind::String(s) => {
                        AttributeValue::Literal(node, AttributeLiteralKind::String(s))
                    }
                    LiteralKind::Bool => AttributeValue::Literal(node, AttributeLiteralKind::Bool),
                    _ => panic!("not supported"),
                }))
            }
            Rule::meta_path => {
                let path = self.parse_qualified_identifier(
                    &matched_alternative.clone().into_inner().next().unwrap(),
                )?;
                Ok(AttributeArg::Path(path))
            }
            Rule::meta_list => {
                let mut inner = matched_alternative.clone().into_inner();
                let path = self.parse_qualified_identifier(&inner.next().unwrap())?;
                let args = if let Some(list) = inner.next() {
                    self.parse_meta_item_list(&list)?
                } else {
                    vec![]
                };
                Ok(AttributeArg::Function(path, args))
            }
            _ => panic!("unexpected meta_value {:?}", pair.as_rule()),
        }
    }
}
