/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use source_map_node::Node;
use swamp_semantic::ExpressionKind;
use swamp_semantic::{EnumLiteralData, Expression, Fp, Literal};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    /// # Errors
    ///
    /// # Panics
    /// if core hasn't added `Vec`
    #[allow(clippy::too_many_lines)]
    pub fn analyze_complex_literal_to_expression(
        &mut self,
        ast_expression: &swamp_ast::Expression,
        ast_literal_kind: &swamp_ast::LiteralKind,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        let ast_node = &ast_expression.node;
        let (lit_kind, literal_type) = match &ast_literal_kind {
            swamp_ast::LiteralKind::InternalInitializerList(items) => {
                let (encountered_element_type, resolved_items) =
                    self.analyze_internal_initializer_list(ast_node, items, context)?;

                let slice_type = Type::InternalInitializerList(Box::new(encountered_element_type));

                (
                    Literal::InitializerList(slice_type.clone(), resolved_items),
                    slice_type,
                )
            }

            swamp_ast::LiteralKind::InternalInitializerPairList(entries) => {
                let (encountered_key_type, encountered_value_type, resolved_items) =
                    self.analyze_slice_pair_key_and_value_types(ast_node, entries, context)?;

                assert!(!matches!(encountered_key_type, Type::Unit));
                assert!(!matches!(encountered_value_type, Type::Unit));
                let slice_pair_type = Type::InternalInitializerPairList(
                    Box::new(encountered_key_type),
                    Box::new(encountered_value_type),
                );

                (
                    Literal::InitializerPairList(slice_pair_type.clone(), resolved_items),
                    slice_pair_type,
                )
            }

            _ => self.analyze_literal(ast_node, ast_literal_kind, context)?,
        };

        let expression =
            self.create_expr(ExpressionKind::Literal(lit_kind), literal_type, ast_node);

        Ok(expression)
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_literal(
        &mut self,
        ast_node: &swamp_ast::Node,
        ast_literal_kind: &swamp_ast::LiteralKind,
        context: &TypeContext,
    ) -> Result<(Literal, Type), Error> {
        let node_text = self.get_text(ast_node);
        let resolved_literal = match &ast_literal_kind {
            swamp_ast::LiteralKind::Int => (
                Literal::IntLiteral(Self::str_to_int(node_text).map_err(|int_conversion_err| {
                    self.create_err(ErrorKind::IntConversionError(int_conversion_err), ast_node)
                })?),
                Type::Int,
            ),
            swamp_ast::LiteralKind::Float => {
                let float = Self::str_to_float(node_text).map_err(|float_conversion_err| {
                    self.create_err(
                        ErrorKind::FloatConversionError(float_conversion_err),
                        ast_node,
                    )
                })?;
                (Literal::FloatLiteral(Fp::from(float)), Type::Float)
            }
            swamp_ast::LiteralKind::String(processed_string) => (
                Literal::StringLiteral(processed_string.to_string()),
                Type::String,
            ),
            swamp_ast::LiteralKind::Bool => {
                let bool_val = Self::str_to_bool(node_text).map_err(|float_conversion_err| {
                    self.create_err(ErrorKind::BoolConversionError, ast_node)
                })?;
                (Literal::BoolLiteral(bool_val), Type::Bool)
            }
            swamp_ast::LiteralKind::EnumVariant(enum_literal) => {
                let (enum_name, variant_name) = match enum_literal {
                    swamp_ast::EnumVariantLiteral::Simple(enum_name, variant_name) => {
                        (enum_name, variant_name)
                    }
                    swamp_ast::EnumVariantLiteral::Tuple(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                    swamp_ast::EnumVariantLiteral::Struct(enum_name, variant_name, _, _) => {
                        (enum_name, variant_name)
                    }
                };

                let (symbol_table, name) = self.get_symbol_table_and_name(enum_name)?;
                if let Some(enum_type_ref) = symbol_table.get_enum(&name) {
                    let enum_type_clone = enum_type_ref.clone();
                    let enum_type = Type::Enum(enum_type_ref.clone());

                    // Handle enum variant literals in patterns
                    let variant_ref = self.analyze_enum_variant_ref(enum_name, variant_name)?;

                    let resolved_data = match enum_literal {
                        swamp_ast::EnumVariantLiteral::Simple(_, _) => EnumLiteralData::Nothing,
                        swamp_ast::EnumVariantLiteral::Tuple(_node, _variant, ast_expressions) => {
                            let EnumVariantType::Tuple(tuple_data) = &variant_ref else {
                                return Err(self.create_err(
                                    ErrorKind::WrongEnumVariantContainer(variant_ref),
                                    ast_node,
                                ));
                            };

                            let resolved = self.analyze_tuple_type(
                                enum_literal.node(),
                                &tuple_data.fields_in_order,
                                ast_expressions,
                            )?;
                            EnumLiteralData::Tuple(resolved)
                        }
                        swamp_ast::EnumVariantLiteral::Struct(
                            _qualified_type_identifier,
                            variant,
                            anonym_struct_field_and_expressions,
                            detected_rest,
                        ) => {
                            if let EnumVariantType::Struct(ref resolved_variant_struct_ref) =
                                variant_ref
                            {
                                if anonym_struct_field_and_expressions.len()
                                    != resolved_variant_struct_ref
                                        .anon_struct
                                        .field_name_sorted_fields
                                        .len()
                                {
                                    return Err(self.create_err(
                                        ErrorKind::WrongNumberOfArguments(
                                            anonym_struct_field_and_expressions.len(),
                                            resolved_variant_struct_ref
                                                .anon_struct
                                                .field_name_sorted_fields
                                                .len(),
                                        ),
                                        &variant.0,
                                    ));
                                }

                                let resolved = self.analyze_anon_struct_instantiation(
                                    &variant.0.clone(),
                                    &resolved_variant_struct_ref.anon_struct,
                                    anonym_struct_field_and_expressions,
                                    *detected_rest,
                                )?;

                                EnumLiteralData::Struct(resolved)
                            } else {
                                return Err(self.create_err(
                                    ErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                                    &variant.0,
                                ));
                            }
                        }
                    };

                    return Ok((
                        Literal::EnumVariantLiteral(enum_type_clone, variant_ref, resolved_data),
                        enum_type,
                    ));
                }
                return Err(self.create_err(ErrorKind::UnknownEnumType, ast_node));
            }

            swamp_ast::LiteralKind::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.analyze_tuple_literal(expressions)?;
                (
                    Literal::TupleLiteral(tuple_type_ref.clone(), resolved_items),
                    Type::Tuple(tuple_type_ref),
                )
            }
            swamp_ast::LiteralKind::None => {
                if let Some(found_expected_type) = context.expected_type {
                    if let Type::Optional(_some_type) = found_expected_type {
                        return Ok((Literal::NoneLiteral, found_expected_type.clone()));
                    }
                }
                return Err(self.create_err(ErrorKind::NoneNeedsExpectedTypeHint, ast_node));
            }
            &&swamp_ast::LiteralKind::InternalInitializerList(_)
            | &swamp_ast::LiteralKind::InternalInitializerPairList(_) => todo!(),
        };

        Ok(resolved_literal)
    }

    fn analyze_tuple_literal(
        &mut self,
        items: &[swamp_ast::Expression],
    ) -> Result<(Vec<Type>, Vec<Expression>), Error> {
        let expressions = self.analyze_argument_expressions(None, items)?;
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = expr.ty.clone();
            tuple_types.push(item_type);
        }

        Ok((tuple_types, expressions))
    }

    fn analyze_tuple_type(
        &mut self,
        node: &swamp_ast::Node,
        expected_types: &[Type],
        ast_expressions: &Vec<swamp_ast::Expression>,
    ) -> Result<Vec<Expression>, Error> {
        if ast_expressions.len() != expected_types.len() {
            return Err(self.create_err(ErrorKind::WrongNumberOfArguments(0, 0), node));
        }

        let mut expressions = Vec::new();
        for (expected_type, expr) in expected_types.iter().zip(ast_expressions) {
            let context = TypeContext::new_argument(expected_type);
            let resolved_expr = self.analyze_expression(expr, &context)?;
            expressions.push(resolved_expr);
        }

        Ok(expressions)
    }

    #[must_use]
    pub const fn create_err(&self, kind: ErrorKind, ast_node: &swamp_ast::Node) -> Error {
        Error {
            node: self.to_node(ast_node),
            kind,
        }
    }

    #[must_use]
    pub fn create_err_resolved(&self, kind: ErrorKind, resolved_node: &Node) -> Error {
        Error {
            node: resolved_node.clone(),
            kind,
        }
    }
}
