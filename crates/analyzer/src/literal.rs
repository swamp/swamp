/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use source_map_node::Node;
use swamp_semantic::{EnumLiteralExpressions, ExpressionKind};
use swamp_semantic::{Expression, Fp};
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
    ) -> Expression {
        let ast_node = &ast_expression.node;
        let (lit_kind, literal_type) = match &ast_literal_kind {
            swamp_ast::LiteralKind::InternalInitializerList(items) => {
                let (collection_type, resolved_items) =
                    self.analyze_internal_initializer_list(ast_node, items, context);

                (
                    ExpressionKind::InitializerList(collection_type.clone(), resolved_items),
                    collection_type,
                )
            }

            swamp_ast::LiteralKind::InternalInitializerPairList(entries) => {
                let (collection_type, resolved_items) =
                    self.analyze_internal_initializer_pair_list(ast_node, entries, context);

                (
                    ExpressionKind::InitializerPairList(collection_type.clone(), resolved_items),
                    collection_type,
                )
            }

            _ => return self.analyze_literal(ast_node, ast_literal_kind, context),
        };

        self.create_expr(lit_kind, literal_type, ast_node)
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_literal(
        &mut self,
        ast_node: &swamp_ast::Node,
        ast_literal_kind: &swamp_ast::LiteralKind,
        context: &TypeContext,
    ) -> Expression {
        let node_text = self.get_text(ast_node);
        let (expression_kind, ty) = match &ast_literal_kind {
            swamp_ast::LiteralKind::Int => match Self::str_to_int(node_text) {
                Err(int_err) => {
                    return self.create_err(ErrorKind::IntConversionError(int_err), ast_node);
                }
                Ok(int_value) => (
                    ExpressionKind::IntLiteral(int_value),
                    self.shared.state.types.int(),
                ),
            },
            swamp_ast::LiteralKind::Float => match Self::str_to_float(node_text) {
                Err(float_err) => {
                    return self.create_err(ErrorKind::FloatConversionError(float_err), ast_node);
                }
                Ok(float_value) => (
                    ExpressionKind::FloatLiteral(Fp::from(float_value)),
                    self.shared.state.types.float(),
                ),
            },
            swamp_ast::LiteralKind::String(processed_string) => (
                ExpressionKind::StringLiteral(processed_string.to_string()),
                self.shared.state.types.string(),
            ),
            swamp_ast::LiteralKind::Bool => match Self::str_to_bool(node_text) {
                Err(_bool_err) => return self.create_err(ErrorKind::BoolConversionError, ast_node),
                Ok(bool_value) => (
                    ExpressionKind::BoolLiteral(bool_value),
                    self.shared.state.types.bool(),
                ),
            },
            swamp_ast::LiteralKind::EnumVariant(enum_literal) => {
                let (enum_name, variant_name_node) = match enum_literal {
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

                // Get the text first to avoid borrowing conflicts
                let variant_name_text = self.get_text(&variant_name_node.0).to_string();

                // Get the enum type, then release the borrow
                let found_enum_type = {
                    let Some((symbol_table, name)) = self.get_symbol_table_and_name(enum_name)
                    else {
                        self.add_err(ErrorKind::UnknownModule, &enum_name.name.0);
                        return self.create_err(ErrorKind::UnknownEnumVariantType, ast_node);
                    };
                    let Some(found_enum_type) = symbol_table.get_type(&name) else {
                        return self.create_err(ErrorKind::UnknownEnumType, ast_node);
                    };
                    found_enum_type.clone()
                };

                let TypeKind::Enum(enum_type) = &*found_enum_type.kind else {
                    return self.create_err(ErrorKind::UnknownEnumType, ast_node);
                };
                let variant_name = &variant_name_text;
                // Handle enum variant literals in patterns
                let Some(variant_ref) = enum_type.get_variant(variant_name) else {
                    return self
                        .create_err(ErrorKind::UnknownEnumVariantType, &variant_name_node.0);
                };

                let resolved_data = match enum_literal {
                    swamp_ast::EnumVariantLiteral::Simple(_, _) => EnumLiteralExpressions::Nothing,
                    swamp_ast::EnumVariantLiteral::Tuple(_node, _variant, ast_expressions) => {
                        let TypeKind::Tuple(tuple_field_types) = &*variant_ref.payload_type.kind
                        else {
                            return self.create_err(
                                ErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                                ast_node,
                            );
                        };

                        if tuple_field_types.len() != ast_expressions.len() {
                            return self.create_err(
                                ErrorKind::WrongNumberOfArguments(
                                    tuple_field_types.len(),
                                    ast_expressions.len(),
                                ),
                                ast_node,
                            );
                        }

                        let resolved_expression = tuple_field_types
                            .iter()
                            .zip(ast_expressions)
                            .map(|(expected_type, ast_expression)| {
                                let ctx = TypeContext::new_argument(expected_type);
                                self.analyze_expression(ast_expression, &ctx)
                            })
                            .collect();

                        EnumLiteralExpressions::Tuple(resolved_expression)
                    }
                    swamp_ast::EnumVariantLiteral::Struct(
                        _qualified_type_identifier,
                        variant,
                        anonym_struct_field_and_expressions,
                        detected_rest,
                    ) => {
                        let TypeKind::AnonymousStruct(anon_payload) =
                            &*variant_ref.payload_type.kind
                        else {
                            return self.create_err(
                                ErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                                ast_node,
                            );
                        };

                        if anonym_struct_field_and_expressions.len()
                            != anon_payload.field_name_sorted_fields.len()
                        {
                            return self.create_err(
                                ErrorKind::WrongNumberOfArguments(
                                    anonym_struct_field_and_expressions.len(),
                                    anon_payload.field_name_sorted_fields.len(),
                                ),
                                &variant.0,
                            );
                        }

                        let resolved_fields = self.analyze_anon_struct_instantiation(
                            &variant.0.clone(),
                            anon_payload,
                            anonym_struct_field_and_expressions,
                            *detected_rest,
                        );

                        EnumLiteralExpressions::Struct(resolved_fields)
                    }
                };

                (
                    ExpressionKind::EnumVariantLiteral(variant_ref.clone(), resolved_data),
                    found_enum_type.clone(),
                )
            }

            swamp_ast::LiteralKind::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.analyze_tuple_literal(expressions);
                let tuple_type = self.shared.state.types.tuple(tuple_type_ref);
                (ExpressionKind::TupleLiteral(resolved_items), tuple_type)
            }
            swamp_ast::LiteralKind::None => {
                if let Some(found_expected_type) = context.expected_type {
                    let underlying = found_expected_type;
                    if let TypeKind::Optional(_some_type) = &*underlying.kind {
                        (ExpressionKind::NoneLiteral, underlying.clone())
                    } else {
                        return self.create_err(ErrorKind::NoneNeedsExpectedTypeHint, ast_node);
                    }
                } else {
                    return self.create_err(ErrorKind::NoneNeedsExpectedTypeHint, ast_node);
                }
            }
            &&swamp_ast::LiteralKind::InternalInitializerList(_)
            | &swamp_ast::LiteralKind::InternalInitializerPairList(_) => todo!(),
        };

        self.create_expr(expression_kind, ty, ast_node)
    }

    fn analyze_tuple_literal(
        &mut self,
        items: &[swamp_ast::Expression],
    ) -> (Vec<TypeRef>, Vec<Expression>) {
        let expressions = self.analyze_argument_expressions(None, items);
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = expr.ty.clone();
            tuple_types.push(item_type);
        }

        (tuple_types, expressions)
    }

    fn analyze_tuple_type(
        &mut self,
        node: &swamp_ast::Node,
        expected_types: &[TypeRef],
        ast_expressions: &Vec<swamp_ast::Expression>,
    ) -> Vec<Expression> {
        if ast_expressions.len() != expected_types.len() {
            return vec![self.create_err(
                ErrorKind::WrongNumberOfArguments(expected_types.len(), ast_expressions.len()),
                node,
            )];
        }

        let mut expressions = Vec::new();
        for (expected_type, expr) in expected_types.iter().zip(ast_expressions) {
            let context = TypeContext::new_argument(expected_type);
            let resolved_expr = self.analyze_expression(expr, &context);
            expressions.push(resolved_expr);
        }

        expressions
    }

    pub fn add_err(&mut self, kind: ErrorKind, ast_node: &swamp_ast::Node) {
        self.add_err_resolved(kind, &self.to_node(ast_node));
    }

    pub(crate) fn add_err_resolved(&mut self, kind: ErrorKind, node: &Node) {
        let err = Error {
            node: node.clone(),
            kind,
        };
        self.errors.push(err);
    }

    #[must_use]
    pub fn create_err_vec(
        &mut self,
        kind: ErrorKind,
        ast_node: &swamp_ast::Node,
    ) -> Vec<Expression> {
        vec![self.create_err(kind, ast_node)]
    }

    #[must_use]
    pub fn create_err(&mut self, kind: ErrorKind, ast_node: &swamp_ast::Node) -> Expression {
        self.add_err(kind, ast_node);

        Expression {
            ty: self.types().unit(),
            node: self.to_node(ast_node),
            kind: ExpressionKind::Error,
        }
    }
    #[must_use]
    pub fn create_err_resolved(&mut self, kind: ErrorKind, resolved_node: &Node) -> Expression {
        self.add_err_resolved(kind, resolved_node);

        Expression {
            ty: self.types().unit(),
            node: resolved_node.clone(),
            kind: ExpressionKind::Error,
        }
    }
}
