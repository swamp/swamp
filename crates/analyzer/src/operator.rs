/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{Analyzer, TypeContext};
use swamp_semantic::err::ErrorKind;
use swamp_semantic::ExpressionKind::BinaryOp;
use swamp_semantic::{BinaryOperator, BinaryOperatorKind, UnaryOperator, UnaryOperatorKind};
use swamp_types::prelude::*;
use tracing::error;

impl Analyzer<'_> {
    pub(crate) fn analyze_binary_op(
        &mut self,
        ast_left: &swamp_ast::Expression,
        ast_op: &swamp_ast::BinaryOperator,
        ast_right: &swamp_ast::Expression,
    ) -> Option<(BinaryOperator, TypeRef)> {
        let anything_context = TypeContext::new_anything_argument(true); // we assume that equal operator allocates temp memory
        let left = self.analyze_expression(ast_left, &anything_context);
        let left_type = &*left.ty.kind;

        let kind = Self::convert_binary_operator_kind(ast_op);
        let right_context = match kind {
            BinaryOperatorKind::Equal => {
                match left_type {
                    TypeKind::Enum(_enum_type) => {
                        anything_context.with_expected_type(Some(&left.ty), true)
                    }
                    _ => anything_context
                }
            }
            _ => anything_context
        };

        let right = self.analyze_expression(ast_right, &right_context);
        let right_type = &*right.ty.kind;

        let node = self.to_node(&ast_op.node);

        match (&kind, &left_type, &right_type) {
            (&BinaryOperatorKind::NoneCoalesce, _left_kind, _right_kind) => {
                if let TypeKind::Optional(inner_optional) = &left_type {
                    // Right hand side type must be either an optional <T> or an unwrapped T
                    if self.types().compatible_with(inner_optional, &right.ty)
                        || self.types().compatible_with(&left.ty, &right.ty)
                    {
                        let final_type = right.ty.clone();
                        Some((
                            BinaryOperator {
                                left: Box::new(left),
                                right: Box::new(right.clone()),
                                kind,
                                node,
                            },
                            final_type,
                        ))
                    } else {
                        self.add_err(ErrorKind::ExpectedOptional, &ast_op.node);
                        None
                    }
                } else {
                    self.add_err(ErrorKind::ExpectedOptional, &ast_op.node);
                    None
                }
            }

            (&BinaryOperatorKind::Multiply, TypeKind::VecStorage(inner, _), TypeKind::Int) => {
                if *inner.kind != TypeKind::Byte {
                    return None;
                }
                Some((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    self.shared.state.types.string(),
                ))
            }

            (
                &BinaryOperatorKind::Multiply,
                TypeKind::StringStorage(..) | TypeKind::String(..),
                TypeKind::Int,
            ) => Some((
                BinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node,
                },
                self.shared.state.types.string(),
            )),

            // String concatenation - allow any type on the right
            (
                &BinaryOperatorKind::Add,
                TypeKind::VecStorage(inner, _),
                TypeKind::StringStorage(..),
            ) => {
                if *inner.kind != TypeKind::Byte {
                    return None;
                }
                Some((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    self.shared.state.types.string(),
                ))
            }

            // Comparison operators
            (
                BinaryOperatorKind::Equal
                | BinaryOperatorKind::NotEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::GreaterEqual
                | BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessEqual,
                _,
                _,
            ) => {
                if !self.shared.state.types.compatible_with(&left.ty, &right.ty) {
                    self.add_err(
                        ErrorKind::IncompatibleTypes {
                            expected: left.ty.clone(),
                            found: right.ty.clone(),
                        },
                        &ast_op.node,
                    );
                    return None;
                }
                Some((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    self.shared.state.types.bool(),
                ))
            }

            // All other operators require exact type matches
            _ => {
                if !self.shared.state.types.compatible_with(&left.ty, &right.ty) {
                    self.add_err_resolved(
                        ErrorKind::IncompatibleTypes {
                            expected: left.ty.clone(),
                            found: right.ty.clone(),
                        },
                        &node,
                    );

                    return None;
                }
                let result_type = left.ty.clone();
                Some((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    result_type,
                ))
            }
        }
    }

    pub(crate) fn analyze_unary_op(
        &mut self,
        ast_op: &swamp_ast::UnaryOperator,
        ast_left: &swamp_ast::Expression,
    ) -> Option<(UnaryOperator, TypeRef)> {
        //let int_type = self.shared.state.types.int();
        let (node, left, kind) = match ast_op {
            swamp_ast::UnaryOperator::Not(node) => {
                let bool_type = self.shared.state.types.bool();
                let context = TypeContext::new_argument(&bool_type, false);
                let left = self.analyze_expression(ast_left, &context);

                if !matches!(&*left.ty.kind, &TypeKind::Bool) {
                    error!(
                       ty=?left.ty ,
                        "not expects bool"
                    );
                }
                (node, left, UnaryOperatorKind::Not)
            }
            swamp_ast::UnaryOperator::Negate(node) => {
                let context = TypeContext::new_anything_argument(false);
                let left = self.analyze_expression(ast_left, &context);

                (node, left, UnaryOperatorKind::Negate)
            }
            swamp_ast::UnaryOperator::BorrowMutRef(_) => {
                panic!("unary borrow should have been handled")
            }
        };
        Some((
            UnaryOperator {
                left: Box::new(left.clone()),
                kind,
                node: self.to_node(node),
            },
            left.ty,
        ))
    }

    const fn convert_binary_operator_kind(
        binary_operator: &swamp_ast::BinaryOperator,
    ) -> BinaryOperatorKind {
        match binary_operator.kind {
            swamp_ast::BinaryOperatorKind::Add => BinaryOperatorKind::Add,
            swamp_ast::BinaryOperatorKind::Subtract => BinaryOperatorKind::Subtract,
            swamp_ast::BinaryOperatorKind::Multiply => BinaryOperatorKind::Multiply,
            swamp_ast::BinaryOperatorKind::Divide => BinaryOperatorKind::Divide,
            swamp_ast::BinaryOperatorKind::Modulo => BinaryOperatorKind::Modulo,
            swamp_ast::BinaryOperatorKind::LogicalOr => BinaryOperatorKind::LogicalOr,
            swamp_ast::BinaryOperatorKind::LogicalAnd => BinaryOperatorKind::LogicalAnd,
            swamp_ast::BinaryOperatorKind::Equal => BinaryOperatorKind::Equal,
            swamp_ast::BinaryOperatorKind::NotEqual => BinaryOperatorKind::NotEqual,
            swamp_ast::BinaryOperatorKind::LessThan => BinaryOperatorKind::LessThan,
            swamp_ast::BinaryOperatorKind::LessEqual => BinaryOperatorKind::LessEqual,
            swamp_ast::BinaryOperatorKind::GreaterThan => BinaryOperatorKind::GreaterThan,
            swamp_ast::BinaryOperatorKind::GreaterEqual => BinaryOperatorKind::GreaterEqual,
            swamp_ast::BinaryOperatorKind::NoneCoalescingOperator => {
                BinaryOperatorKind::NoneCoalesce
            }
        }
    }
}
