/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ErrorKind;
use crate::{Analyzer, TypeContext};
use swamp_semantic::{BinaryOperator, BinaryOperatorKind, UnaryOperator, UnaryOperatorKind};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    pub(crate) fn analyze_binary_op(
        &mut self,
        ast_left: &swamp_ast::Expression,
        ast_op: &swamp_ast::BinaryOperator,
        ast_right: &swamp_ast::Expression,
    ) -> Option<(BinaryOperator, TypeRef)> {
        let anything_context = TypeContext::new_anything_argument();
        let left = self.analyze_expression(ast_left, &anything_context);
        let left_type = &*left.ty.kind;

        let right = self.analyze_expression(ast_right, &anything_context);
        let right_type = &*right.ty.kind;

        let kind = Self::convert_binary_operator_kind(ast_op);
        let node = self.to_node(&ast_op.node);

        match (&kind, &left_type, &right_type) {
            // String concatenation - allow any type on the right
            (&BinaryOperatorKind::Add, TypeKind::String, _) => Some((
                BinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node,
                },
                self.shared.state.types.string(),
            )),

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
        let bool_type = self.shared.state.types.bool();
        let (node, kind, require_type) = match ast_op {
            swamp_ast::UnaryOperator::Not(node) => (node, UnaryOperatorKind::Not, Some(&bool_type)),
            swamp_ast::UnaryOperator::Negate(node) => (node, UnaryOperatorKind::Negate, None),
            swamp_ast::UnaryOperator::BorrowMutRef(_) => {
                panic!("unary borrow should have been handled")
            }
        };
        let context = TypeContext::new_unsure_argument(require_type);
        let left = self.analyze_expression(ast_left, &context);
        let resolved_type = left.ty.clone();
        Some((
            UnaryOperator {
                left: Box::new(left),
                kind,
                node: self.to_node(node),
            },
            resolved_type,
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
        }
    }
}
