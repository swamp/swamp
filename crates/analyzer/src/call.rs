/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::TypeContext;
use crate::{Analyzer, LocationSide};
use source_map_node::Node;
use swamp_semantic::err::ErrorKind;
use swamp_semantic::{ArgumentExpression, Expression, ExpressionKind};
use swamp_types::prelude::*;

pub struct MaybeBorrowMutRefExpression {
    pub ast_expression: swamp_ast::Expression,
    pub has_borrow_mutable_reference: Option<Node>,
}

impl Analyzer<'_> {
    /*
    TODO: @ideas
    The analyzer should be more explicit in how the values are transferred, to make it easier and more consistent for the code generator.

    - PassSimpleValue (For simple (primitive) type parameters, not mut, e.g. `a: Int`)
    - PassSimpleValueWithCopyback (For **mut** simple (primitive) parameters, e.g. `mut a: Int`)
    - PassComplexAddressFromLValue (For complex type parameters, argument is LValue. can be used for both `mut` and not mut, but is mandatory if is mut)
    - PassComplexAddressFromRValueMaterialization (For complex type parameters, argument is RValue. Param must NOT be mut)

    */
    /// # Errors
    ///
    pub fn analyze_argument(
        &mut self,
        fn_parameter: &TypeForParameter,
        argument_expr: &swamp_ast::Expression,
    ) -> ArgumentExpression {
        let context = TypeContext::new_argument(
            &fn_parameter.resolved_type,
            false, // Function arguments cannot provide storage for aggregate return values
        );

        let ref_checked_argument = self.analyze_maybe_ref_expression(argument_expr);

        if fn_parameter.is_mutable {
            if ref_checked_argument.has_borrow_mutable_reference.is_none() {
                // if the parameter is mutable you must pass in mutable reference to it
                let expr = self.create_err(ErrorKind::ArgumentIsNotMutable, &argument_expr.node);
                return ArgumentExpression::Expression(expr);
            }
            let mut_location = self.analyze_to_location(
                &ref_checked_argument.ast_expression,
                &context,
                LocationSide::Rhs,
            );

            ArgumentExpression::BorrowMutableReference(mut_location)
        } else {
            if ref_checked_argument.has_borrow_mutable_reference.is_some() {
                {
                    let expr =
                        self.create_err(ErrorKind::ParameterIsNotMutable, &argument_expr.node);
                    // Why did you pass in a mutable reference to something that can not do anything useful with it
                    return ArgumentExpression::Expression(expr);
                }
            }

            let resolved_expr = self.analyze_expression(&ref_checked_argument.ast_expression, &context.with_ephemeral());
            // Check if this expression needs materialization for fixed-size types
            if self.needs_materialization(&resolved_expr) {
                // and then check if it will be possible to create temporary storage:
                if resolved_expr.ty.is_blittable() {
                    ArgumentExpression::MaterializedExpression(resolved_expr)
                } else {
                    // Error
                    ArgumentExpression::Expression(self.create_err(ErrorKind::CanNotCreateTemporaryStorage, &argument_expr.node))
                }
            } else {
                ArgumentExpression::Expression(resolved_expr)
            }
        }
    }

    /// # Errors
    ///
    pub fn analyze_and_verify_parameters(
        &mut self,
        node: &swamp_ast::Node,
        fn_parameters: &[TypeForParameter],
        arguments: &[swamp_ast::Expression],
    ) -> Vec<ArgumentExpression> {
        if fn_parameters.len() != arguments.len() {
            self.add_err(
                ErrorKind::WrongNumberOfArguments(fn_parameters.len(), arguments.len()),
                node,
            );
            return vec![];
        }

        if fn_parameters.len() > Self::MAX_PARAMETER_COUNT {
            self.add_err(
                ErrorKind::TooManyParameters {
                    encountered: fn_parameters.len(),
                    allowed: Self::MAX_PARAMETER_COUNT,
                },
                node,
            );
            return vec![];
        }

        let mut resolved_arguments = Vec::new();
        for (fn_parameter, argument_expr) in fn_parameters.iter().zip(arguments) {
            let mut_or_immutable = self.analyze_argument(fn_parameter, argument_expr);
            resolved_arguments.push(mut_or_immutable);
        }

        resolved_arguments
    }

    /// # Errors
    ///
    pub fn analyze_mut_or_immutable_expression(
        &mut self,
        expr: &swamp_ast::Expression,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> ArgumentExpression {
        let maybe_borrow_or_normal_expression = self.analyze_maybe_ref_expression(expr);

        if maybe_borrow_or_normal_expression
            .has_borrow_mutable_reference
            .is_some()
        {
            ArgumentExpression::BorrowMutableReference(self.analyze_to_location(
                &maybe_borrow_or_normal_expression.ast_expression,
                context,
                location_side,
            ))
        } else {
            ArgumentExpression::Expression(
                self.analyze_expression(&maybe_borrow_or_normal_expression.ast_expression, context),
            )
        }
    }

    /// Determines if an expression needs materialization (temporary storage)
    /// This applies to expressions that produce values but don't have a direct memory location
    /// Only applies to non-mutable parameters with blittable (fixed-size) types
    /*
        pub(crate) fn rvalue_needs_memory_location_to_materialize_in(
        layout_cache: &mut LayoutCache,
        expr: &Expression,
    ) -> bool {
        let specific_kind_of_expression_needs_memory_target = match &expr.kind {
            // TODO: Should have more robust check here. maybe check primitives instead and invert?
            ExpressionKind::EnumVariantLiteral(_, _)
            | ExpressionKind::TupleLiteral(_)
            | ExpressionKind::InitializerList(_, _)
            | ExpressionKind::InitializerPairList(_, _) => true,
            ExpressionKind::Option(_)
            | ExpressionKind::AnonymousStructLiteral(_)
            | ExpressionKind::CoerceToAny(_) => true,
            _ => false,
        };

        if specific_kind_of_expression_needs_memory_target {
            true
        } else {
            // Easy to forget that you should also check if it's a function call with a return type requiring memory allocation
            match &expr.kind {
                ExpressionKind::InternalCall(_, _)
                | ExpressionKind::HostCall(_, _)
                | ExpressionKind::IntrinsicCallEx(_, _) => {
                    let basic_type = layout_cache.layout(&expr.ty);
                    basic_type.is_aggregate()
                }
                _ => false,
            }
        }
    }
     */
    fn needs_materialization(&self, expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::ConstantAccess(_) => false,
            ExpressionKind::VariableAccess(_) => false,
            ExpressionKind::BinaryOp(_) => true,
            ExpressionKind::UnaryOp(_) => true,
            ExpressionKind::PostfixChain(_, _) => false,
            ExpressionKind::CoerceOptionToBool(_) => false,
            ExpressionKind::CoerceIntToChar(_) => false,
            ExpressionKind::CoerceIntToByte(_) => false,
            ExpressionKind::CoerceToAny(_) => true,
            ExpressionKind::IntrinsicCallEx(_, _) => true,
            ExpressionKind::InternalCall(_, _) => true,
            ExpressionKind::HostCall(_, _) => true,
            ExpressionKind::VariableDefinition(_, _) => true,
            ExpressionKind::VariableDefinitionLValue(_, _) => true,
            ExpressionKind::VariableReassignment(_, _) => true,
            ExpressionKind::Assignment(_, _) => true,
            ExpressionKind::CompoundAssignment(_, _, _) => true,
            ExpressionKind::AnonymousStructLiteral(_) => true,
            ExpressionKind::NamedStructLiteral(_) => true,
            ExpressionKind::FloatLiteral(_) => true,
            ExpressionKind::NoneLiteral => true,
            ExpressionKind::IntLiteral(_) => true,
            ExpressionKind::ByteLiteral(_) => true,
            ExpressionKind::StringLiteral(_) => true,
            ExpressionKind::BoolLiteral(_) => true,
            ExpressionKind::EnumVariantLiteral(_, _) => true,
            ExpressionKind::TupleLiteral(_) => true,
            ExpressionKind::InitializerList(_, _) => true,
            ExpressionKind::InitializerPairList(_, _) => true,
            ExpressionKind::Option(_) => true,
            ExpressionKind::ForLoop(_, _, _) => true,
            ExpressionKind::WhileLoop(_, _) => true,
            ExpressionKind::Block(_) => true,
            ExpressionKind::Match(_) => true,
            ExpressionKind::Guard(_) => true,
            ExpressionKind::If(_, _, _) => true,
            ExpressionKind::When(_, _, _) => true,
            ExpressionKind::TupleDestructuring(_, _, _) => true,
            ExpressionKind::Lambda(_, _) => false,
            ExpressionKind::BorrowMutRef(_) => true,
            ExpressionKind::Error(_) => true,
        }
    }
}
