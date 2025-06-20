/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::TypeContext;
use crate::err::ErrorKind;
use crate::{Analyzer, LocationSide};
use source_map_node::Node;
use swamp_semantic::ArgumentExpression;
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
        let context = TypeContext::new_argument(&fn_parameter.resolved_type);
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
            let resolved_expr = self.analyze_expression(argument_expr, &context);
            ArgumentExpression::Expression(resolved_expr)
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
}
