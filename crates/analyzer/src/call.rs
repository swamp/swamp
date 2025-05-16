/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::TypeContext;
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, LocationSide};
use source_map_node::Node;
use swamp_semantic::MutRefOrImmutableExpression;
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
    pub fn analyze_argument(
        &mut self,
        fn_parameter: &TypeForParameter,
        argument_expr: &swamp_ast::Expression,
    ) -> Result<MutRefOrImmutableExpression, Error> {
        let context = TypeContext::new_argument(&fn_parameter.resolved_type);
        let ref_checked_argument = self.analyze_maybe_ref_expression(&argument_expr)?;

        let mut_or_immutable = if fn_parameter.is_mutable {
            if ref_checked_argument.has_borrow_mutable_reference.is_none() {
                // if the parameter is mutable you must pass in mutable reference to it
                return Err(self.create_err(ErrorKind::ArgumentIsNotMutable, &argument_expr.node));
            }
            let mut_location = self.analyze_to_location(
                &ref_checked_argument.ast_expression,
                &context,
                LocationSide::Rhs,
            )?;

            MutRefOrImmutableExpression::Location(mut_location)
        } else {
            if ref_checked_argument.has_borrow_mutable_reference.is_some() {
                // Why did you pass in a mutable reference to something that can not do anything useful with it
                return Err(self.create_err(ErrorKind::ParameterIsNotMutable, &argument_expr.node));
            }
            let resolved_expr = self.analyze_expression(&argument_expr, &context)?;
            MutRefOrImmutableExpression::Expression(resolved_expr)
        };

        Ok(mut_or_immutable)
    }

    /// # Errors
    ///
    pub fn analyze_and_verify_parameters(
        &mut self,
        node: &Node,
        fn_parameters: &[TypeForParameter],
        arguments: &[swamp_ast::Expression],
    ) -> Result<Vec<MutRefOrImmutableExpression>, Error> {
        if fn_parameters.len() != arguments.len() {
            return Err(self.create_err_resolved(
                ErrorKind::WrongNumberOfArguments(fn_parameters.len(), arguments.len()),
                node,
            ));
        }

        let mut resolved_arguments = Vec::new();
        for (fn_parameter, argument_expr) in fn_parameters.iter().zip(arguments) {
            let mut_or_immutable = self.analyze_argument(fn_parameter, argument_expr)?;
            resolved_arguments.push(mut_or_immutable);
        }

        Ok(resolved_arguments)
    }

    /// # Errors
    ///
    pub fn analyze_mut_or_immutable_expression(
        &mut self,
        expr: &swamp_ast::Expression,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> Result<MutRefOrImmutableExpression, Error> {
        let maybe_borrow_or_normal_expression = self.analyze_maybe_ref_expression(&expr)?;

        let expression_or_location =
            if maybe_borrow_or_normal_expression
                .has_borrow_mutable_reference
                .is_some()
            {
                MutRefOrImmutableExpression::Location(self.analyze_to_location(
                    &maybe_borrow_or_normal_expression.ast_expression,
                    context,
                    location_side,
                )?)
            } else {
                MutRefOrImmutableExpression::Expression(self.analyze_expression(
                    &maybe_borrow_or_normal_expression.ast_expression,
                    context,
                )?)
            };

        Ok(expression_or_location)
    }
}
