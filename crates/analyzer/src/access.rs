/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::TypeContext;
use crate::err::Error;
use swamp_semantic::Literal::BoolLiteral;
use swamp_semantic::{Expression, ExpressionKind, Function, MutRefOrImmutableExpression};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    #[must_use]
    pub fn lookup_associated_function(&self, ty: &Type, function_name: &str) -> Option<Function> {
        let x = self
            .shared
            .state
            .instantiator
            .associated_impls
            .get_member_function(ty, function_name)
            .cloned();

        if let Some(found_func_ref) = x {
            Some(found_func_ref.as_ref().clone())
        } else {
            None
        }
    }

    pub(crate) fn analyze_min_max_expr(
        &mut self,
        min_expr: &swamp_ast::Expression,
        max_expr: &swamp_ast::Expression,
    ) -> Result<(Expression, Expression), Error> {
        let context = TypeContext::new_argument(&Type::Int);

        let resolved_min = self.analyze_expression(min_expr, &context)?;
        let resolved_max = self.analyze_expression(max_expr, &context)?;

        Ok((resolved_min, resolved_max))
    }

    /// # Errors
    ///
    /// #  Panics
    /// If core hasn't added the `Range` type
    pub fn analyze_range(
        &mut self,
        min_expr: &swamp_ast::Expression,
        max_expr: &swamp_ast::Expression,
        mode: &swamp_ast::RangeMode,
        ast_node: &swamp_ast::Node,
    ) -> Result<Expression, Error> {
        let (min, max) = self.analyze_min_max_expr(min_expr, max_expr)?;

        let range_type = self
            .shared
            .core_symbol_table
            .get_type("Range")
            .unwrap()
            .clone();

        let is_inclusive = matches!(mode, swamp_ast::RangeMode::Inclusive);

        let bool_expr_kind = ExpressionKind::Literal(BoolLiteral(is_inclusive));
        let bool_expr = self.create_expr(bool_expr_kind, Type::Bool, ast_node);

        let call_kind = self.create_static_call(
            "new",
            &[
                MutRefOrImmutableExpression::Expression(min),
                MutRefOrImmutableExpression::Expression(max),
                MutRefOrImmutableExpression::Expression(bool_expr),
            ],
            ast_node,
            &range_type,
        )?;

        Ok(self.create_expr(call_kind, range_type, ast_node))
    }
}
