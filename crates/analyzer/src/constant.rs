/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use swamp_semantic::{Constant, ConstantRef, Expression, ExpressionKind};

impl Analyzer<'_> {
    fn analyze_constant(&mut self, constant: &swamp_ast::ConstantInfo) -> Result<(), Error> {
        let maybe_annotation_type = if let Some(found_ast_type) = &constant.annotation {
            Some(self.analyze_type(found_ast_type)?)
        } else {
            None
        };

        let context = TypeContext::new_unsure_argument(maybe_annotation_type.as_ref());
        let resolved_expr = self.analyze_expression(&constant.expression, &context)?;
        let resolved_type = resolved_expr.ty.clone();
        assert!(resolved_type.can_be_stored_in_field());

        let name_node = self.to_node(&constant.constant_identifier.0);
        let name_text = self.get_text_resolved(&name_node).to_string();
        let constant = Constant {
            name: name_node.clone(),
            assigned_name: name_text,
            id: self.shared.state.constants_in_dependency_order.len() as u32,
            expr: resolved_expr,
            resolved_type,
            function_scope_state: self.function_variables.clone(),
        };

        let const_ref = self
            .shared
            .definition_table
            .add_constant(constant)
            .map_err(|s| self.create_err_resolved(ErrorKind::SemanticError(s), &name_node))?;

        self.shared
            .lookup_table
            .add_constant_link(const_ref.clone())
            .map_err(|s| self.create_err_resolved(ErrorKind::SemanticError(s), &name_node))?;

        // This extra storage of the constants in modules is to have them in analyze / dependency order
        self.shared
            .state
            .constants_in_dependency_order
            .push(const_ref);

        Ok(())
    }

    pub(crate) fn analyze_constant_definition(
        &mut self,
        constant: &swamp_ast::ConstantInfo,
    ) -> Result<(), Error> {
        self.analyze_constant(constant)
    }

    pub(crate) fn analyze_constant_access(
        &self,
        qualified_constant_identifier: &swamp_ast::QualifiedConstantIdentifier,
    ) -> Result<Expression, Error> {
        self.try_find_constant(qualified_constant_identifier)
            .map_or_else(
                || {
                    Err(self.create_err(
                        ErrorKind::UnknownConstant,
                        &qualified_constant_identifier.name,
                    ))
                },
                |constant_ref| {
                    let ty = constant_ref.resolved_type.clone();
                    Ok(self.create_expr(
                        ExpressionKind::ConstantAccess(constant_ref.clone()),
                        ty,
                        &qualified_constant_identifier.name,
                    ))
                },
            )
    }

    #[must_use]
    pub fn try_find_constant(
        &self,
        qualified_constant_identifier: &swamp_ast::QualifiedConstantIdentifier,
    ) -> Option<&ConstantRef> {
        let path = self.get_module_path(qualified_constant_identifier.module_path.as_ref());
        let constant_name = self.get_text(&qualified_constant_identifier.name);

        let maybe_symbol_table = self.shared.get_symbol_table(&path);
        maybe_symbol_table.map_or_else(
            || None,
            |symbol_table| Some(symbol_table.get_constant(constant_name)),
        )?
    }
}
