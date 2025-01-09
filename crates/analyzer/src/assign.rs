use crate::err::ResolveError;
use crate::{wrap_in_some_if_optional, Resolver};
use swamp_script_ast::{CompoundOperator, Expression, Node};
use swamp_script_semantic::{ResolvedAccess, ResolvedExpression};

impl<'a> Resolver<'a> {
    pub(crate) fn resolve_field_assignment(
        &mut self,
        ast_struct_field_expr: &Expression,
        ast_field_name: &Node,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut chain = Vec::new();
        let (resolved_last_type, resolved_first_base_expression) =
            self.collect_field_chain(ast_struct_field_expr, &mut chain)?;

        let _ast_field_name_str = self.get_text(ast_field_name).to_string();
        // Add the last lookup that is part of the field_assignment itself
        let (field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        let source_expression = self.resolve_expression(ast_source_expression)?;
        let wrapped_expression = wrap_in_some_if_optional(&field_type, source_expression);

        Ok(ResolvedExpression::StructFieldAssignment(
            Box::new(resolved_first_base_expression),
            chain,
            Box::from(wrapped_expression),
        ))
    }

    pub(crate) fn resolve_field_assignment_compound(
        &mut self,
        ast_struct_field_expr: &Expression,
        ast_field_name: &Node,
        ast_operator: &CompoundOperator,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut chain = Vec::new();
        let (resolved_last_type, resolved_first_base_expression) =
            self.collect_field_chain(ast_struct_field_expr, &mut chain)?;

        let resolved_operator = self.resolve_compound_operator(ast_operator);

        // Add the last lookup that is part of the field_assignment itself
        let (field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        let source_expression = self.resolve_expression(ast_source_expression)?;
        let wrapped_expression = wrap_in_some_if_optional(&field_type, source_expression);

        Ok(ResolvedExpression::FieldCompoundAssignment(
            Box::new(resolved_first_base_expression),
            chain,
            resolved_operator,
            Box::from(wrapped_expression),
        ))
    }
}
