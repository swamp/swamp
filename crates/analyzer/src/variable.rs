/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use source_map_node::Node;
use std::rc::Rc;
use swamp_semantic::err::ErrorKind;
use swamp_semantic::ScopeInfo;
use swamp_semantic::{
    ArgumentExpression, BlockScopeMode, Expression, ExpressionKind, Variable, VariableRef,
    VariableType,
};
use swamp_types::prelude::*;

const MAX_VIRTUAL_REGISTER: usize = 24;

/// Common helper function for allocating the next available register from `ScopeInfo`
/// This function uses high watermark approach - simply increment counter, restore on scope pop
pub(crate) const fn allocate_next_register(scope: &mut ScopeInfo) -> Option<u8> {
    if scope.total_scopes.current_register >= MAX_VIRTUAL_REGISTER {
        None
    } else {
        scope.total_scopes.current_register += 1;
        Some(scope.total_scopes.current_register as u8)
    }
}

impl Analyzer<'_> {
    fn try_find_local_variable(&self, node: &Node) -> Option<&VariableRef> {
        let current_scope = self
            .scope
            .active_scope
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        let variable_text = self.get_text_resolved(node).to_string();

        current_scope.lookup.get(&variable_text)
    }

    #[allow(unused)]
    pub(crate) fn find_variable(&mut self, variable: &swamp_ast::Variable) -> VariableRef {
        if let Some(x) = self.try_find_variable(&variable.name) {
            x
        } else {
            self.add_err(ErrorKind::UnknownVariable, &variable.name);
            VariableRef::from(Variable::create_err(self.types().unit()))
        }
    }

    pub(crate) fn try_find_variable(&self, node: &swamp_ast::Node) -> Option<VariableRef> {
        let variable_text = self.get_text(node);

        for scope in self.scope.active_scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.lookup.get(&variable_text.to_string()) {
                return Some(value.clone());
            }
            if scope.mode == BlockScopeMode::Closed {
                break;
            }
        }

        None
    }

    pub(crate) fn create_parameter_resolved(
        &mut self,
        variable: &Node,
        is_mutable: Option<&Node>,
        variable_type_ref: &TypeRef,
    ) {
        let (variable_ref, _name_str) = self.create_variable_like_resolved(
            variable,
            is_mutable,
            variable_type_ref,
            VariableType::Parameter,
        );
    }

    pub(crate) fn create_local_variable(
        &mut self,
        variable: &swamp_ast::Node,
        is_mutable: Option<&swamp_ast::Node>,
        variable_type_ref: &TypeRef,
        concrete_check: bool,
    ) -> VariableRef {
        let debug_text = self.get_text(variable);
        if !debug_text.starts_with('_')
            && concrete_check
            && !variable_type_ref.can_be_stored_in_variable()
        {
            self.add_err(
                ErrorKind::VariableTypeMustBeBlittable(variable_type_ref.clone()),
                variable,
            );
            return Rc::new(Variable::create_err(self.types().unit()));
        }
        self.create_local_variable_resolved(
            &self.to_node(variable),
            Option::from(&self.to_node_option(is_mutable)),
            variable_type_ref,
        )
    }

    pub(crate) fn create_local_variable_parameter_like(
        &mut self,
        variable: &swamp_ast::Node,
        is_mutable: Option<&swamp_ast::Node>,
        variable_type_ref: &TypeRef,
        concrete_check: bool,
    ) -> VariableRef {
        let debug_text = self.get_text(variable);
        if !debug_text.starts_with('_')
            && concrete_check
            && !variable_type_ref.can_be_stored_in_variable()
        {
            self.add_err(
                ErrorKind::VariableTypeMustBeBlittable(variable_type_ref.clone()),
                variable,
            );
            return Rc::new(Variable::create_err(self.types().unit()));
        }
        self.create_local_variable_parameter_like_resolved(
            &self.to_node(variable),
            Option::from(&self.to_node_option(is_mutable)),
            variable_type_ref,
        )
    }

    pub(crate) fn create_variable(
        &mut self,
        variable: &swamp_ast::Variable,
        variable_type_ref: &TypeRef,
    ) -> VariableRef {
        self.create_local_variable(
            &variable.name,
            Option::from(&variable.is_mutable),
            variable_type_ref,
            true,
        )
    }

    pub(crate) fn create_variable_param_like(
        &mut self,
        variable: &swamp_ast::Variable,
        variable_type_ref: &TypeRef,
    ) -> VariableRef {
        self.create_local_variable_parameter_like(
            &variable.name,
            Option::from(&variable.is_mutable),
            variable_type_ref,
            true,
        )
    }

    pub(crate) fn create_local_variable_resolved(
        &mut self,
        variable: &Node,
        is_mutable: Option<&Node>,
        variable_type_ref: &TypeRef,
    ) -> VariableRef {
        let (variable_ref, variable_str) = self.create_variable_like_resolved(
            variable,
            is_mutable,
            variable_type_ref,
            VariableType::Local,
        );

        variable_ref
    }

    pub(crate) fn create_local_variable_parameter_like_resolved(
        &mut self,
        variable: &Node,
        is_mutable: Option<&Node>,
        variable_type_ref: &TypeRef,
    ) -> VariableRef {
        let (variable_ref, variable_str) = self.create_variable_like_resolved(
            variable,
            is_mutable,
            variable_type_ref,
            VariableType::Parameter,
        );

        variable_ref
    }

    pub(crate) fn create_variable_like_resolved(
        &mut self,
        variable: &Node,
        is_mutable: Option<&Node>,
        variable_type_ref: &TypeRef,
        variable_type: VariableType,
    ) -> (VariableRef, String) {
        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            self.add_err_resolved(ErrorKind::OverwriteVariableNotAllowedHere, variable);

            let error_var_ref = VariableRef::new(Variable {
                name: Default::default(),
                assigned_name: "err".to_string(),
                resolved_type: self.types().unit(),
                mutable_node: None,
                variable_type,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                virtual_register: 0,
                is_unused: false,
            });

            return (error_var_ref, "err".to_string());
        }
        let variable_str = self.get_text_resolved(variable).to_string();

        let scope_index = self.scope.active_scope.block_scope_stack.len() - 1;

        let index = { self.scope.active_scope.emit_variable_index() };

        let should_insert_in_scope = !variable_str.starts_with('_');

        let variables_len = &mut self
            .scope
            .active_scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables
            .len();

        // Check for unused mutable variables before incrementing register counter
        if !should_insert_in_scope && is_mutable.is_some() {
            self.add_err_resolved(ErrorKind::UnusedVariablesCanNotBeMut, variable);

            let error_var_ref = VariableRef::new(Variable {
                name: Default::default(),
                assigned_name: "err".to_string(),
                resolved_type: self.types().unit(),
                mutable_node: None,
                variable_type,
                scope_index: 0,
                variable_index: 0,
                unique_id_within_function: 0,
                virtual_register: 0,
                is_unused: false,
            });

            return (error_var_ref, "err".to_string());
        }

        // Only increment register counter when we're actually creating a valid variable
        let maybe_virtual_register = allocate_next_register(&mut self.scope);
        if let Some(virtual_register) = maybe_virtual_register {
            let resolved_variable = Variable {
                name: variable.clone(),
                assigned_name: variable_str.clone(),
                variable_type,
                resolved_type: variable_type_ref.clone(),
                mutable_node: is_mutable.cloned(),
                scope_index,
                variable_index: *variables_len,
                unique_id_within_function: index,
                virtual_register,
                is_unused: !should_insert_in_scope,
            };

            let variable_ref = Rc::new(resolved_variable);

            if should_insert_in_scope {
                let lookups = &mut self
                    .scope
                    .active_scope
                    .block_scope_stack
                    .last_mut()
                    .expect("block scope should have at least one scope")
                    .lookup;
                lookups
                    .insert(variable_str.clone(), variable_ref.clone())
                    .expect("should have checked earlier for variable");
            }

            let variables = &mut self
                .scope
                .active_scope
                .block_scope_stack
                .last_mut()
                .expect("block scope should have at least one scope")
                .variables;
            variables
                .insert(variable_ref.unique_id_within_function, variable_ref.clone())
                .expect("should have checked earlier for variable");

            self.scope
                .total_scopes
                .all_variables
                .push(variable_ref.clone());

            (variable_ref, variable_str)
        } else {
            eprintln!("variable: {variable_str}");
            for var in &self.scope.total_scopes.all_variables {
                eprintln!("var id:{} '{}'", var.virtual_register, var.assigned_name);
            }
            self.add_err_resolved(ErrorKind::OutOfVirtualRegisters, variable);
            let resolved_variable = Variable {
                name: variable.clone(),
                assigned_name: variable_str,
                variable_type,
                resolved_type: variable_type_ref.clone(),
                mutable_node: is_mutable.cloned(),
                scope_index,
                variable_index: *variables_len,
                unique_id_within_function: index,
                virtual_register: 0,
                is_unused: true,
            };

            let variable_ref = Rc::new(resolved_variable);
            (variable_ref, "error".to_string())
        }
    }


    #[allow(clippy::too_many_lines)]
    pub(crate) fn create_variable_binding_for_with(
        &mut self,
        ast_variable: &swamp_ast::Variable,
        converted_expression: ArgumentExpression,
    ) -> Expression {
        let expression_type = converted_expression.ty();

        match converted_expression {
            ArgumentExpression::Expression(expr) => {
                // For immutable bindings or expressions that can't be aliased,
                // create a regular variable definition
                let variable_ref = self.create_local_variable(
                    &ast_variable.name,
                    ast_variable.is_mutable.as_ref(),
                    &expression_type,
                    false,
                );

                let var_def_kind = ExpressionKind::VariableDefinition(variable_ref, Box::new(expr));
                let unit_type = self.types().unit();
                self.create_expr(var_def_kind, unit_type, &ast_variable.name)
            }
            ArgumentExpression::BorrowMutableReference(loc) => {
                // For mutable reference bindings, check if it's a scalar or aggregate type
                if ast_variable.is_mutable.is_none() {
                    return self.create_err(ErrorKind::VariableIsNotMutable, &ast_variable.name);
                }

                // Check if the type is a scalar/primitive type
                let is_scalar = match &*expression_type.kind {
                    TypeKind::Int | TypeKind::Float | TypeKind::Bool | TypeKind::String { .. } => {
                        true
                    }
                    _ => false,
                };

                if is_scalar {
                    // For scalars, treat mutable references as copies for now
                    // TODO: Implement copy-back mechanism later
                    let original_expr = ExpressionKind::VariableAccess(loc.starting_variable);
                    let original_expr_wrapped = self.create_expr(
                        original_expr,
                        expression_type.clone(),
                        &ast_variable.name,
                    );

                    // Create a regular variable definition (copy)
                    let variable_ref = self.create_local_variable(
                        &ast_variable.name,
                        ast_variable.is_mutable.as_ref(),
                        &expression_type,
                        false,
                    );

                    let var_def_kind = ExpressionKind::VariableDefinition(
                        variable_ref,
                        Box::new(original_expr_wrapped),
                    );
                    let unit_type = self.types().unit();
                    self.create_expr(var_def_kind, unit_type, &ast_variable.name)
                } else {
                    // For aggregate types, create a proper alias using VariableDefinitionLValue
                    let variable_ref = self.create_local_variable(
                        &ast_variable.name,
                        ast_variable.is_mutable.as_ref(),
                        &expression_type,
                        false,
                    );

                    // Use the VariableDefinitionLValue expression to bind the variable to the lvalue
                    let expr_kind = ExpressionKind::VariableDefinitionLValue(variable_ref, loc);
                    let unit_type = self.types().unit();
                    self.create_expr(expr_kind, unit_type, &ast_variable.name)
                }
            }
        }
    }

    pub const fn types(&mut self) -> &mut TypeCache {
        &mut self.shared.state.types
    }
}
