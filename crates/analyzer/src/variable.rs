/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use source_map_node::Node;
use std::rc::Rc;
use swamp_semantic::{
    ArgumentExpression, BlockScopeMode, Expression, ExpressionKind, Variable, VariableRef,
    VariableType,
};
use swamp_types::prelude::*;

impl Analyzer<'_> {
    fn try_find_local_variable(&self, node: &Node) -> Option<&VariableRef> {
        let current_scope = self
            .scope
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        let variable_text = self.get_text_resolved(node).to_string();

        current_scope.variables.get(&variable_text)
    }

    #[allow(unused)]
    pub(crate) fn find_variable(
        &self,
        variable: &swamp_ast::Variable,
    ) -> Result<VariableRef, Error> {
        self.try_find_variable(&variable.name).map_or_else(
            || Err(self.create_err(ErrorKind::UnknownVariable, &variable.name)),
            Ok,
        )
    }

    pub(crate) fn try_find_variable(&self, node: &swamp_ast::Node) -> Option<VariableRef> {
        let variable_text = self.get_text(node);

        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(&variable_text.to_string()) {
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
    ) -> Result<(), Error> {
        let (variable_ref, _name_str, _should_be_inserted) = self.create_variable_like_resolved(
            variable,
            is_mutable,
            variable_type_ref,
            VariableType::Parameter,
        )?;

        self.function_parameters.push(variable_ref);

        Ok(())
    }

    pub(crate) fn create_local_variable(
        &mut self,
        variable: &swamp_ast::Node,
        is_mutable: Option<&swamp_ast::Node>,
        variable_type_ref: &TypeRef,
        concrete_check: bool,
    ) -> Result<VariableRef, Error> {
        let debug_text = self.get_text(variable);
        if !debug_text.starts_with('_') {
            // TODO: Add back concrete type checking when public method is available
        }
        self.create_local_variable_resolved(
            &self.to_node(variable),
            Option::from(&self.to_node_option(is_mutable)),
            variable_type_ref,
        )
    }

    pub(crate) fn create_variable(
        &mut self,
        variable: &swamp_ast::Variable,
        variable_type_ref: &TypeRef,
    ) -> Result<VariableRef, Error> {
        self.create_local_variable(
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
    ) -> Result<VariableRef, Error> {
        let (variable_ref, variable_str, should_insert_in_scope) = self
            .create_variable_like_resolved(
                variable,
                is_mutable,
                variable_type_ref,
                VariableType::Local,
            )?;
        if should_insert_in_scope {
            self.function_variables.push(variable_ref.clone());
        }

        Ok(variable_ref)
    }

    pub(crate) fn create_variable_like_resolved(
        &mut self,
        variable: &Node,
        is_mutable: Option<&Node>,
        variable_type_ref: &TypeRef,
        variable_type: VariableType,
    ) -> Result<(VariableRef, String, bool), Error> {
        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(
                self.create_err_resolved(ErrorKind::OverwriteVariableNotAllowedHere, variable)
            );
        }
        let variable_str = self.get_text_resolved(variable).to_string();

        let scope_index = self.scope.block_scope_stack.len() - 1;

        let index = { self.scope.emit_variable_index() };

        let should_insert_in_scope = !variable_str.starts_with('_');
        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = Variable {
            name: variable.clone(),
            assigned_name: variable_str.clone(),
            variable_type,
            resolved_type: variable_type_ref.clone(),
            mutable_node: is_mutable.cloned(),
            scope_index,
            variable_index: variables.len(),
            unique_id_within_function: index,
            is_unused: !should_insert_in_scope,
        };

        let variable_ref = Rc::new(resolved_variable);

        if !should_insert_in_scope && is_mutable.is_some() {
            return Err(self.create_err_resolved(ErrorKind::UnusedVariablesCanNotBeMut, variable));
        }

        if should_insert_in_scope {
            variables
                .insert(variable_str.clone(), variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        Ok((variable_ref, variable_str, should_insert_in_scope))
    }

    #[allow(clippy::unnecessary_wraps)]
    pub(crate) fn create_local_variable_generated(
        &mut self,
        variable_str: &str,
        is_mutable: bool,
        variable_type_ref: &TypeRef,
    ) -> Result<VariableRef, Error> {
        let scope_index = self.scope.block_scope_stack.len() - 1;

        let index_within_function = self.scope.emit_variable_index();

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let is_marked_as_unused = variable_str.starts_with('_');

        let actual_name = if is_marked_as_unused {
            format!("_{index_within_function}")
        } else {
            variable_str.to_string()
        };

        let resolved_variable = Variable {
            name: Node::default(),
            assigned_name: actual_name.clone(),
            resolved_type: variable_type_ref.clone(),
            variable_type: VariableType::Local,
            mutable_node: if is_mutable {
                Some(Node::default())
            } else {
                None
            },
            scope_index,
            variable_index: variables.len(),
            unique_id_within_function: index_within_function,
            is_unused: is_marked_as_unused,
        };

        let variable_ref = Rc::new(resolved_variable);

        if !is_marked_as_unused {
            variables
                .insert(actual_name, variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        self.function_variables.push(variable_ref.clone());

        Ok(variable_ref)
    }

    pub(crate) fn create_variable_binding_for_with(
        &mut self,
        ast_variable: &swamp_ast::Variable,
        converted_expression: ArgumentExpression,
    ) -> Result<Expression, Error> {
        let expression_type = converted_expression.ty().clone();

        let variable_ref = if let ArgumentExpression::Expression(expr) = &converted_expression {
            if let ExpressionKind::VariableAccess(source_var) = &expr.kind {
                if ast_variable.is_mutable.is_some() && !source_var.is_mutable() {
                    return Err(
                        self.create_err(ErrorKind::VariableIsNotMutable, &ast_variable.name)
                    );
                }

                let scope_index = self.scope.block_scope_stack.len() - 1;
                let variable_str = self.get_text(&ast_variable.name).to_string();
                let node = self.to_node(&ast_variable.name);
                let mut_node = ast_variable.is_mutable.as_ref().map(|n| self.to_node(n));

                let variables = &mut self
                    .scope
                    .block_scope_stack
                    .last_mut()
                    .expect("block scope should have at least one scope")
                    .variables;

                let resolved_variable = Variable {
                    name: node,
                    assigned_name: variable_str.clone(),
                    variable_type: VariableType::Local,
                    resolved_type: source_var.resolved_type.clone(),
                    mutable_node: mut_node.or_else(|| source_var.mutable_node.clone()),
                    scope_index,
                    variable_index: variables.len(),
                    unique_id_within_function: source_var.unique_id_within_function, // Reuse the same ID
                    is_unused: false,
                };

                let alias_ref = Rc::new(resolved_variable);
                variables
                    .insert(variable_str, alias_ref.clone())
                    .expect("should have checked earlier for variable");

                alias_ref
            } else {
                self.create_local_variable(
                    &ast_variable.name,
                    ast_variable.is_mutable.as_ref(),
                    &expression_type,
                    false,
                )?
            }
        } else {
            self.create_local_variable(
                &ast_variable.name,
                ast_variable.is_mutable.as_ref(),
                &expression_type,
                false,
            )?
        };

        // Create a variable definition expression
        let source_expr = match converted_expression {
            ArgumentExpression::Expression(expr) => expr,
            ArgumentExpression::BorrowMutableReference(loc) => {
                let ast_node = swamp_ast::Node {
                    span: swamp_ast::SpanWithoutFileId {
                        offset: loc.node.span.offset,
                        length: loc.node.span.length,
                    },
                };
                self.create_expr(
                    ExpressionKind::VariableAccess(loc.starting_variable.clone()),
                    loc.starting_variable.resolved_type.clone(),
                    &ast_node,
                )
            }
        };

        let expr_kind = ExpressionKind::VariableDefinition(variable_ref, Box::new(source_expr));
        let unit_type = self.shared.state.types.unit();
        let expr = self.create_expr(expr_kind, unit_type, &ast_variable.name);

        Ok(expr)
    }
}
