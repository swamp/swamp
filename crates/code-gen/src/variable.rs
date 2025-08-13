/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, ExpressionKind, VariableRef};
use swamp_vm_isa::MemoryOffset;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::VmTypeOrigin;

impl CodeBuilder<'_> {
    pub fn initialize_variable_the_first_time(&mut self, variable: &VariableRef) {
        let target_register = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find variable id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            })
            .clone();
        // For frame-placed variables, we need to emit a LEA instruction first
        // to initialize the register to point to the frame-allocated space
        if let VmTypeOrigin::Frame(frame_region) = target_register.ty.origin {
            self.builder.add_lea_from_frame_region(
                &target_register,
                frame_region,
                &variable.name,
                &format!(
                    "initialize frame-placed variable {}",
                    variable.assigned_name
                ),
            );

            // Clear the memory region for aggregate types
            self.builder.add_frame_memory_clear(
                frame_region,
                &variable.name,
                &format!("clear memory for variable {}", variable.assigned_name),
            );

            // For aggregate types, we also need to initialize the collection metadata (capacity, etc.)
            if target_register.ty.basic_type.is_aggregate() {
                // Clone the register before moving it
                let target_reg_clone = target_register;
                let memory_location =
                    MemoryLocation::new_copy_over_whole_type_with_zero_offset(target_reg_clone);

                // Initialize all collections in the variable, both direct and nested
                self.emit_initialize_memory_for_any_type(
                    &memory_location,
                    &variable.name,
                    &format!(
                        "initialize collections for variable {}",
                        variable.assigned_name
                    ),
                );
            }
        }
    }

    pub(crate) fn emit_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.initialize_variable_the_first_time(variable);

        // Now proceed with the variable assignment, but skip initialization since we already did it
        self.emit_variable_assignment_with_init_flag(variable, expression, ctx, true);
    }

    pub(crate) fn emit_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.emit_variable_assignment_with_init_flag(variable, expression, ctx, false);
    }

    fn emit_variable_assignment_with_init_flag(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
        already_initialized: bool,
    ) {
        let target_register = self
            .variable_registers
            .get(&variable.unique_id_within_function)
            .unwrap_or_else(|| {
                panic!(
                    "could not find variable id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            })
            .clone();

        // For primitives, always pass them using direct register assignment.
        if target_register.ty.basic_type.is_reg_copy() {
            self.emit_expression_into_register(
                &target_register,
                expression,
                "variable scalar",
                ctx,
            );
        } else {
            let memory_location = MemoryLocation {
                base_ptr_reg: target_register.clone(),
                offset: MemoryOffset(0),
                ty: target_register.ty,
            };

            // Check if this is a function call that returns an aggregate
            // If so, we can optimize by letting the function call use the variable's space directly
            let is_function_call_returning_aggregate =
                matches!(
                    &expression.kind,
                    ExpressionKind::InternalCall(_, _)
                        | ExpressionKind::HostCall(_, _)
                        | ExpressionKind::PostfixChain(_, _)
                ) && !self.state.layout_cache.layout(&expression.ty).is_scalar();

            // Parameters should never be initialized since they're already set up by the calling convention
            let is_parameter = matches!(
                variable.variable_type,
                swamp_semantic::VariableType::Parameter
            );

            if is_function_call_returning_aggregate || is_parameter {
                // For function calls returning aggregates or parameters, don't initialize the memory first
                // Let the function call write directly to the variable's allocated space
                self.emit_expression_into_target_memory(
                    &memory_location,
                    expression,
                    "variable assignment (direct function call or parameter)",
                    ctx,
                );
            } else {
                // For other expressions, initialize the memory first (unless already initialized)
                if !already_initialized {
                    self.emit_initialize_memory_for_any_type(
                        &memory_location,
                        &variable.name,
                        "initialize variable for the first time",
                    );
                }

                self.emit_expression_into_target_memory(
                    &memory_location,
                    expression,
                    "variable assignment",
                    ctx,
                );
            }
        }
    }

    pub(crate) fn emit_variable_reassignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.emit_variable_assignment(variable, expression, ctx);
    }
}
