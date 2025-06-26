/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

//! Assignment helper functions for the code builder emitter.

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{CompoundOperatorKind, Expression, TargetAssignmentLocation, VariableRef};
use swamp_types::{Type, TypeKind};
use swamp_vm_types::types::{Destination, TypedRegister, VmTypeOrigin};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
    /// Emits code for an assignment operation (lhs = rhs).
    ///
    /// In compiler terminology:
    ///
    /// - The left-hand side (lhs) must be a lvalue (addressable storage location)
    /// - The right-hand side (rhs) is materialized as a rvalue (computed value)
    ///
    /// The assignment process involves two steps:
    ///
    /// 1. Compute the destination address of the lvalue
    ///
    /// 2. Materialize the rhs expression into that destination
    ///
    /// This approach, known as "destination passing style", avoids unnecessary
    /// copies by materializing the rhs expression directly into the storage location
    /// of the lhs.
    ///
    /// # Example in Source
    ///
    /// ```ignore
    /// foo.bar[i] = compute_value()
    /// ```
    ///
    /// # Generated Operations
    ///
    /// ```ignore
    /// addr = emit_lvalue_address(foo.bar[i]) // Get address of lhs
    /// emit_expression(addr, compute_value()) // Materialize rhs into addr
    /// ```
    pub(crate) fn emit_assignment(
        &mut self,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output_destination = self.emit_lvalue_address(&lhs.0, ctx);
        self.emit_expression(&output_destination, rhs, ctx);
    }

    pub(crate) fn emit_variable_assignment(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
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
        // TODO: clean this up into a helper function with this if-else
        if target_register.ty.basic_type.is_scalar() {
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

            self.emit_initialize_target_memory_first_time(
                &memory_location,
                &variable.name,
                "initialize variable for the first time",
            );

            self.emit_expression_into_target_memory(
                &memory_location,
                expression,
                "variable assignment",
                ctx,
            );
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

    pub(crate) fn emit_compound_assignment(
        &mut self,
        target_location: &TargetAssignmentLocation,
        op: &CompoundOperatorKind,
        source: &Expression,
        ctx: &Context,
    ) {
        let assignment_target = self.emit_lvalue_address(&target_location.0, ctx);

        let hwm = self.temp_registers.save_mark();

        let resolved = self.emit_materialize_value_to_register(
            &assignment_target,
            &target_location.0.node,
            "compound_assignment",
        );

        let source_info = self.emit_scalar_rvalue(source, ctx);

        let type_to_consider = &source.ty;

        match &*type_to_consider.kind {
            TypeKind::Int => {
                self.emit_compound_assignment_i32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            TypeKind::Float => {
                self.emit_compound_assignment_f32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            TypeKind::String => todo!(),
            _ => panic!("not allowed as a compound assignment"),
        }

        if let Destination::Memory(mem) = assignment_target {
            self.emit_store_scalar_to_memory_offset_instruction(
                &mem,
                resolved.register(),
                &source.node,
                "emit compound",
            );
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    fn emit_compound_assignment_i32(
        &mut self,
        node: &Node,
        target: &TypedRegister,
        op: &CompoundOperatorKind,
        source_reg: &TypedRegister,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_u32(target, target, source_reg, node, "+=  (i32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_u32(target, target, source_reg, node, "-=  (i32)");
            }
            CompoundOperatorKind::Mul => {
                self.builder
                    .add_mul_i32(target, target, source_reg, node, "*=  (i32)");
            }
            CompoundOperatorKind::Div => {
                self.builder
                    .add_div_i32(target, target, source_reg, node, "/=  (i32)");
            }
            CompoundOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target, target, source_reg, node, "%=  (i32)");
            }
        }
    }

    fn emit_compound_assignment_f32(
        &mut self,
        node: &Node,
        target: &TypedRegister,
        op: &CompoundOperatorKind,
        source_ctx: &TypedRegister,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_f32(target, target, source_ctx, node, "+=  (f32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_f32(target, target, source_ctx, node, "-=  (f32)");
            }
            CompoundOperatorKind::Mul => {
                self.builder
                    .add_mul_f32(target, target, source_ctx, node, "*=  (f32)");
            }
            CompoundOperatorKind::Div => {
                self.builder
                    .add_div_f32(target, target, source_ctx, node, "/=  (f32)");
            }
            CompoundOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target, target, source_ctx, node, "%=  (f32)");
            }
        }
    }

    pub(crate) fn emit_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
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
        }

        // Now proceed with the normal variable assignment
        self.emit_variable_assignment(variable, expression, ctx);
    }
}
