//! Assignment helper functions for the code builder emitter.

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{
    CompoundOperatorKind, Expression, ExpressionKind, Literal, TargetAssignmentLocation,
    VariableRef,
};
use swamp_types::Type;
use swamp_vm_types::types::{BasicTypeKind, TypedRegister};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

impl CodeBuilder<'_> {
    pub(crate) fn emit_assignment(
        &mut self,
        lhs: &TargetAssignmentLocation,
        rhs: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output_destination = self.emit_lvalue_location(&lhs.0, ctx);

        self.emit_expression(&output_destination, rhs, ctx);
    }

    pub(crate) fn emit_assignment_conversion(
        &mut self,
        target_location: &MemoryLocation,
        rhs: &Expression,
        ctx: &Context,
    ) -> bool {
        match (&target_location.ty.basic_type.kind, &rhs.kind) {
            (
                BasicTypeKind::InternalVecStorage(element_type, capacity),
                ExpressionKind::Literal(Literal::Slice(_, elements)),
            ) => {
                self.emit_vec_storage_init(
                    &target_location.pointer_location().unwrap(),
                    elements,
                    element_type,
                    *capacity,
                    &target_location.ty.basic_type,
                    &rhs.node,
                    ctx,
                );
                true
            }

            (
                BasicTypeKind::InternalMapStorage(element_type, capacity),
                ExpressionKind::Literal(Literal::SlicePair(_, key_value_pairs_vec)),
            ) => {
                self.emit_map_storage_init_from_slice_pair_literal(
                    &target_location.pointer_location().unwrap(),
                    key_value_pairs_vec,
                    element_type,
                    *capacity,
                    &rhs.node,
                    ctx,
                );
                true
            }
            _ => false,
        }
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
                    "could not find id {} {}",
                    variable.unique_id_within_function, variable.assigned_name
                )
            })
            .clone();

        if variable.resolved_type.is_primitive() {
            self.emit_expression_into_register(
                &target_register,
                expression,
                "variable primitive",
                ctx,
            );
        } else {
            let memory_location = MemoryLocation {
                base_ptr_reg: target_register.clone(),
                offset: MemoryOffset(0),
                ty: target_register.ty,
            };
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
        let assignment_target = self.emit_lvalue_location(&target_location.0, ctx);

        let hwm = self.temp_registers.save_mark();

        let resolved = self.emit_load_primitive_from_detailed_location_if_needed(
            &assignment_target,
            &target_location.0.node,
            "compound_assignment",
        );

        let source_info = self.emit_scalar_rvalue(source, ctx);

        let type_to_consider = Self::referenced_or_not_type(&source.ty);

        match &type_to_consider {
            Type::Int => {
                self.emit_compound_assignment_i32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            Type::Float => {
                self.emit_compound_assignment_f32(
                    &source.node,
                    resolved.register(),
                    op,
                    &source_info,
                );
            }
            Type::String => todo!(),
            _ => panic!("not allowed as a compound assignment"),
        }

        /*
        TODO:
        self.emit_store_primitive_from_detailed_location_if_needed(
            &resolved,
            &assignment_target,
            &target_location.0.node,
        );

         */

        /*
        if let DetailedLocationResolved::TempRegister(temp_reg) = resolved {
            self.temp_registers.free(temp_reg);
        }

         */
        self.temp_registers.restore_to_mark(hwm);
    }

    fn emit_compound_assignment_i32(
        &mut self,
        node: &Node,
        target: &TypedRegister,
        op: &CompoundOperatorKind,
        source_ctx: &TypedRegister,
    ) {
        match op {
            CompoundOperatorKind::Add => {
                self.builder
                    .add_add_u32(target, target, source_ctx, node, "+=  (i32)");
            }
            CompoundOperatorKind::Sub => {
                self.builder
                    .add_sub_u32(target, target, source_ctx, node, "-=  (i32)");
            }
            CompoundOperatorKind::Mul => {
                self.builder
                    .add_mul_i32(target, target, source_ctx, node, "*=  (i32)");
            }
            CompoundOperatorKind::Div => {
                self.builder
                    .add_div_i32(target, target, source_ctx, node, "/=  (i32)");
            }
            CompoundOperatorKind::Modulo => {
                self.builder
                    .add_mod_i32(target, target, source_ctx, node, "%=  (i32)");
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
                    .add_mod_f32(target, target, source_ctx, node, "%=  (f32)");
            }
        }
    }

    pub(crate) fn emit_variable_definition(
        &mut self,
        variable: &VariableRef,
        expression: &Expression,
        ctx: &Context,
    ) {
        self.emit_variable_assignment(variable, expression, ctx);
    }
}
