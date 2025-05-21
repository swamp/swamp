//! Assignment helper functions for the code builder emitter.
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, ExpressionKind, Literal, TargetAssignmentLocation};
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::BasicTypeKind;

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
}
