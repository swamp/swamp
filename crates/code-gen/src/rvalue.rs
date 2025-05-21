use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{Expression, ExpressionKind};
use swamp_vm_types::types::{OutputDestination, TypedRegister, VmType};

impl CodeBuilder<'_> {
    pub fn emit_bool_expression(
        &mut self,
        target_reg: &OutputDestination,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(expr.ty.is_bool(), "must have scalar type");
        self.emit_expression(target_reg, expr, ctx);
    }

    pub fn emit_scalar_rvalue(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        if let ExpressionKind::VariableAccess(variable_ref) = &expr.kind {
            self.get_variable_register(variable_ref).clone()
        } else {
            let ty = layout_type(&expr.ty);
            let temp_target_reg = self.temp_registers.allocate(
                VmType::new_unknown_placement(ty),
                "to produce a scalar rvalue, we have to allocate a temporary variable",
            );

            self.emit_expression_into_register(
                temp_target_reg.register(),
                expr,
                "emit_scalar_rvalue",
                ctx,
            );

            temp_target_reg.register
        }
    }
}
