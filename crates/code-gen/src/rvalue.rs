use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{Expression, ExpressionKind};
use swamp_vm_types::types::{Destination, TypedRegister, VmType};

impl CodeBuilder<'_> {
    pub fn emit_bool_expression(
        &mut self,
        target_reg: &Destination,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(expr.ty.is_bool(), "must have scalar type");
        self.emit_expression(target_reg, expr, ctx);
    }

    /// Emits code to evaluate an expression into a scalar rvalue.
    ///
    /// In compiler terminology:
    /// - "emit" means to generate the machine code
    /// - "scalar" refers to single-value types (numbers, booleans, pointers) as opposed to aggregates
    /// - "rvalue" is a value-producing expression that can appear on the right side of an assignment
    ///
    /// This method provides an optimized path for scalar values, avoiding the complexity
    /// of aggregate type handling in the general `emit_expression`. It's particularly
    /// important for efficient code generation of expressions that must produce values
    /// (rvalues) rather than storage locations (lvalues).
    ///
    /// # Direct Register Access
    ///
    /// The following cases can return a register without materialization:
    ///
    /// - Variable access (returns the existing register)
    /// - Constants (the memory location can be materialized into a register)
    /// - Some scalar literals (can be materialized directly into a register)
    ///
    /// # Register Allocation
    ///
    /// For other expressions that can't provide a direct register,
    /// a temporary register is allocated and the expression is evaluated into it.
    ///
    /// # Examples in Compiler Terms
    ///
    /// ```ignore
    /// // Binary operations need rvalues for both operands
    /// let left_reg = emit_scalar_rvalue(&binary_op.left);   // rvalue needed
    /// let right_reg = emit_scalar_rvalue(&binary_op.right); // rvalue needed
    ///
    /// // Assignment needs an lvalue on left, rvalue on right
    /// let target = emit_lvalue(&assign.left);        // lvalue needed
    /// let value = emit_scalar_rvalue(&assign.right); // rvalue needed
    /// ```
    pub fn emit_scalar_rvalue(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        match &expr.kind {
            ExpressionKind::VariableAccess(variable_ref) => {
                self.get_variable_register(variable_ref).clone()
            }
            ExpressionKind::ConstantAccess(constant_ref) => {
                // TODO: Implement direct register access for constants
                let ty = layout_type(&expr.ty);
                let temp_target_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(ty),
                    "temporary for constant access",
                );
                self.emit_constant_access(
                    temp_target_reg.register(),
                    &expr.node,
                    constant_ref,
                    ctx,
                );
                temp_target_reg.register
            }
            _ => {
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
}
