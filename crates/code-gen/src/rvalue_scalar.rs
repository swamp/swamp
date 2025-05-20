use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{Expression, ExpressionKind};
use swamp_vm_types::types::{TypedRegister, VmType};

impl CodeBuilder<'_> {
    pub fn emit_bool_rvalue_to_specific_register(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(expr.ty.is_bool(), "must have scalar type");
        self.emit_scalar_rvalue_to_specific_register(target_reg, expr, ctx)
    }

    // It emits a value to a target register
    pub fn emit_scalar_rvalue_to_specific_register(
        &mut self,
        target_reg: &TypedRegister,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(expr.ty.is_scalar(), "must have scalar type");
        match &expr.kind {
            ExpressionKind::ConstantAccess(constant_ref) => {
                self.emit_constant_access(target_reg, &expr.node, constant_ref, ctx);
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                self.builder.add_mov_reg(
                    target_reg,
                    &variable_register,
                    &expr.node,
                    "extra copy var access",
                );
            }
            ExpressionKind::BorrowMutRef(expression) => {
                self.emit_borrow_mutable_reference(target_reg, &expr.node, expression, ctx)
            }
            ExpressionKind::BinaryOp(operator) => {
                self.emit_binary_operator(target_reg, operator, ctx)
            }
            ExpressionKind::UnaryOp(operator) => {
                self.emit_unary_operator(target_reg, operator, ctx)
            }
            ExpressionKind::PostfixChain(start, chain) => {
                self.emit_scalar_rvalue_postfix_chain(target_reg, start, chain, ctx)
            }
            ExpressionKind::Block(expressions) => {
                self.emit_block_to_scalar_rvalue_to_specific_register(target_reg, expressions, ctx)
            }
            ExpressionKind::Match(match_expr) => {
                self.emit_match_scalar_rvalue_to_specific_register(target_reg, match_expr, ctx)
            }
            ExpressionKind::Guard(guards) => self.emit_guard(target_reg, guards, ctx),
            ExpressionKind::If(conditional, true_expr, false_expr) => self.emit_if(
                target_reg,
                conditional,
                true_expr,
                false_expr.as_deref(),
                ctx,
            ),
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(target_reg, bindings, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.emit_single_intrinsic_call(
                    target_reg,
                    &expr.node,
                    intrinsic_fn,
                    arguments,
                    ctx,
                );
            }
            ExpressionKind::CoerceOptionToBool(a) => {
                self.emit_coerce_option_to_bool(target_reg, a, ctx)
            }
            ExpressionKind::InternalCall(internal, arguments) => self.emit_internal_call(
                Option::from(target_reg),
                &expr.node,
                internal,
                arguments,
                ctx,
            ),
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(&expr.node, host_fn, arguments, ctx);
            }
            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
            _ => panic!("not an expression, probably a statement {:?} ", expr.kind),
        }
    }

    pub fn emit_scalar_rvalue(&mut self, expr: &Expression, ctx: &Context) -> TypedRegister {
        match &expr.kind {
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                variable_register
            }
            _ => {
                let ty = layout_type(&expr.ty);
                let temp_target_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(ty),
                    "to produce a scalar rvalue, we have to allocate a temporary variable",
                );

                self.emit_scalar_rvalue_to_specific_register(temp_target_reg.register(), expr, ctx);

                temp_target_reg.register
            }
        }
    }
}
