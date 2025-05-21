use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{Expression, ExpressionKind, Literal};
use swamp_vm_types::types::{OutputDestination, TypedRegister};
use swamp_vm_types::{MemoryLocation, MemoryOffset};
use tracing::info;

impl CodeBuilder<'_> {
    /// The expression materializer! Transforms high-level expressions into their code representation,
    /// making sure each value finds its proper home in either a register or memory location.
    ///
    /// # Optimization Magic
    ///
    /// Uses Destination-Passing Style (DPS) to optimize code generation. Instead of creating
    /// temporary values and copying them around, we tell each expression exactly where its
    /// result should end up. This means we can often construct values directly in their
    /// final location, avoiding unnecessary copies and temporary allocations.
    ///
    /// # Arguments
    ///
    /// * `output` - Where should our expression's result live? (register or memory location)
    /// * `expr` - The expression we're bringing to life
    /// * `ctx` - The context with all our compilation knowledge
    ///
    /// If something needs temporary storage, we'll handle that too.
    #[allow(clippy::too_many_lines)]
    pub fn emit_expression(
        &mut self,
        output: &OutputDestination,
        expr: &Expression,
        ctx: &Context,
    ) {
        let node = &expr.node;

        // If the expression needs a memory target, and the current output is not a memory target, create temp memory to materialize in
        // and return a pointer in the register instead and hopefully it works out.
        if !matches!(output, OutputDestination::AggregateToMemoryLocation(_))
            && Self::rvalue_needs_memory_location_to_materialize_in(expr)
        {
            info!(
                ?expr,
                "this needs a temporary location to be able to get a pointer"
            );
            let temp_materialization_target = self.allocate_frame_space_and_assign_register(
                output.ty(),
                &expr.node,
                "rvalue temporary materialization",
            );

            self.emit_expression(&temp_materialization_target, expr, ctx);

            self.builder.add_mov_reg(
                output.grab_register(),
                &temp_materialization_target
                    .grab_memory_location()
                    .base_ptr_reg,
                node,
                "copy temp materialization memory pointer reg",
            );

            return;
        }

        let hwm = self.temp_registers.save_mark();

        //info!(?output, ?expr.kind, "emit_expression");
        //debug_assert!(expr.ty.is_scalar(), "must have scalar type {}", expr.ty);

        match &expr.kind {
            ExpressionKind::Literal(basic_literal) => {
                self.emit_literal(output, basic_literal, node, ctx);
            }
            ExpressionKind::If(condition, true_expression, maybe_false_expression) => {
                self.emit_if(
                    output,
                    condition,
                    true_expression,
                    maybe_false_expression.as_deref(),
                    ctx,
                );
            }
            ExpressionKind::Block(expressions) => {
                self.emit_block(output, expressions, ctx);
            }
            ExpressionKind::AnonymousStructLiteral(anon_struct) => {
                // Literals can not have pointers to them, they need to materialize into a memory location
                self.emit_anonymous_struct_literal_into_memory_location(
                    &output.grab_aggregate_memory_location(),
                    anon_struct,
                    &expr.ty,
                    node,
                    "struct literal",
                    ctx,
                );
            }
            ExpressionKind::Option(maybe_option) => self
                .emit_option_expression_into_target_memory_location(
                    &output.grab_aggregate_memory_location(),
                    node,
                    maybe_option.as_deref(),
                    ctx,
                ),
            ExpressionKind::ConstantAccess(constant_ref) => {
                self.emit_constant_access(output.grab_register(), &expr.node, constant_ref, ctx);
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                match output {
                    OutputDestination::ScalarToRegister(target_reg) => {
                        self.builder.add_mov_reg(
                            target_reg,
                            &variable_register,
                            &expr.node,
                            "extra copy var access",
                        );
                    }
                    OutputDestination::AggregateToMemoryLocation(location) => {
                        let memory_size = variable_register.ty.basic_type.total_size;

                        self.builder.add_block_copy_with_offset(
                            location,
                            &variable_register,
                            MemoryOffset(0),
                            memory_size,
                            node,
                            "copy var access block",
                        );
                    }
                    OutputDestination::Unit => panic!("should not be possible"),
                }
            }
            ExpressionKind::BorrowMutRef(expression) => {
                self.emit_borrow_mutable_reference(
                    output.grab_register(),
                    &expr.node,
                    expression,
                    ctx,
                ); // todo:
            }
            ExpressionKind::BinaryOp(operator) => {
                self.emit_binary_operator(output.grab_register(), operator, ctx);
            }
            ExpressionKind::UnaryOp(operator) => {
                self.emit_unary_operator(output.grab_register(), operator, ctx);
            }
            ExpressionKind::PostfixChain(start, chain) => {
                self.emit_postfix_chain(output, start, chain, ctx);
            }
            ExpressionKind::Match(match_expr) => self.emit_match(output, match_expr, ctx),
            ExpressionKind::Guard(guards) => self.emit_guard(output, guards, ctx),
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(output, bindings, true_expr, false_expr.as_deref(), ctx);
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.emit_single_intrinsic_call(output, &expr.node, intrinsic_fn, arguments, ctx);
            }
            ExpressionKind::CoerceOptionToBool(a) => {
                self.emit_coerce_option_to_bool(output.grab_register(), a, ctx);
            }
            ExpressionKind::InternalCall(internal, arguments) => {
                self.emit_internal_call(output, &expr.node, internal, arguments, ctx);
            }
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(output, &expr.node, host_fn, arguments, ctx);
            }

            // Statements - can not return anything, so should assert that output is unit (nothing)
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                debug_assert!(output.is_unit());
                self.emit_tuple_destructuring(variables, tuple_types, tuple_expression, ctx);
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                debug_assert!(output.is_unit());
                self.emit_assignment(target_mut_location_expr, source_expr, "", ctx);
            }
            ExpressionKind::VariableDefinition(variable, expression) => {
                debug_assert!(output.is_unit());
                self.emit_variable_definition(variable, expression, ctx);
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                debug_assert!(output.is_unit());
                self.emit_variable_reassignment(variable, expression, ctx);
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                debug_assert!(output.is_unit());
                self.emit_compound_assignment(target_location, operator_kind, source_expr, ctx);
            }
            ExpressionKind::ForLoop(for_pattern, collection, lambda_expr) => {
                debug_assert!(output.is_unit());
                self.emit_for_loop(&expr.node, for_pattern, collection, lambda_expr, ctx);
            }
            ExpressionKind::WhileLoop(condition, expression) => {
                debug_assert!(output.is_unit());
                self.emit_while_loop(condition, expression, ctx);
            }

            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) const fn rvalue_needs_memory_location_to_materialize_in(expr: &Expression) -> bool {
        match &expr.kind {
            ExpressionKind::Literal(literal) => matches!(
                literal,
                Literal::EnumVariantLiteral(_, _, _)
                    | Literal::TupleLiteral(_, _)
                    | Literal::Slice(_, _)
                    | Literal::SlicePair(_, _)
            ),
            ExpressionKind::Option(_) | ExpressionKind::AnonymousStructLiteral(_) => true,
            _ => false,
        }
    }

    pub(crate) fn emit_expression_into_target_memory(
        &mut self,
        memory_location: &MemoryLocation,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output = OutputDestination::new_location(memory_location.clone());

        self.emit_expression(&output, expr, ctx);
    }

    pub(crate) fn emit_expression_into_register(
        &mut self,
        target_register: &TypedRegister,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output = OutputDestination::new_reg(target_register.clone());

        self.emit_expression(&output, expr, ctx);
    }
}
