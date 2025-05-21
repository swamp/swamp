use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use source_map_node::Node;
use swamp_semantic::{Expression, ExpressionKind, Literal};
use swamp_types::Type;
use swamp_vm_types::MemoryOffset;
use swamp_vm_types::types::{OutputDestination, TypedRegister, VmType, int_type};
use tracing::info;

impl CodeBuilder<'_> {
    pub fn emit_bool_expression(
        &mut self,
        target_reg: &OutputDestination,
        expr: &Expression,
        ctx: &Context,
    ) {
        debug_assert!(expr.ty.is_bool(), "must have scalar type");
        self.emit_expression(target_reg, expr, ctx)
    }

    pub fn emit_literal(
        &mut self,
        output: &OutputDestination,
        basic_literal: &Literal,
        node: &Node,
        ctx: &Context,
    ) {
        match basic_literal {
            Literal::StringLiteral(str) => {
                self.emit_string_literal(output.grab_register(), node, str, ctx);
            }
            Literal::IntLiteral(int) => match output {
                OutputDestination::ScalarToRegister(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        *int as u32,
                        node,
                        "int literal",
                    );
                }
                OutputDestination::AggregateToMemoryLocation(location) => {
                    let temp_int_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for int literal",
                    );
                    self.builder.add_mov_32_immediate_value(
                        temp_int_literal_reg.register(),
                        *int as u32,
                        node,
                        "int literal",
                    );
                    self.builder.add_st32_using_ptr_with_offset(
                        location,
                        temp_int_literal_reg.register(),
                        node,
                        "copy int literal into destination memory",
                    );
                }
                OutputDestination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            Literal::FloatLiteral(fixed_point) => match output {
                OutputDestination::ScalarToRegister(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        fixed_point.inner() as u32,
                        node,
                        "float literal",
                    );
                }
                OutputDestination::AggregateToMemoryLocation(location) => {
                    let temp_fixed_point_temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for float literal",
                    );
                    self.builder.add_mov_32_immediate_value(
                        temp_fixed_point_temp_reg.register(),
                        fixed_point.inner() as u32,
                        node,
                        "float literal",
                    );
                    self.builder.add_st32_using_ptr_with_offset(
                        location,
                        temp_fixed_point_temp_reg.register(),
                        node,
                        "copy float literal into destination memory",
                    );
                }
                OutputDestination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            Literal::NoneLiteral => {
                todo!()
                /*
                self.builder
                    .add_mov8_immediate(output, 0, node, "none literal");

                 */
            }
            Literal::BoolLiteral(truthy) => match output {
                OutputDestination::ScalarToRegister(target_reg) => {
                    self.builder.add_mov8_immediate(
                        target_reg,
                        u8::from(*truthy),
                        node,
                        "bool literal",
                    );
                }
                OutputDestination::AggregateToMemoryLocation(location) => {
                    let temp_bool_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for bool literal",
                    );
                    self.builder.add_mov8_immediate(
                        temp_bool_literal_reg.register(),
                        u8::from(*truthy),
                        node,
                        "bool literal",
                    );

                    self.builder.add_st8_using_ptr_with_offset(
                        location,
                        temp_bool_literal_reg.register(),
                        node,
                        "copy bool literal into destination memory",
                    );
                }
                OutputDestination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },

            Literal::EnumVariantLiteral(enum_type, enum_variant, enum_variant_payload) => {
                // A enum variant literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_enum_variant_to_memory_location(
                    &output.grab_aggregate_memory_location(),
                    enum_type,
                    enum_variant,
                    enum_variant_payload,
                    node,
                    ctx,
                )
            }
            Literal::TupleLiteral(types, expressions) => {
                // A tuple literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_tuple_literal_into_memory(
                    &output.grab_aggregate_memory_location(),
                    types,
                    expressions,
                    ctx,
                    node,
                );
            }
            Literal::Slice(slice_type, expressions) => {
                // A tuple literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                let Type::DynamicSlice(element_type) = slice_type else {
                    panic!("must be slice")
                };
                let element_gen_type = layout_type(element_type);
                self.emit_slice_literal_into_target_lvalue_memory_location(
                    &output.grab_aggregate_memory_location(),
                    &element_gen_type,
                    expressions,
                    ctx,
                );
            }
            Literal::SlicePair(slice_pair_type, pairs) => {
                todo!() //self.emit_slice_pair_literal(slice_pair_type, pairs, node, ctx);
            }
        }
    }

    // It emits a value to a target register
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
            && self.rvalue_needs_memory_location_to_materialize_in(&expr)
        {
            info!(
                ?expr,
                "this needs a temporary location to be able to get a pointer"
            );
            let temp_materialization_target = self.allocate_frame_space_and_assign_register(
                output.ty(),
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
                self.emit_literal(output, basic_literal, node, ctx)
            }

            ExpressionKind::If(condition, true_expression, maybe_false_expression) => {
                self.emit_if(
                    &output,
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
                ) // todo:
            }
            ExpressionKind::BinaryOp(operator) => {
                self.emit_binary_operator(output.grab_register(), operator, ctx)
            }
            ExpressionKind::UnaryOp(operator) => {
                self.emit_unary_operator(output.grab_register(), operator, ctx)
            }
            ExpressionKind::PostfixChain(start, chain) => {
                self.emit_postfix_chain(output, start, chain, ctx)
            }
            ExpressionKind::Match(match_expr) => self.emit_match(output, match_expr, ctx),
            ExpressionKind::Guard(guards) => self.emit_guard(output, guards, ctx),
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(output, bindings, true_expr, false_expr.as_deref(), ctx)
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.emit_single_intrinsic_call(output, &expr.node, intrinsic_fn, arguments, ctx);
            }
            ExpressionKind::CoerceOptionToBool(a) => {
                self.emit_coerce_option_to_bool(output.grab_register(), a, ctx)
            }
            ExpressionKind::InternalCall(internal, arguments) => {
                self.emit_internal_call(output, &expr.node, internal, arguments, ctx)
            }
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(output, &expr.node, host_fn, arguments, ctx);
            }

            // Statements - can not return anything, so should assert that output is unit (nothing)
            ExpressionKind::TupleDestructuring(variables, tuple_types, tuple_expression) => {
                debug_assert!(output.is_unit());
                self.emit_tuple_destructuring(variables, tuple_types, tuple_expression, ctx)
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                debug_assert!(output.is_unit());
                self.emit_assignment(target_mut_location_expr, source_expr, "", ctx)
            }
            ExpressionKind::VariableDefinition(variable, expression) => {
                debug_assert!(output.is_unit());
                self.emit_variable_definition(variable, expression, ctx)
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                debug_assert!(output.is_unit());
                self.emit_variable_reassignment(variable, expression, ctx)
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                debug_assert!(output.is_unit());
                self.emit_compound_assignment(target_location, operator_kind, source_expr, ctx)
            }
            ExpressionKind::ForLoop(for_pattern, collection, lambda_expr) => {
                debug_assert!(output.is_unit());
                self.emit_for_loop(&expr.node, for_pattern, collection, lambda_expr, ctx)
            }
            ExpressionKind::WhileLoop(condition, expression) => {
                debug_assert!(output.is_unit());
                self.emit_while_loop(condition, expression, ctx)
            }

            // Low priority
            ExpressionKind::VariableBinding(_, _) => todo!(), // only used for `when` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
            _ => panic!("not an expression, probably a statement {:?} ", expr.kind),
        }

        self.temp_registers.restore_to_mark(hwm);
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
