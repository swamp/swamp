use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use swamp_semantic::{Expression, ExpressionKind, Literal};
use swamp_vm_types::types::{BasicTypeKind, Destination, TypedRegister};
use swamp_vm_types::{MemoryLocation, MemoryOffset};

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
    pub fn emit_expression(&mut self, output: &Destination, expr: &Expression, ctx: &Context) {
        let node = &expr.node;

        if let Destination::Memory(memory) = &output {
            if self.try_container_init_from_slice_literal(memory, expr, ctx) {
                // If special container initialization was done, the materialization is complete.
                return;
            }
        }

        // If the expression needs a memory target, and the current output is not a memory target, create temp memory to materialize in
        // and return a pointer in the register instead and hopefully it works out.
        if !matches!(output, Destination::Memory(_))
            && Self::rvalue_needs_memory_location_to_materialize_in(expr)
        {
            //info!(
            //  ?expr,
            // "this needs a temporary location to be able to get a pointer"
            //);
            let temp_materialization_target = self
                .allocate_frame_space_and_return_destination_to_it(
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
            ExpressionKind::ConstantAccess(constant_ref) => match output {
                Destination::Register(reg) => {
                    self.emit_constant_access(reg, &expr.node, constant_ref, ctx);
                }
                Destination::Memory(mem_loc) => {
                    let temp_reg = self
                        .temp_registers
                        .allocate(mem_loc.ty.clone(), "constant_access_temp");
                    self.emit_constant_access(temp_reg.register(), &expr.node, constant_ref, ctx);

                    self.builder.add_st32_using_ptr_with_offset(
                        mem_loc,
                        temp_reg.register(),
                        node,
                        "store constant access result directly to memory with field offset",
                    );
                }
                Destination::Unit => {
                    panic!("a constant can not be unit")
                }
            },
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                match output {
                    Destination::Register(target_reg) => {
                        self.builder.add_mov_reg(
                            target_reg,
                            &variable_register,
                            &expr.node,
                            "extra copy var access",
                        );
                    }
                    Destination::Memory(location) => {
                        let memory_size = variable_register.ty.basic_type.total_size;

                        if variable_register.ty.can_be_contained_inside_register() {
                            match variable_register.ty.basic_type.kind {
                                BasicTypeKind::B8 | BasicTypeKind::U8 => {
                                    self.builder.add_st8_using_ptr_with_offset(
                                        location,
                                        &variable_register,
                                        node,
                                        "var access to primitive memory location",
                                    );
                                }
                                BasicTypeKind::S32
                                | BasicTypeKind::U32
                                | BasicTypeKind::Fixed32 => {
                                    self.builder.add_st32_using_ptr_with_offset(
                                        location,
                                        &variable_register,
                                        node,
                                        "var access to primitive memory location",
                                    );
                                }
                                _ => panic!("not sure"),
                            }
                        } else {
                            self.builder.add_block_copy_with_offset(
                                location,
                                &variable_register,
                                MemoryOffset(0),
                                memory_size,
                                node,
                                "copy var access block",
                            );
                        }
                    }
                    Destination::Unit => panic!("should not be possible"),
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
            ExpressionKind::BinaryOp(operator) => match output {
                Destination::Register(reg) => {
                    self.emit_binary_operator(reg, operator, ctx);
                }
                Destination::Memory(mem_loc) => {
                    let temp_reg = self
                        .temp_registers
                        .allocate(mem_loc.ty.clone(), "binary_op_temp");
                    self.emit_binary_operator(temp_reg.register(), operator, ctx);

                    self.builder.add_st32_using_ptr_with_offset(
                        mem_loc,
                        temp_reg.register(),
                        node,
                        "store binary op result directly to memory with field offset",
                    );
                }
                Destination::Unit => {
                    panic!("binary operator always returns a value")
                }
            },
            ExpressionKind::UnaryOp(operator) => match output {
                Destination::Register(reg) => {
                    self.emit_unary_operator(reg, operator, ctx);
                }
                Destination::Memory(mem_loc) => {
                    let temp_reg = self
                        .temp_registers
                        .allocate(mem_loc.ty.clone(), "unary_op_temp");
                    self.emit_unary_operator(temp_reg.register(), operator, ctx);

                    self.builder.add_st32_using_ptr_with_offset(
                        mem_loc,
                        temp_reg.register(),
                        node,
                        "store unary op result directly to memory with field offset",
                    );
                }
                Destination::Unit => {
                    panic!("unary operator always returns a value")
                }
            },
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
                self.emit_for_loop(
                    output,
                    &expr.node,
                    for_pattern,
                    collection,
                    lambda_expr,
                    ctx,
                );
            }
            ExpressionKind::WhileLoop(condition, expression) => {
                debug_assert!(output.is_unit());
                self.emit_while_loop(condition, expression, ctx);
            }

            // Low priority
            ExpressionKind::VariableBinding(variable, argument_expression) => {
                let variable_target_reg = self
                    .variable_registers
                    .get(&variable.unique_id_within_function)
                    .unwrap()
                    .clone();
                self.emit_argument_expression_binding(
                    &variable_target_reg,
                    argument_expression,
                    ctx,
                );
            } // only used for `with` expressions

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
            _ => panic!("unknown expression {:?}", expr.kind),
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn rvalue_needs_memory_location_to_materialize_in(expr: &Expression) -> bool {
        let specific_kind_of_expression_needs_memory_target = match &expr.kind {
            ExpressionKind::Literal(literal) => matches!(
                literal,
                Literal::EnumVariantLiteral(_, _, _)
                    | Literal::TupleLiteral(_, _)
                    | Literal::Slice(_, _)
                    | Literal::SlicePair(_, _)
            ),
            ExpressionKind::Option(_) | ExpressionKind::AnonymousStructLiteral(_) => true,
            _ => false,
        };

        if specific_kind_of_expression_needs_memory_target {
            true
        } else {
            // Easy to forget that you should also check if it's a function call with a return type requiring memory allocation
            match &expr.kind {
                ExpressionKind::InternalCall(_, _)
                | ExpressionKind::HostCall(_, _)
                | ExpressionKind::IntrinsicCallEx(_, _) => {
                    let basic_type = layout_type(&expr.ty);
                    basic_type.is_represented_as_a_pointer_in_reg()
                }
                _ => false,
            }
        }
    }

    pub(crate) fn emit_expression_into_target_memory(
        &mut self,
        memory_location: &MemoryLocation,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output = Destination::new_location(memory_location.clone());

        self.emit_expression(&output, expr, ctx);
    }

    pub(crate) fn emit_expression_into_register(
        &mut self,
        target_register: &TypedRegister,
        expr: &Expression,
        comment: &str,
        ctx: &Context,
    ) {
        let output = Destination::new_reg(target_register.clone());

        self.emit_expression(&output, expr, ctx);
    }
}
