/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use swamp_semantic::{BinaryOperatorKind, Expression, ExpressionKind};
use swamp_types::TypeKind;
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::MemoryLocation;
use swamp_vm_types::types::{Destination, TypedRegister, VmType, int_type};

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

        if self.options.should_show_debug {
            self.debug_expression(expr, "emitting expression");
        }

        // Must check for initializer first, since they do not require temporary memory
        // or similar, should be handled first and then return.
        match &expr.kind {
            ExpressionKind::InitializerList(_element_type, expressions) => {
                self.emit_vec_like_collection_init_from_initialization_list(
                    output,
                    expressions,
                    &expr.node,
                    ctx,
                );
                return;
            }
            ExpressionKind::InitializerPairList(_element_type, expressions) => {
                self.emit_map_like_init_from_initialization_pair_list(
                    output,
                    expressions,
                    &expr.node,
                    ctx,
                );
                return;
            }
            _ => {}
        }

        // If the expression needs a memory target, and the current output is not a memory target, create temp memory to materialize in
        // and return a pointer in the register instead and hopefully it works out.
        if !matches!(output, Destination::Memory(_))
            && Self::rvalue_needs_memory_location_to_materialize_in(
                &mut self.state.layout_cache,
                expr,
            )
        {
            let expr_basic_type = self.state.layout_cache.layout(&expr.ty);
            let temp_materialization_target = self
                .allocate_frame_space_and_return_destination_to_it(
                    &expr_basic_type,
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

        match &expr.kind {
            &ExpressionKind::InitializerList(_, _) | &ExpressionKind::InitializerPairList(_, _) => {
                panic!("handled earlier")
            }
            ExpressionKind::Error(_) => {
                return;
            }

            ExpressionKind::StringLiteral(str) => {
                self.emit_string_literal(output, node, str, ctx);
            }
            ExpressionKind::IntLiteral(int) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        *int as u32,
                        node,
                        "int literal",
                    );
                }
                Destination::Memory(location) => {
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
                        &format!("copy int literal into destination memory {location} <- {temp_int_literal_reg}"),
                    );
                }
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            ExpressionKind::ByteLiteral(byte) => match output {
                Destination::Register(target_reg) => {
                    self.builder
                        .add_mov8_immediate(target_reg, *byte, node, "int literal");
                }
                Destination::Memory(location) => {
                    let temp_byte_literal_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(int_type()),
                        "temporary for byte literal",
                    );
                    self.builder.add_mov8_immediate(
                        temp_byte_literal_reg.register(),
                        *byte,
                        node,
                        "byte literal",
                    );
                    self.builder.add_st8_using_ptr_with_offset(
                        location,
                        temp_byte_literal_reg.register(),
                        node,
                        &format!("copy byte literal into destination memory {location} <- {temp_byte_literal_reg}"),
                    );
                }
                Destination::Unit => {
                    panic!("byte can not materialize into nothing")
                }
            },
            ExpressionKind::FloatLiteral(fixed_point) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov_32_immediate_value(
                        target_reg,
                        fixed_point.inner() as u32,
                        node,
                        "float literal",
                    );
                }
                Destination::Memory(location) => {
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
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },
            ExpressionKind::NoneLiteral => {
                //let union_info = output.ty().unwrap_info().unwrap();

                match output {
                    Destination::Register(target_reg) => {
                        self.builder
                            .add_mov8_immediate(target_reg, 0, node, "none literal");
                    }
                    Destination::Memory(location) => {
                        let temp_none_literal_reg = self.temp_registers.allocate(
                            VmType::new_contained_in_register(int_type()),
                            "temporary for none literal",
                        );
                        self.builder.add_mov8_immediate(
                            temp_none_literal_reg.register(),
                            0,
                            node,
                            "none literal",
                        );

                        self.builder.add_st8_using_ptr_with_offset(
                            location,
                            temp_none_literal_reg.register(),
                            node,
                            "copy none literal into destination memory",
                        );
                    }
                    Destination::Unit => {
                        panic!("none can not materialize into nothing")
                    }
                }
            }
            ExpressionKind::BoolLiteral(truthy) => match output {
                Destination::Register(target_reg) => {
                    self.builder.add_mov8_immediate(
                        target_reg,
                        u8::from(*truthy),
                        node,
                        "bool literal",
                    );
                }
                Destination::Memory(location) => {
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
                Destination::Unit => {
                    panic!("int can not materialize into nothing")
                }
            },

            ExpressionKind::EnumVariantLiteral(enum_variant, expressions) => {
                // A enum variant literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_enum_variant_to_memory_location(
                    &output.grab_aggregate_memory_location(),
                    &expr.ty,
                    enum_variant,
                    expressions,
                    node,
                    ctx,
                );
            }
            ExpressionKind::TupleLiteral(expressions) => {
                // A tuple literal can not be represented as a register, not even a pointer to it, it needs materialization into memory
                self.emit_tuple_literal_into_memory(
                    &output.grab_aggregate_memory_location(),
                    &expr.ty,
                    expressions,
                    ctx,
                    node,
                );
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
            ExpressionKind::NamedStructLiteral(inner) => {
                self.emit_expression(output, inner, ctx);
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
                    output,
                    node,
                    maybe_option.as_deref(),
                    ctx,
                ),
            ExpressionKind::ConstantAccess(constant_ref) => {
                self.emit_constant_access(output, constant_ref, &expr.node, ctx);
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                let source_destination = Destination::Register(variable_register);
                self.emit_copy_value_between_destinations(
                    output,
                    &source_destination,
                    node,
                    &format!(
                        "copy variable '{}' to destination",
                        variable_ref.assigned_name
                    ),
                );
            }
            ExpressionKind::BorrowMutRef(expression) => {
                self.emit_borrow_mutable_reference(
                    output.grab_register(),
                    &expr.node,
                    expression,
                    ctx,
                ); // todo:
            }
            ExpressionKind::BinaryOp(operator) => match operator.kind {
                BinaryOperatorKind::NoneCoalesce => self.emit_none_coalesce_operator(
                    output,
                    &operator.left,
                    &operator.right,
                    &operator.node,
                    ctx,
                ),
                _ => match output {
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
                            "store binary op result directly to memory with offset",
                        );
                    }
                    Destination::Unit => {
                        panic!("binary operator always returns a value")
                    }
                },
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
                        "store unary op result directly to memory with offset",
                    );
                }
                Destination::Unit => {
                    panic!("unary operator always returns a value")
                }
            },
            ExpressionKind::PostfixChain(start, chain) => {
                self.emit_postfix_chain(output, start, chain, ctx);
            }
            ExpressionKind::Match(match_expr) => match &*match_expr.expression.ty.kind {
                TypeKind::Enum(_enum_type) => {
                    self.emit_match_enum(output, match_expr, ctx);
                }
                _ => {
                    self.emit_match_literal(output, match_expr, ctx);
                }
            },
            ExpressionKind::Guard(guards) => self.emit_guard(output, guards, ctx),
            ExpressionKind::When(bindings, true_expr, false_expr) => {
                self.emit_when(output, bindings, true_expr, false_expr.as_deref(), ctx);
            }
            ExpressionKind::IntrinsicCallEx(intrinsic_fn, arguments) => {
                self.emit_single_intrinsic_call(output, &expr.node, intrinsic_fn, arguments, ctx);
            }
            ExpressionKind::CoerceToAny(a) => {
                self.emit_coerce_to_any(output, a, ctx);
            }
            ExpressionKind::CoerceOptionToBool(a) => {
                self.emit_coerce_option_to_bool(output.grab_register(), a, ctx);
            }
            ExpressionKind::CoerceIntToChar(a) => {
                self.emit_coerce_int_to_char(output.grab_register(), a, ctx);
            }
            ExpressionKind::CoerceIntToByte(a) => {
                self.emit_coerce_int_to_byte(output, a, ctx);
            }
            ExpressionKind::InternalCall(internal, arguments) => {
                self.emit_internal_call(output, &expr.node, internal, arguments, ctx);
            }
            ExpressionKind::HostCall(host_fn, arguments) => {
                self.emit_host_call(output, &expr.node, host_fn, arguments, ctx);
            }

            // Statements - can not return anything, so should assert that output is unit (nothing)
            ExpressionKind::TupleDestructuring(variables, tuple_type, tuple_expression) => {
                debug_assert!(output.is_unit());
                self.emit_tuple_destructuring(variables, tuple_type, tuple_expression, ctx);
            }
            ExpressionKind::Assignment(target_mut_location_expr, source_expr) => {
                debug_assert!(output.is_unit());
                self.emit_assignment(target_mut_location_expr, source_expr, "", ctx);
            }
            ExpressionKind::VariableDefinition(variable, expression) => {
                debug_assert!(output.is_unit());
                self.emit_variable_definition(variable, expression, ctx);
            }
            ExpressionKind::VariableDefinitionLValue(variable, location_expr) => {
                // For lvalue definitions, the variable should be initialized with the memory address
                // This is used in `with` statements, e.g. `mut var = &some.field`
                match output {
                    Destination::Unit => {
                        let lvalue_destination = self.emit_lvalue_address(location_expr, ctx);

                        let alias_register = self.get_variable_register(variable).clone();

                        self.emit_compute_effective_address_to_target_register(
                            &alias_register,
                            &lvalue_destination,
                            &variable.name,
                            "initialize alias variable with lvalue address",
                        );
                    }
                    _ => {
                        panic!("VariableDefinitionLValue should only be used with Unit destination")
                    }
                }
            }
            ExpressionKind::VariableReassignment(variable, expression) => {
                assert!(output.is_unit());
                self.emit_variable_reassignment(variable, expression, ctx);
            }
            ExpressionKind::CompoundAssignment(target_location, operator_kind, source_expr) => {
                assert!(output.is_unit());
                self.emit_compound_assignment(target_location, operator_kind, source_expr, ctx);
            }
            ExpressionKind::ForLoop(for_pattern, collection, lambda_expr) => {
                if !output.is_unit() {
                    let x = 3;
                    eprintln!("problem");
                }
                assert!(output.is_unit());
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
                assert!(output.is_unit());
                self.emit_while_loop(condition, expression, ctx);
            }

            // Illegal
            ExpressionKind::Lambda(_vec, _x) => {
                panic!("something went wrong. non-capturing lambdas can not be evaluated")
            }
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    pub(crate) fn rvalue_needs_memory_location_to_materialize_in(
        layout_cache: &mut LayoutCache,
        expr: &Expression,
    ) -> bool {
        let specific_kind_of_expression_needs_memory_target = match &expr.kind {
            // TODO: Should have more robust check here. maybe check primitives instead and invert?
            ExpressionKind::EnumVariantLiteral(_, _)
            | ExpressionKind::TupleLiteral(_)
            | ExpressionKind::InitializerList(_, _)
            | ExpressionKind::InitializerPairList(_, _) => true,
            ExpressionKind::Option(_)
            | ExpressionKind::AnonymousStructLiteral(_)
            | ExpressionKind::CoerceToAny(_) => true,
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
                    let basic_type = layout_cache.layout(&expr.ty);
                    !basic_type.is_reg_copy()
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
        // Only initialize if it's a direct collection type (not inside a tuple/struct)
        if matches!(
            expr.kind,
            ExpressionKind::InitializerList(_, _) | ExpressionKind::InitializerPairList(_, _)
        ) {
            self.emit_initialize_memory_for_any_type(
                memory_location,
                &expr.node,
                &format!("{comment} - initialize memory for collection"),
            );
        }

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
