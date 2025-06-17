/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::{Expression, ExpressionKind, Literal};
use swamp_vm_layout::LayoutCache;
use swamp_vm_types::types::{BasicTypeKind, Destination, TypedRegister};
use swamp_vm_types::{MemoryLocation, MemorySize};

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

        match &expr.kind {
            ExpressionKind::Literal(Literal::InitializerList(element_type, expressions)) => {
                self.emit_collection_init_from_initialization_list(
                    output,
                    expressions,
                    &expr.node,
                    ctx,
                );
                return;
            }
            ExpressionKind::Literal(Literal::InitializerPairList(element_type, expressions)) => {
                self.emit_container_init_from_initialization_pair_list(
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
                self.emit_constant_access(output, constant_ref, &expr.node, ctx);
            }
            ExpressionKind::VariableAccess(variable_ref) => {
                let variable_register = self.get_variable_register(variable_ref).clone();
                match output {
                    Destination::Register(target_reg) => {
                        self.builder.add_mov_reg(
                            target_reg,
                            &variable_register,
                            &expr.node,
                            "Extra copy variable access. Use `emit_scalar_rvalue` to avoid this extra copy",
                        );
                    }
                    Destination::Memory(location) => {
                        if variable_register.ty.is_scalar() {
                            match variable_register.ty.basic_type.kind {
                                BasicTypeKind::B8 | BasicTypeKind::U8 => {
                                    self.builder.add_st8_using_ptr_with_offset(
                                        location,
                                        &variable_register,
                                        node,
                                        &format!("var access to primitive memory location {location} <- {variable_register}"),
                                    );
                                }
                                BasicTypeKind::S32
                                | BasicTypeKind::U32
                                | BasicTypeKind::InternalStringPointer
                                | BasicTypeKind::Fixed32 => {
                                    self.builder.add_st32_using_ptr_with_offset(
                                        location,
                                        &variable_register,
                                        node,
                                        &format!("var access to primitive memory location {location} <- {variable_register}"),
                                    );
                                }
                                _ => panic!(
                                    "unknown scalar {}",
                                    variable_register.ty.basic_type.kind
                                ),
                            }
                        } else {
                            let source_memory_location =
                                MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                                    variable_register,
                                );
                            self.emit_block_copy_with_size_from_location(
                                location,
                                &source_memory_location,
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
                        "store binary op result directly to memory with offset",
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
            ExpressionKind::Literal(literal) => matches!(
                literal,
                Literal::EnumVariantLiteral(_, _, _)
                    | Literal::TupleLiteral(_, _)
                    | Literal::InitializerList(_, _)
                    | Literal::InitializerPairList(_, _)
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
                    let basic_type = layout_cache.layout(&expr.ty);
                    basic_type.is_aggregate()
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

    pub fn emit_block_copy_with_size_from_location(
        &mut self,
        destination_location: &MemoryLocation,
        source_location: &MemoryLocation,
        node: &Node,
        comment: &str,
    ) {
        let dest_pointer_location = self.emit_compute_effective_address_from_location_to_register(
            destination_location,
            node,
            "flatten destination location for block copy",
        );
        let source_pointer_location = self
            .emit_compute_effective_address_from_location_to_register(
                source_location,
                node,
                "flatten source location for block copy",
            );
        self.builder.add_block_copy_with_immediate_size(
            &dest_pointer_location,
            &source_pointer_location,
            source_location.ty.basic_type.total_size,
            node,
            comment,
        );
    }

    pub fn emit_block_copy_with_immediate_size(
        &mut self,
        destination_location: &MemoryLocation,
        source_location: &MemoryLocation,
        size: MemorySize,
        node: &Node,
        comment: &str,
    ) {
        let dest_pointer_location = self.emit_compute_effective_address_from_location_to_register(
            destination_location,
            node,
            "flatten destination location for block copy",
        );
        let source_pointer_location = self
            .emit_compute_effective_address_from_location_to_register(
                source_location,
                node,
                "flatten source location for block copy",
            );
        self.builder.add_block_copy_with_immediate_size(
            &dest_pointer_location,
            &source_pointer_location,
            size,
            node,
            comment,
        );
    }

    pub fn emit_block_copy_with_variable_size(
        &mut self,
        destination_location: &MemoryLocation,
        source_location: &MemoryLocation,
        size_reg: &TypedRegister,
        node: &Node,
        comment: &str,
    ) {
        let dest_pointer_location = self.emit_compute_effective_address_from_location_to_register(
            destination_location,
            node,
            "flatten destination location for block copy",
        );
        let source_pointer_location = self
            .emit_compute_effective_address_from_location_to_register(
                source_location,
                node,
                "flatten source location for block copy",
            );
        self.builder.add_block_copy_with_offset_with_variable_size(
            &dest_pointer_location,
            &source_pointer_location,
            size_reg,
            node,
            comment,
        );
    }
}
