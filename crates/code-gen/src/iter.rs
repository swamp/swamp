/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{Collection, FlagStateKind, Transformer, TransformerResult};
use source_map_node::Node;
use swamp_semantic::{Expression, VariableRef};
use swamp_vm_types::types::{
    BasicType, BasicTypeKind, Destination, TypedRegister, VmType, u8_type, u32_type,
};
use swamp_vm_types::{InstructionPosition, MemoryLocation, MemoryOffset, PatchPosition};
use tracing::error;

impl CodeBuilder<'_> {
    /// Generates code to iterate over a collection using a transformer (e.g., map, filter, `filter_map`)
    /// and a lambda expression. Handles creation of result vectors, iterator setup, lambda invocation,
    /// early exit logic, and result collection.
    ///
    /// Steps:
    /// 1. (Optional) Initialize a target vector for the result, if the transformer produces one.
    /// 2. Initialize the iterator for the collection.
    /// 3. Generate code to fetch the next element from the iterator.
    /// 4. Inline the lambda code for the current element.
    /// 5. If the transformer supports early exit (e.g., filter, find), set the P flag based on the lambda result.
    /// 6. Conditionally skip result insertion if early exit is triggered.
    /// 7. (Optional) If applicable, insert the (possibly unwrapped) result into the target vector.
    /// 8. Loop back to fetch the next element.
    /// 9. Finalize iteration, handling any post-processing (e.g., normalizing boolean results).
    ///
    /// # Parameters
    /// - `node`: The AST node for error reporting and code location.
    /// - `collection_type`: The type of collection being iterated.
    /// - `transformer`: The transformer operation (map, filter, find, fold, etc.).
    /// - `collection_self_region`: Memory region of the collection.
    /// - `lambda_expression`: The lambda expression to apply.
    /// - `ctx`: Code generation context. Contains the result target.
    ///
    /// # Returns
    /// - `Ok(())` on success, or an error if code generation fails.
    ///
    /// # Errors
    /// // TODO:
    /// # Panics
    /// - If the lambda expression or its kind is not as expected (internal error).
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn emit_iterate_over_collection_with_lambda(
        &mut self,
        target_destination: &Destination,
        node: &Node,
        source_collection_type: Collection,
        transformer: Transformer,
        source_collection_reg: &TypedRegister,
        lambda_tuple: (Vec<VariableRef>, &Expression),
        ctx: &Context,
    ) {
        let (lambda_variables, lambda_expr) = lambda_tuple;
        let maybe_primary_element_gen_type = source_collection_reg
            .ty
            .basic_type
            .underlying()
            .element()
            .cloned();

        let target_variables: Vec<_> = lambda_variables
            .iter()
            .map(|x| {
                self.variable_registers
                    .get(&x.unique_id_within_function)
                    .unwrap()
                    .clone()
            })
            .collect();

        // Primary is the right most variable
        let primary_variable = &target_variables[target_variables.len() - 1];

        let hwm = self.temp_registers.save_mark();

        // 2. Initialize the iterator and generate code to fetch the next element.
        let (continue_iteration_label, iteration_complete_patch_position) = self
            .emit_iter_init_and_next(
                node,
                source_collection_type,
                maybe_primary_element_gen_type,
                source_collection_reg,
                &target_variables,
            );

        // 3. Inline the lambda code for the current element(s).
        let lambda_result = self.emit_scalar_rvalue(lambda_expr, ctx);

        // 4. If the transformer supports early exit, set the P flag based on the lambda result.
        let transformer_t_flag_state =
            self.check_if_transformer_sets_t_flag(transformer, &lambda_result, node);

        // 5. Conditionally skip result insertion if early exit is triggered.
        let maybe_skip_early = if self.check_if_transformer_can_skip_early(transformer)
            && matches!(
                transformer_t_flag_state,
                FlagStateKind::TFlagIsTrueWhenSet | FlagStateKind::TFlagIsTrueWhenClear
            ) {
            // The P flag is set so we can act on it
            let skip_early = self.builder.add_jmp_if_not_equal_polarity_placeholder(
                &transformer_t_flag_state.polarity(),
                node,
                "skip early",
            );

            Some(skip_early)
        } else {
            // P flag is not set, we have to iterate through the whole collection
            None
        };

        // 6. If applicable, insert the (possibly unwrapped) result into the target vector.
        match transformer.return_type() {
            TransformerResult::Unit => {
                // Only alternative is that it is a bool return, so no need to take any action here
            }
            TransformerResult::Bool => {
                // Only alternative is that it is a bool return, so no need to take any action here
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                // Handled elsewhere
            }
            TransformerResult::VecWithLambdaResult => {
                self.transformer_add_to_collection(
                    &lambda_result,
                    transformer.needs_tag_removed(),
                    source_collection_type,
                    target_destination.register().unwrap(),
                    node,
                );
            }
            TransformerResult::VecFromSourceCollection => {
                let skip_if_false_patch_position = self
                    .builder
                    .add_jmp_if_not_true_placeholder(node, "skip the result if it is false");

                let absolute_pointer = self.emit_compute_effective_address_to_register(
                    target_destination,
                    node,
                    "get pointer to collection",
                );
                self.add_to_collection(
                    node,
                    source_collection_type,
                    &absolute_pointer,
                    primary_variable,
                );

                self.builder.patch_jump_here(skip_if_false_patch_position);
            }
        }

        // 7. Loop back to fetch the next element.
        self.builder.add_jmp(
            continue_iteration_label,
            &lambda_expr.debug_last_expression().node,
            "jump to iter_next",
        );

        self.builder
            .patch_jump_here(iteration_complete_patch_position);

        // 8. Finalize iteration, handling any post-processing (e.g., normalizing boolean results).
        if let Some(found_skip_early) = maybe_skip_early {
            self.builder.patch_jump_here(found_skip_early);
        }

        match transformer.return_type() {
            TransformerResult::Bool => {
                // It is a transformer that returns a bool, lets store P flag as bool it
                self.builder.add_stz(
                    target_destination.register().unwrap(),
                    node,
                    "transformer sets standard bool",
                );
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                let destination_type = target_destination.ty();
                let BasicTypeKind::Optional(tagged_union) = &destination_type.kind else {
                    panic!("expected optional {destination_type:?}");
                };

                let tag_some_value_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u8_type()),
                    "iterate over collection target",
                );

                self.builder.add_mov8_immediate(
                    tag_some_value_reg.register(),
                    1,
                    node,
                    "mark tag as Some",
                );

                let result_location = target_destination.grab_memory_location();

                self.builder.add_st8_using_ptr_with_offset(
                    &result_location.unsafe_add_offset(tagged_union.tag_offset),
                    tag_some_value_reg.register(),
                    node,
                    "store Tag value of (1) into memory",
                );

                // TODO: let payload_memory_location = result_location.unsafe_add_offset(tagged_union.payload_offset);
                let payload_memory_location = result_location.unsafe_add_offset(MemoryOffset(4));
                let payload_destination = Destination::Memory(payload_memory_location);

                let source_location = Destination::Register(primary_variable.clone());

                self.emit_store_value_to_memory_destination(
                    &payload_destination,
                    &source_location,
                    node,
                    "copy result in place",
                );
            }
            _ => {}
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    #[allow(clippy::unnecessary_wraps)]
    #[allow(clippy::too_many_lines)]
    fn emit_iter_init_and_next(
        &mut self,
        node: &Node,
        collection_type: Collection,
        maybe_element_type: Option<BasicType>,
        collection_self_addr: &TypedRegister,
        target_variables: &[TypedRegister],
    ) -> (InstructionPosition, PatchPosition) {
        let iterator_gen_type = collection_type.iterator_gen_type();

        let target_iterator_header_reg = self.allocate_frame_space_and_return_absolute_pointer_reg(
            &iterator_gen_type,
            node,
            "allocate iterator header space",
        );

        let primary_register = if target_variables.len() == 2 {
            &target_variables[1]
        } else {
            &target_variables[0]
        };

        let iter_next_position = InstructionPosition(self.builder.position().0 + 1);
        let placeholder = match collection_type {
            Collection::Range => {
                self.builder.add_range_iter_init(
                    &target_iterator_header_reg,
                    collection_self_addr,
                    node,
                    "range init",
                );

                assert_eq!(target_variables.len(), 1);
                self.builder.add_range_iter_next_placeholder(
                    &target_iterator_header_reg,
                    &target_variables[0],
                    node,
                    "range iter next single",
                )
            }

            // Low  prio
            Collection::String => todo!(),
            _ => {
                let (temp_registers, target_variables_to_use) =
                    if primary_register.ty.basic_type.is_scalar() {
                        // For primitives, create temp register to hold the address, since they do not want
                        // the address
                        let temp_addr = self
                            .temp_registers
                            .allocate(primary_register.ty.clone(), "temp address for value");

                        if target_variables.len() == 2 {
                            (
                                Some(temp_addr.register().clone()),
                                vec![target_variables[0].clone(), temp_addr.register.clone()],
                            )
                        } else {
                            (
                                Some(temp_addr.register().clone()),
                                vec![temp_addr.register.clone()],
                            )
                        }
                    } else {
                        (None, target_variables.to_vec())
                    };

                let patch_position = self.emit_iter_init_and_next_to_memory(
                    &target_iterator_header_reg,
                    node,
                    collection_type,
                    maybe_element_type,
                    collection_self_addr,
                    &target_variables_to_use,
                );

                if let Some(temp_regs) = temp_registers {
                    let source_memory_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(temp_regs);
                    self.emit_load_value_from_memory_source(
                        primary_register,
                        &source_memory_location,
                        node,
                        "load primitive from element address",
                    );
                }

                patch_position
            }
        };

        (iter_next_position, placeholder)
    }

    fn emit_iter_init_and_next_to_memory(
        &mut self,
        target_iterator_header_reg: &TypedRegister,
        node: &Node,
        collection_type: Collection,
        maybe_element_type: Option<BasicType>,
        collection_self_addr: &TypedRegister,
        target_variables: &[TypedRegister],
    ) -> PatchPosition {
        let primary_register = if target_variables.len() == 2 {
            &target_variables[1]
        } else {
            &target_variables[0]
        };
        let is_pair = target_variables.len() == 2;
        match collection_type {
            Collection::Vec => {
                if maybe_element_type.is_none() {
                    error!(
                        ?collection_self_addr,
                        "can no start iterating with this strange vec collection"
                    );
                }

                self.builder.add_vec_iter_init(
                    target_iterator_header_reg,
                    collection_self_addr,
                    maybe_element_type.unwrap().total_size,
                    node,
                    "vec iter init",
                );

                let is_pair = target_variables.len() == 2;

                if is_pair {
                    // For non-primitives, use target directly
                    self.builder.add_vec_iter_next_pair_placeholder(
                        target_iterator_header_reg,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "vec iter next pair",
                    )
                } else {
                    // For non-primitives, use target directly
                    self.builder.add_vec_iter_next_placeholder(
                        target_iterator_header_reg,
                        primary_register,
                        node,
                        "vec iter next single",
                    )
                }
            }
            Collection::Sparse => {
                self.builder.add_sparse_iter_init(
                    target_iterator_header_reg,
                    collection_self_addr,
                    node,
                    "sparse init",
                );

                if is_pair {
                    self.builder.add_sparse_iter_next_pair_placeholder(
                        target_iterator_header_reg,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "sparse next_pair",
                    )
                } else {
                    self.builder.add_sparse_iter_next_placeholder(
                        target_iterator_header_reg,
                        primary_register,
                        node,
                        "sparse next_single",
                    )
                }
            }

            Collection::Map => {
                self.builder.add_map_iter_init(
                    target_iterator_header_reg,
                    collection_self_addr,
                    node,
                    "map init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_map_iter_next_pair_placeholder(
                        target_iterator_header_reg,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "map next_pair",
                    )
                } else {
                    self.builder.add_map_iter_next_placeholder(
                        target_iterator_header_reg,
                        &target_variables[0],
                        node,
                        "map next_single",
                    )
                }
            }
            Collection::Grid => todo!(),
            _ => panic!("unknown collection"),
        }
    }

    fn check_if_transformer_sets_t_flag(
        &mut self,
        transformer: Transformer,
        in_value: &TypedRegister,
        node: &Node,
    ) -> FlagStateKind {
        match transformer {
            Transformer::For => FlagStateKind::TFlagIsIndeterminate,
            Transformer::Filter => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_reg(in_value, node, "filter bool to P flag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::Find => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_reg(in_value, node, "find: bool to P flag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::Map => FlagStateKind::TFlagIsIndeterminate,
            Transformer::Any => {
                self.builder.add_tst_reg(in_value, node, "any, check tag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::All => {
                self.builder.add_tst_reg(in_value, node, "all, check tag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst_reg(in_value, node, "filter map, check tag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
        }
    }

    fn add_to_collection(
        &mut self,
        node: &Node,
        collection: Collection,
        mut_collection: &TypedRegister,
        value: &TypedRegister,
    ) {
        match collection {
            Collection::Vec => {
                let hwm = self.temp_registers.save_mark();

                let target_element_addr_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u32_type()),
                    "address for the element entry",
                );

                self.builder.add_vec_push_addr(
                    target_element_addr_reg.register(),
                    mut_collection,
                    value.ty.basic_type.total_size,
                    node,
                    "add_to_collection for transformer",
                );

                let vec_entry_destination =
                    Destination::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        target_element_addr_reg.register,
                    ));

                let source_destination = if value.ty.is_scalar() {
                    Destination::Register(value.clone())
                } else {
                    Destination::Memory(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        value.clone(),
                    ))
                };

                self.emit_store_value_to_memory_destination(
                    &vec_entry_destination,
                    &source_destination,
                    node,
                    "store value in the new vec entry slot",
                );

                self.temp_registers.restore_to_mark(hwm);
            }
            Collection::Sparse => panic!("not supported by sparse"),
            Collection::Map => todo!(),
            Collection::Grid => todo!(),
            Collection::String => todo!(),
            Collection::Range => todo!(),
        }
    }

    fn transformer_add_to_collection(
        &mut self,
        in_value: &TypedRegister,
        should_unwrap_value: bool,
        collection_type: Collection,
        mut_collection: &TypedRegister,
        node: &Node,
    ) {
        let hwm = self.temp_registers.save_mark();

        let (register_to_be_inserted_in_collection, maybe_temp) = if should_unwrap_value {
            let tagged_union = in_value.underlying().optional_info().unwrap().clone();
            let some_variant = tagged_union.get_variant_by_index(1);
            let payload_vm_type = VmType::new_unknown_placement(some_variant.ty.clone());
            let temp_reg = self
                .temp_registers
                .allocate(payload_vm_type, "transform add to collection");
            let (_tag_offset, _tag_size, payload_offset, _) =
                in_value.underlying().unwrap_info().unwrap();

            /*
            in_value.unsafe_add_offset(payload_offset);

            self.emit_load_from_memory_internal(
                temp_reg.register(),
                in_value,
                payload_offset,
                &payload_vm_type,
                node,
                "transformer add to collection",
            );

             */
            (temp_reg.register.clone(), Some(temp_reg))
        } else {
            (in_value.clone(), None)
        };

        self.add_to_collection(
            node,
            collection_type,
            mut_collection,
            &register_to_be_inserted_in_collection,
        );

        self.temp_registers.restore_to_mark(hwm);
    }

    const fn check_if_transformer_can_skip_early(&self, transformer: Transformer) -> bool {
        match transformer {
            Transformer::For => false,
            Transformer::Filter => false,
            Transformer::Find => true,
            Transformer::Map => false,
            Transformer::Any => true,
            Transformer::All => true,
            Transformer::FilterMap => false,
        }
    }
}
