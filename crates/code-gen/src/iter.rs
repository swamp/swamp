use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::{layout_optional_type, layout_type};
use crate::{Collection, FlagStateKind, Transformer, TransformerResult};
use source_map_node::Node;
use swamp_semantic::{ArgumentExpression, ExpressionKind};
use swamp_types::Type;
use swamp_vm_types::types::{BasicTypeKind, Destination, TypedRegister, VmType, u8_type};
use swamp_vm_types::{InstructionPosition, PatchPosition};

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
    /// 5. If the transformer supports early exit (e.g., filter, find), set the Z flag based on the lambda result.
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
        source_collection_self_region: &TypedRegister,
        source_collection_analyzed_type: &Type,
        lambda_expression: &ArgumentExpression,
        ctx: &Context,
    ) {
        // Take out lambda and other lookups before generating the code
        let ArgumentExpression::Expression(expr) = lambda_expression else {
            panic!("internal error");
        };

        let ExpressionKind::Lambda(lambda_variables, lambda_expr) = &expr.kind else {
            panic!();
        };

        let primary_element_type = source_collection_analyzed_type.primary_element_type();

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

        let lambda_return_analyzed_type = &lambda_expr.ty;

        // 1. Optionally initialize the result vector if the transformer produces one.
        let lambda_return_gen_type = layout_type(lambda_return_analyzed_type);

        if matches!(
            transformer.return_type(),
            TransformerResult::VecWithLambdaResult | TransformerResult::VecFromSourceCollection
        ) {
            let element_size_in_target_vec = match transformer.return_type() {
                TransformerResult::VecFromSourceCollection => {
                    let element_gen_type = layout_type(primary_element_type.unwrap());
                    element_gen_type.total_size
                }
                TransformerResult::VecWithLambdaResult => {
                    if transformer.needs_tag_removed() {
                        let (_tag_size, _tag_offset, _payload_offset, payload_size) =
                            lambda_return_gen_type.unwrap_info().unwrap();
                        payload_size
                    } else {
                        lambda_return_gen_type.total_size
                    }
                }
                _ => panic!("should not happen"),
            };

            let pointer_reg = self.emit_absolute_pointer_if_needed(
                target_destination,
                node,
                "create absolute pointer reg for vec_create",
            );
            self.builder
                .add_vec_create(&pointer_reg, node, "target result vector");
        }

        let hwm = self.temp_registers.save_mark();

        // 2. Initialize the iterator and generate code to fetch the next element.
        let (continue_iteration_label, iteration_complete_patch_position) = self
            .emit_iter_init_and_next(
                node,
                source_collection_type,
                source_collection_self_region,
                &target_variables,
            );

        // 3. Inline the lambda code for the current element(s).
        let lambda_result = self.emit_scalar_rvalue(lambda_expr, ctx);

        // 4. If the transformer supports early exit, set the Z flag based on the lambda result.
        let transformer_t_flag_state =
            self.check_if_transformer_sets_t_flag(transformer, &lambda_result, node);

        // 5. Conditionally skip result insertion if early exit is triggered.
        let maybe_skip_early = if matches!(
            transformer_t_flag_state,
            FlagStateKind::TFlagIsTrueWhenSet | FlagStateKind::TFlagIsTrueWhenClear
        ) {
            // The z flag is set so we can act on it
            let skip_early = self.builder.add_jmp_if_not_equal_polarity_placeholder(
                &transformer_t_flag_state.polarity(),
                node,
                "skip early",
            );

            Some(skip_early)
        } else {
            // Z flag is not set, we have to iterate through the whole collection
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
                let absolute_pointer = self.emit_absolute_pointer_if_needed(
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
                // It is a transformer that returns a bool, lets store z flag as bool it
                self.builder.add_stz(
                    target_destination.register().unwrap(),
                    node,
                    "transformer sets standard bool",
                );
            }
            TransformerResult::WrappedValueFromSourceCollection => {
                let some_payload = layout_optional_type(&Type::Optional(Box::from(
                    lambda_return_analyzed_type.clone(),
                )));
                let BasicTypeKind::Optional(tagged_union) = &some_payload.kind else {
                    panic!("expected optional {:?}", target_destination.ty());
                };

                //let tag_target = ctx.target_register().move_to_optional_tag();
                let tag_target = self.temp_registers.allocate(
                    VmType::new_unknown_placement(u8_type()),
                    "iterate over collection target",
                );

                self.builder
                    .add_mov8_immediate(tag_target.register(), 1, node, "mark tag as Some");
                /* TODO:

                self.builder.add_st8_using_ptr_with_offset(
                    primary_variable,
                    tagged_union.tag_offset,
                    tag_target.register(),
                    node,
                    "copy Tag value of (1)",
                );

                 */

                //self.copy_contents_to_memory(node, primary_variable, tagged_union.payload_offset, )

                // TODO: Unsure how to copy to memory in a good way
            }
            _ => {}
        }

        self.temp_registers.restore_to_mark(hwm);
    }

    #[allow(clippy::unnecessary_wraps)]
    fn emit_iter_init_and_next(
        &mut self,
        node: &Node,
        collection_type: Collection,
        collection_self_addr: &TypedRegister,
        target_variables: &[TypedRegister],
    ) -> (InstructionPosition, PatchPosition) {
        let iterator_gen_type = collection_type.iterator_gen_type();

        let target_iterator_header_reg = self.allocate_frame_space_and_return_absolute_pointer_reg(
            &iterator_gen_type,
            node,
            "allocate iterator header space",
        );

        let iter_next_position = InstructionPosition(self.builder.position().0 + 1);
        let placeholder = match collection_type {
            Collection::Vec => {
                self.builder.add_vec_iter_init(
                    &target_iterator_header_reg,
                    collection_self_addr,
                    node,
                    "vec init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_vec_iter_next_pair_placeholder(
                        &target_iterator_header_reg,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "vec iter next pair",
                    )
                } else {
                    self.builder.add_vec_iter_next_placeholder(
                        &target_iterator_header_reg,
                        &target_variables[0],
                        node,
                        "vec iter next single",
                    )
                }
            }
            Collection::Map => {
                self.builder.add_map_iter_init(
                    &target_iterator_header_reg,
                    collection_self_addr,
                    node,
                    "map init",
                );

                if target_variables.len() == 2 {
                    self.builder.add_map_iter_next_pair_placeholder(
                        &target_iterator_header_reg,
                        &target_variables[0],
                        &target_variables[1],
                        node,
                        "map next_pair",
                    )
                } else {
                    self.builder.add_map_iter_next_placeholder(
                        &target_iterator_header_reg,
                        &target_variables[0],
                        node,
                        "map next_single",
                    )
                }
            }
            Collection::Grid => todo!(),
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
        };

        (iter_next_position, placeholder)
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
                    .add_tst_u8(in_value, node, "filter bool to z flag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::Find => {
                // TODO: Bring this back //assert_eq!(in_value.size().0, 1); // bool
                self.builder
                    .add_tst_u8(in_value, node, "find: bool to z flag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::Map => FlagStateKind::TFlagIsIndeterminate,
            Transformer::Any => {
                self.builder.add_tst_u8(in_value, node, "any, check tag");
                FlagStateKind::TFlagIsTrueWhenClear
            }
            Transformer::All => {
                self.builder.add_tst_u8(in_value, node, "all, check tag");
                FlagStateKind::TFlagIsTrueWhenSet
            }
            Transformer::FilterMap => {
                self.builder
                    .add_tst_u8(in_value, node, "filter map, check tag");
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
                self.builder.add_vec_push_addr(
                    mut_collection,
                    value,
                    value.ty.basic_type.total_size,
                    node,
                    "push",
                );
            }
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
                .allocate(payload_vm_type.clone(), "transform add to collection");
            let (_tag_offset, _tag_size, payload_offset, _) =
                in_value.underlying().unwrap_info().unwrap();
            self.emit_load_from_memory(
                temp_reg.register(),
                in_value,
                payload_offset,
                &payload_vm_type,
                node,
                "transformer add to collection",
            );
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
}
