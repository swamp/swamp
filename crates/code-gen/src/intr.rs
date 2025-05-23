use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::{Collection, FlagState, FlagStateKind, Transformer};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{Expression, MutRefOrImmutableExpression};
use swamp_types::Type;
use swamp_vm_types::types::{Destination, RValueOrLValue, TypedRegister, VmType, pointer_type};
use swamp_vm_types::{
    AggregateMemoryLocation, MAP_HEADER_COUNT_OFFSET, MemoryLocation, MemoryOffset, MemorySize,
    STRING_HEADER_COUNT_OFFSET, VEC_HEADER_COUNT_OFFSET,
};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::single_match_else)]
    pub fn emit_single_intrinsic_call(
        &mut self,
        target_reg: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> FlagState {
        match intrinsic_fn {
            IntrinsicFunction::MapFromSlicePair => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.emit_scalar_rvalue(expr, ctx);

                let slice_type = arguments[0].ty();

                let Type::DynamicSlicePair(key_type, value_type) = slice_type else {
                    panic!("problem");
                };

                assert!(key_type.is_concrete_or_unit()); // is unit when it is empty
                assert!(value_type.is_concrete_or_unit());

                self.builder.add_map_new_from_slice(
                    target_reg.grab_register(),
                    &slice_region,
                    node,
                    "create map from temporary slice pair",
                );

                FlagState::default()
            }

            _ => {
                let (self_arg, maybe_self_type) = if arguments.is_empty() {
                    (None, None)
                } else {
                    let self_region =
                        self.emit_expression_location_mut_ref_or_immutable(&arguments[0], ctx);
                    (Some(self_region), Some(arguments[0].ty()))
                };
                let rest_args = if arguments.len() > 1 {
                    &arguments[1..]
                } else {
                    &vec![]
                };
                self.emit_single_intrinsic_call_with_self(
                    target_reg,
                    node,
                    intrinsic_fn,
                    maybe_self_type,
                    self_arg.as_ref(),
                    rest_args,
                    ctx,
                    "single intrinsic call",
                )
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn emit_single_intrinsic_call_with_self(
        &mut self,
        target_reg: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_type: Option<Type>,
        self_addr_l_or_rvalue: Option<&RValueOrLValue>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
        comment: &str,
    ) -> FlagState {
        let maybe_target = target_reg.register();
        let self_addr: Option<&TypedRegister> = self_addr_l_or_rvalue.and_then(|s| s.rvalue());

        let mut t_flag_result = FlagState::default();
        match intrinsic_fn {
            IntrinsicFunction::RuntimePanic => {
                self.builder
                    .add_panic(self_addr.unwrap(), node, "intrinsic panic");
            }

            // Bool
            IntrinsicFunction::BoolToString => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                self.builder.bool_to_string(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "bool_to_string",
                );
            }

            // Fixed
            IntrinsicFunction::FloatRound => self.builder.add_float_round(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float round",
            ),
            IntrinsicFunction::FloatFloor => self.builder.add_float_floor(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float floor",
            ),
            IntrinsicFunction::FloatSqrt => {
                self.builder.add_float_sqrt(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float sqr",
                );
            }
            IntrinsicFunction::FloatSign => {
                self.builder.add_float_sign(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float sign",
                );
            }
            IntrinsicFunction::FloatAbs => {
                self.builder.add_float_abs(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float abs",
                );
            }
            IntrinsicFunction::FloatRnd => {
                self.builder.add_float_prnd(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float pseudo random",
                );
            }
            IntrinsicFunction::FloatCos => {
                self.builder.add_float_cos(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float cos",
                );
            }
            IntrinsicFunction::FloatSin => {
                self.builder.add_float_sin(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "float sin",
                );
            }
            IntrinsicFunction::FloatAcos => self.builder.add_float_acos(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float acos",
            ),
            IntrinsicFunction::FloatAsin => self.builder.add_float_asin(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float asin",
            ),
            IntrinsicFunction::FloatAtan2 => self.builder.add_float_atan2(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float atan2",
            ),
            IntrinsicFunction::FloatMin => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_scalar_rvalue(float_arg_expr, ctx);
                self.builder.add_float_min(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &float_region,
                    node,
                    "float min",
                );
            }
            IntrinsicFunction::FloatMax => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_scalar_rvalue(float_arg_expr, ctx);
                self.builder.add_float_max(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &float_region,
                    node,
                    "float max",
                );
            }
            IntrinsicFunction::FloatClamp => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_scalar_rvalue(float_arg_expr, ctx);

                let float_b = &arguments[1];
                let MutRefOrImmutableExpression::Expression(float_b_expr) = float_b else {
                    panic!();
                };
                let float_b_region = self.emit_scalar_rvalue(float_b_expr, ctx);

                self.builder.add_float_clamp(
                    maybe_target.unwrap(),
                    &float_region,
                    self_addr.unwrap(),
                    &float_b_region,
                    node,
                    "float round",
                );
            }
            IntrinsicFunction::FloatToString => self.builder.float_to_string(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "float_to_string",
            ),

            // Int
            IntrinsicFunction::IntAbs => {
                self.builder.add_int_abs(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "int abs",
                );
            }

            IntrinsicFunction::IntRnd => {
                self.builder.add_int_rnd(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "int pseudo random",
                );
            }
            IntrinsicFunction::IntMax => {
                self.builder.add_int_max(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "int max",
                );
            }
            IntrinsicFunction::IntMin => {
                self.builder.add_int_min(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "int min",
                );
            }
            IntrinsicFunction::IntClamp => {
                self.builder.add_int_clamp(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    "int clamp",
                );
            }
            IntrinsicFunction::IntToFloat => {
                self.builder.add_int_to_float(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    node,
                    &format!("int to float {}", self_addr.unwrap().comment()),
                );
            }
            IntrinsicFunction::IntToString => self.builder.add_int_to_string(
                maybe_target.unwrap(),
                self_addr.unwrap(),
                node,
                "int_to_string",
            ),

            // String
            IntrinsicFunction::StringLen => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    STRING_HEADER_COUNT_OFFSET,
                    node,
                    "get the length",
                );
            }

            // Vec
            IntrinsicFunction::MapFromSlicePair => {
                panic!("no self in mac from slice")
            }
            IntrinsicFunction::VecPush => {
                let maybe_element_expr = &arguments[0];
                let MutRefOrImmutableExpression::Expression(element_expr) = maybe_element_expr
                else {
                    panic!();
                };
                let element_gen_type = layout_type(&element_expr.ty);

                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(pointer_type()),
                    "pointer to new element",
                );

                self.builder.add_vec_push_addr(
                    temp_element_ptr.register(),
                    self_addr.unwrap(),
                    element_gen_type.total_size,
                    node,
                    "set pointer to new element",
                );

                let location = AggregateMemoryLocation {
                    location: MemoryLocation {
                        base_ptr_reg: temp_element_ptr.register,
                        offset: MemoryOffset(0),
                        ty: VmType::new_unknown_placement(element_gen_type),
                    },
                };

                self.emit_expression_into_target_memory(
                    &location.location,
                    element_expr,
                    "vec push",
                    ctx,
                );
            }

            IntrinsicFunction::VecPop => {
                self.builder.add_vec_pop(
                    maybe_target.unwrap(),
                    self_addr.unwrap(), // mut self
                    node,
                    "vec pop",
                );
            }
            IntrinsicFunction::VecRemoveIndex => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_scalar_rvalue(index_expr, ctx);
                self.builder.add_vec_remove_index(
                    self_addr.unwrap(),
                    &index_region,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);
                self.builder.add_vec_remove_index_get_value(
                    maybe_target.unwrap(),
                    self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec remove index get value",
                );
            }
            IntrinsicFunction::VecClear => {
                self.builder.add_vec_clear(
                    self_addr.unwrap(), // mut self
                    node,
                    "vec clear",
                );
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);
                self.builder.add_vec_get(
                    maybe_target.unwrap(),
                    self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec get",
                );
            }
            IntrinsicFunction::VecCreate => {
                self.builder.add_vec_create(
                    maybe_target.unwrap(),
                    MemorySize(0),
                    node,
                    "vec create",
                ); // TODO: Fix to have proper element memory size
            }
            IntrinsicFunction::VecFromSlice => {
                /*
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(slice_expression) = maybe_key_argument
                else {
                    panic!();
                };

                let element_base_ptr_reg = self.temp_registers.allocate(
                    VmType::new_unknown_placement(vec_type()),
                    "element base ptr",
                );
                let BasicTypeKind::InternalVecStorage(element_type, fixed_size_capacity) =
                    &target_reg.ty.basic_type.kind
                else {
                    panic!("mut have storage");
                };

                self.builder.add_vec_init_fill_capacity_and_element_addr(
                    target_reg,
                    element_base_ptr_reg.register(),
                    *fixed_size_capacity as u16,
                    0,
                    node,
                    "vec create",
                ); // TODO: Fix to have proper element memory size

                // let slice_register = self.emit_slice_literal( element_base_ptr_reg.register(), element_type, node, ctx);

                self.emit_expression_materialize(
                    element_base_ptr_reg.register(),
                    slice_expression,
                    ctx,
                );

                 */
                todo!()
            }

            IntrinsicFunction::VecSubscript => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_scalar_rvalue(index_expr, ctx);
                self.builder.add_vec_subscript(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &index_region,
                    node,
                    "vec get element at index",
                );
            }
            IntrinsicFunction::VecSubscriptMut => {
                let maybe_index_argument = &arguments[0];

                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_scalar_rvalue(index_expr, ctx);
                // TODO:

                /*
                let source_argument = &arguments[1];
                let MutRefOrImmutableExpression::Expression(value_expr) = source_argument else {
                    panic!();
                };

                let value_region = self.emit_expression_for_access(value_expr)?;
                 */
                let value_region = &index_region;

                self.builder.add_vec_set(
                    self_addr.unwrap(),
                    &index_region,
                    value_region,
                    node,
                    "set the vec subscript",
                );
            }
            IntrinsicFunction::VecSubscriptRange => {
                let maybe_range_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(range_expr) = maybe_range_argument
                else {
                    panic!();
                };
                let range_header_region = self.emit_scalar_rvalue(range_expr, ctx);
                // TODO: Bring this back // assert_eq!(range_header_region.size(), RANGE_HEADER_SIZE);
                self.builder.add_vec_get_range(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),   // mut self (string header)
                    &range_header_region, // range x..=y
                    node,
                    "vec subscript range",
                );
            }
            IntrinsicFunction::VecIter => {
                // TODO:
                // Intentionally empty, since it should never be called
            }
            IntrinsicFunction::VecIterMut => {
                // TODO:
                // Intentionally empty, since it should never be called
            }
            IntrinsicFunction::VecFor => todo!(),   // Low prio
            IntrinsicFunction::VecWhile => todo!(), // Low prio
            IntrinsicFunction::VecFindMap => todo!(), // Low prio

            IntrinsicFunction::VecLen => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    VEC_HEADER_COUNT_OFFSET,
                    node,
                    "get the vec length",
                );
            }
            IntrinsicFunction::VecAny => todo!(), // Low prio
            IntrinsicFunction::VecAll => todo!(), // Low prio
            IntrinsicFunction::VecMap => todo!(), // Low prio
            IntrinsicFunction::VecFilterMap => todo!(), // Low prio
            IntrinsicFunction::VecSwap => {
                let index_a = self
                    .emit_for_access_or_location(&arguments[0], ctx)
                    .grab_rvalue()
                    .clone();
                let index_b = self
                    .emit_for_access_or_location(&arguments[1], ctx)
                    .grab_rvalue()
                    .clone();
                self.builder
                    .add_vec_swap(self_addr.unwrap(), &index_a, &index_b, node, "vec swap");
            }

            IntrinsicFunction::VecInsert => { // Low prio
            }
            IntrinsicFunction::VecFirst => { // Low prio
            }
            IntrinsicFunction::VecLast => { // Low prio
            }
            IntrinsicFunction::VecFold => { // Low prio
            }
            IntrinsicFunction::VecFilter => {
                self.emit_iterate_over_collection_with_lambda(
                    maybe_target.unwrap(),
                    node,
                    Collection::Vec,
                    Transformer::Filter,
                    self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            IntrinsicFunction::VecFind => {
                self.emit_iterate_over_collection_with_lambda(
                    maybe_target.unwrap(),
                    node,
                    Collection::Vec,
                    Transformer::Find,
                    self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            // Map
            IntrinsicFunction::MapCreate => {
                // TODO:
            }
            IntrinsicFunction::MapHas => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_scalar_rvalue(key_argument, ctx);
                self.builder
                    .add_map_has(self_addr.unwrap(), &key, node, "map_has");
                t_flag_result.kind = FlagStateKind::TFlagIsTrueWhenSet;
            }
            IntrinsicFunction::MapRemove => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.emit_intrinsic_map_remove(self_addr.unwrap(), key_argument, ctx);
            }
            IntrinsicFunction::MapIter => {
                // Never called directly
            }
            IntrinsicFunction::MapIterMut => {
                // Never called directly
            }
            IntrinsicFunction::MapLen => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    MAP_HEADER_COUNT_OFFSET,
                    node,
                    "get the map length",
                );
            }
            IntrinsicFunction::MapSubscript => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_scalar_rvalue(key_argument, ctx);
                self.builder.add_map_get_entry_location(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &key,
                    node,
                    "map_subscript",
                );
            }
            IntrinsicFunction::MapSubscriptMut => {}
            IntrinsicFunction::MapSubscriptMutCreateIfNeeded => {}

            // Grid
            IntrinsicFunction::GridCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.target(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }

            IntrinsicFunction::GridSet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.emit_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.emit_expression_for_access(y_argument)?;

                let MutRefOrImmutableExpression::Expression(value) = &arguments[2] else {
                    panic!("must be expression for key");
                };
                let value_region = self.emit_expression_for_access(value)?;

                self.state.builder.add_grid_set(
                    ctx.target(),
                    &self_addr.unwrap(),
                    x_region.addr(),
                    y_region.addr(),
                    value_region.addr(),
                    "grid_get",
                );

                 */
            }
            IntrinsicFunction::GridGet => {
                /*
                let MutRefOrImmutableExpression::Expression(x_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let x_region = self.emit_expression_for_access(x_argument)?;
                let MutRefOrImmutableExpression::Expression(y_argument) = &arguments[1] else {
                    panic!("must be expression for key");
                };
                let y_region = self.emit_expression_for_access(y_argument)?;
                self.state.builder.add_grid_get(
                    ctx.target(),
                    &self_addr.unwrap(),
                    x_region.addr(),
                    y_region.addr(),
                    "grid_get",
                );

                 */
            }

            IntrinsicFunction::GridGetColumn => {}
            IntrinsicFunction::GridFromSlice => {}

            // Map2
            IntrinsicFunction::Map2Remove => {}
            IntrinsicFunction::Map2Insert => {}
            IntrinsicFunction::Map2GetColumn => {}
            IntrinsicFunction::Map2GetRow => {}
            IntrinsicFunction::Map2Get => {}
            IntrinsicFunction::Map2Has => {}
            IntrinsicFunction::Map2Create => {}

            // Low prio ========
            // Sparse
            IntrinsicFunction::SparseCreate => {
                /*
                self.state.builder.add_sparse_create(
                    ctx.target(),
                    "map_subscript_mut_create (set)",
                );

                 */
            }
            IntrinsicFunction::SparseAdd => {}
            IntrinsicFunction::SparseFromSlice => {}
            IntrinsicFunction::SparseIter => {}
            IntrinsicFunction::SparseIterMut => {}
            IntrinsicFunction::SparseSubscript => {}
            IntrinsicFunction::SparseSubscriptMut => {}
            IntrinsicFunction::SparseHas => {}
            IntrinsicFunction::SparseRemove => {}

            // Other
            IntrinsicFunction::Float2Magnitude => {}
        }

        t_flag_result
    }

    fn emit_intrinsic_map_remove(
        &mut self,
        map_region: &TypedRegister,
        key_expr: &Expression,
        ctx: &Context,
    ) {
        let key_region = self.emit_scalar_rvalue(key_expr, ctx);

        self.builder
            .add_map_remove(map_region, &key_region, &key_expr.node, "");
    }
}
