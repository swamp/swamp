use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::{Collection, GeneratedExpressionResult, GeneratedExpressionResultKind, Transformer};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{Expression, MutRefOrImmutableExpression};
use swamp_types::Type;
use swamp_vm_types::types::TypedRegister;
use swamp_vm_types::{
    MAP_HEADER_COUNT_OFFSET, MemorySize, STRING_HEADER_COUNT_OFFSET, VEC_HEADER_COUNT_OFFSET,
};

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::single_match_else)]
    pub fn emit_single_intrinsic_call(
        &mut self,
        target_reg: &TypedRegister,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
    ) -> GeneratedExpressionResult {
        match intrinsic_fn {
            IntrinsicFunction::MapFromSlicePair => {
                let MutRefOrImmutableExpression::Expression(expr) = &arguments[0] else {
                    panic!("problem");
                };

                let slice_region = self.emit_simple_rvalue(expr, ctx);

                let slice_type = arguments[0].ty();

                let Type::FixedSlicePair(key_type, value_type, _) = slice_type else {
                    panic!("problem");
                };

                assert!(key_type.is_concrete_or_unit()); // is unit when it is empty
                assert!(value_type.is_concrete_or_unit());

                self.builder.add_map_new_from_slice(
                    target_reg,
                    &slice_region,
                    node,
                    "create map from temporary slice pair",
                );

                GeneratedExpressionResult::default()
            }

            _ => {
                let (self_arg, maybe_self_type) = if arguments.is_empty() {
                    (None, None)
                } else {
                    let self_region =
                        self.emit_expression_location_mut_ref_or_immutable(&arguments[0], ctx);
                    (Some(self_region), Some(arguments[0].ty().clone()))
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
        target_reg: &TypedRegister,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_type: Option<Type>,
        self_addr: Option<&TypedRegister>,
        arguments: &[MutRefOrImmutableExpression],
        ctx: &Context,
        comment: &str,
    ) -> GeneratedExpressionResult {
        let mut t_flag_result = GeneratedExpressionResult::default();
        match intrinsic_fn {
            IntrinsicFunction::RuntimePanic => {
                self.builder
                    .add_panic(&self_addr.unwrap(), node, "intrinsic panic");
            }

            // Bool
            IntrinsicFunction::BoolToString => {
                self.builder
                    .bool_to_string(target_reg, &self_addr.unwrap(), node, "bool_to_string")
            }

            // Fixed
            IntrinsicFunction::FloatRound => {
                self.builder
                    .add_float_round(target_reg, &self_addr.unwrap(), node, "float round")
            }
            IntrinsicFunction::FloatFloor => {
                self.builder
                    .add_float_floor(target_reg, &self_addr.unwrap(), node, "float floor")
            }
            IntrinsicFunction::FloatSqrt => {
                self.builder
                    .add_float_sqrt(target_reg, &self_addr.unwrap(), node, "float sqr");
            }
            IntrinsicFunction::FloatSign => {
                self.builder
                    .add_float_sign(target_reg, &self_addr.unwrap(), node, "float sign");
            }
            IntrinsicFunction::FloatAbs => {
                self.builder
                    .add_float_abs(target_reg, &self_addr.unwrap(), node, "float abs");
            }
            IntrinsicFunction::FloatRnd => {
                self.builder.add_float_prnd(
                    target_reg,
                    &self_addr.unwrap(),
                    node,
                    "float pseudo random",
                );
            }
            IntrinsicFunction::FloatCos => {
                self.builder
                    .add_float_cos(target_reg, &self_addr.unwrap(), node, "float cos");
            }
            IntrinsicFunction::FloatSin => {
                self.builder
                    .add_float_sin(target_reg, &self_addr.unwrap(), node, "float sin");
            }
            IntrinsicFunction::FloatAcos => {
                self.builder
                    .add_float_acos(target_reg, &self_addr.unwrap(), node, "float acos")
            }
            IntrinsicFunction::FloatAsin => {
                self.builder
                    .add_float_asin(target_reg, &self_addr.unwrap(), node, "float asin")
            }
            IntrinsicFunction::FloatAtan2 => {
                self.builder
                    .add_float_atan2(target_reg, &self_addr.unwrap(), node, "float atan2")
            }
            IntrinsicFunction::FloatMin => {
                let float_arg = &arguments[0];
                let MutRefOrImmutableExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_simple_rvalue(float_arg_expr, ctx);
                self.builder.add_float_min(
                    target_reg,
                    &self_addr.unwrap(),
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
                let float_region = self.emit_simple_rvalue(float_arg_expr, ctx);
                self.builder.add_float_max(
                    target_reg,
                    &self_addr.unwrap(),
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
                let float_region = self.emit_simple_rvalue(float_arg_expr, ctx);

                let float_b = &arguments[1];
                let MutRefOrImmutableExpression::Expression(float_b_expr) = float_b else {
                    panic!();
                };
                let float_b_region = self.emit_simple_rvalue(float_b_expr, ctx);

                self.builder.add_float_clamp(
                    target_reg,
                    &float_region,
                    &self_addr.unwrap(),
                    &float_b_region,
                    node,
                    "float round",
                );
            }
            IntrinsicFunction::FloatToString => self.builder.float_to_string(
                target_reg,
                &self_addr.unwrap(),
                node,
                "float_to_string",
            ),

            // Int
            IntrinsicFunction::IntAbs => {
                self.builder
                    .add_int_abs(target_reg, self_addr.unwrap(), node, "int abs");
            }

            IntrinsicFunction::IntRnd => {
                self.builder
                    .add_int_rnd(target_reg, self_addr.unwrap(), node, "int pseudo random");
            }
            IntrinsicFunction::IntMax => {
                self.builder
                    .add_int_max(target_reg, &self_addr.unwrap(), node, "int max");
            }
            IntrinsicFunction::IntMin => {
                self.builder
                    .add_int_min(target_reg, &self_addr.unwrap(), node, "int min");
            }
            IntrinsicFunction::IntClamp => {
                self.builder
                    .add_int_clamp(target_reg, &self_addr.unwrap(), node, "int clamp");
            }
            IntrinsicFunction::IntToFloat => {
                self.builder.add_int_to_float(
                    target_reg,
                    &self_addr.unwrap(),
                    node,
                    &format!("int to float {}", self_addr.unwrap().comment()),
                );
            }
            IntrinsicFunction::IntToString => self.builder.add_int_to_string(
                target_reg,
                &self_addr.unwrap(),
                node,
                "int_to_string",
            ),

            // String
            IntrinsicFunction::StringLen => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target_reg,
                    &self_addr.unwrap(),
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
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_simple_rvalue(key_expr, ctx);
                self.builder.add_vec_push(
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec push",
                );
            }
            IntrinsicFunction::VecPop => {
                self.builder.add_vec_pop(
                    target_reg,
                    &self_addr.unwrap(), // mut self
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
                let index_region = self.emit_simple_rvalue(index_expr, ctx);
                self.builder.add_vec_remove_index(
                    &self_addr.unwrap(),
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
                let key_region = self.emit_simple_rvalue(key_expr, ctx);
                self.builder.add_vec_remove_index_get_value(
                    target_reg,
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec remove index get value",
                );
            }
            IntrinsicFunction::VecClear => {
                self.builder.add_vec_clear(
                    &self_addr.unwrap(), // mut self
                    node,
                    "vec clear",
                );
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_simple_rvalue(key_expr, ctx);
                self.builder.add_vec_get(
                    target_reg,
                    &self_addr.unwrap(), // mut self
                    &key_region,
                    node,
                    "vec get",
                );
            }
            IntrinsicFunction::VecCreate => {
                self.builder
                    .add_vec_create(target_reg, MemorySize(0), node, "vec create"); // TODO: Fix to have proper element memory size
            }
            IntrinsicFunction::VecSubscript => {
                let maybe_index_argument = &arguments[0];
                let MutRefOrImmutableExpression::Expression(index_expr) = maybe_index_argument
                else {
                    panic!();
                };
                let index_region = self.emit_simple_rvalue(index_expr, ctx);
                self.builder.add_vec_subscript(
                    target_reg,
                    &self_addr.unwrap(),
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
                let index_region = self.emit_simple_rvalue(index_expr, ctx);
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
                    &self_addr.unwrap(),
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
                let range_header_region = self.emit_simple_rvalue(range_expr, ctx);
                // TODO: Bring this back // assert_eq!(range_header_region.size(), RANGE_HEADER_SIZE);
                self.builder.add_vec_get_range(
                    target_reg,
                    &self_addr.unwrap(),  // mut self (string header)
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
                    target_reg,
                    &self_addr.unwrap(),
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
                let index_a = self.emit_for_access_or_location(&arguments[0], ctx);
                let index_b = self.emit_for_access_or_location(&arguments[1], ctx);
                self.builder.add_vec_swap(
                    &self_addr.unwrap(),
                    &index_a,
                    &index_b,
                    node,
                    "vec swap",
                );
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
                self.iterate_over_collection_with_lambda(
                    target_reg,
                    node,
                    Collection::Vec,
                    Transformer::Filter,
                    &self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            IntrinsicFunction::VecFind => {
                self.iterate_over_collection_with_lambda(
                    target_reg,
                    node,
                    Collection::Vec,
                    Transformer::Find,
                    &self_addr.unwrap(),
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
                let key = self.emit_simple_rvalue(key_argument, ctx);
                self.builder
                    .add_map_has(&self_addr.unwrap(), &key, node, "map_has");
                t_flag_result.kind = GeneratedExpressionResultKind::TFlagIsTrueWhenSet;
            }
            IntrinsicFunction::MapRemove => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                self.emit_intrinsic_map_remove(&self_addr.unwrap(), key_argument, ctx);
            }
            IntrinsicFunction::MapIter => {
                // Never called directly
            }
            IntrinsicFunction::MapIterMut => {
                // Never called directly
            }
            IntrinsicFunction::MapLen => {
                self.builder.add_ld32_from_pointer_with_offset_u16(
                    target_reg,
                    &self_addr.unwrap(),
                    MAP_HEADER_COUNT_OFFSET,
                    node,
                    "get the map length",
                );
            }
            IntrinsicFunction::MapSubscript => {
                let MutRefOrImmutableExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_simple_rvalue(key_argument, ctx);
                self.builder.add_map_fetch(
                    target_reg,
                    &self_addr.unwrap(),
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
        let key_region = self.emit_simple_rvalue(key_expr, ctx);

        self.builder
            .add_map_remove(map_region, &key_region, &key_expr.node, "");
    }
}
