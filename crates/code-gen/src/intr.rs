/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::layout::layout_type;
use crate::{Collection, FlagState, FlagStateKind, Transformer};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{ArgumentExpression, Expression};
use swamp_types::Type;
use swamp_vm_types::types::{
    BasicType, Destination, RValueOrLValue, TypedRegister, VmType, pointer_type, u16_type,
};
use swamp_vm_types::{
    AggregateMemoryLocation, COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET,
    MAP_HEADER_ELEMENT_COUNT_OFFSET, MemoryLocation, MemoryOffset, MemorySize,
    STRING_HEADER_COUNT_OFFSET,
};
use tracing::info;

impl CodeBuilder<'_> {
    #[allow(clippy::too_many_lines)]
    #[allow(clippy::single_match_else)]
    pub fn emit_single_intrinsic_call(
        &mut self,
        target_reg: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[ArgumentExpression],
        ctx: &Context,
    ) -> FlagState {
        {
            let (self_arg, maybe_self_type) = if arguments.is_empty() {
                (None, None)
            } else {
                let self_region = self.emit_argument_expression(&arguments[0], ctx);
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
                None,
                self_arg.as_ref(),
                rest_args,
                ctx,
                "single intrinsic call",
            )
        }
    }

    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn emit_single_intrinsic_call_with_self(
        &mut self,
        target_destination: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_type: Option<Type>,
        self_basic_type: Option<&BasicType>,
        self_addr_l_or_rvalue: Option<&RValueOrLValue>,
        arguments: &[ArgumentExpression],
        ctx: &Context,
        comment: &str,
    ) -> FlagState {
        let maybe_target = target_destination.register();
        let maybe_pointer = target_destination.memory_location();
        let self_addr: Option<&TypedRegister> = self_addr_l_or_rvalue.and_then(|s| s.rvalue());

        let mut t_flag_result = FlagState::default();
        match intrinsic_fn {
            IntrinsicFunction::RuntimePanic => {
                self.builder
                    .add_panic(self_addr.unwrap(), node, "intrinsic panic");
            }

            IntrinsicFunction::RuntimeHalt => {
                self.builder.add_halt(node, "intrinsic halt");
            }

            IntrinsicFunction::RuntimeStep => {
                self.builder.add_step(node, "intrinsic step");
            }

            IntrinsicFunction::RangeInit => {
                let start_reg = self_addr.unwrap();
                // let MutRefOrImmutableExpression::Expression(start_arg_expr) = start_arg else {
                //    panic!();
                //};
                // let start_reg = self.emit_scalar_rvalue(start_arg_expr, ctx);

                let end_arg = &arguments[0];
                let ArgumentExpression::Expression(end_arg_expr) = end_arg else {
                    panic!();
                };
                let end_reg = self.emit_scalar_rvalue(end_arg_expr, ctx);

                let is_inclusive = &arguments[1];
                let ArgumentExpression::Expression(is_inclusive_expr) = is_inclusive else {
                    panic!();
                };
                let is_inclusive_reg = self.emit_scalar_rvalue(is_inclusive_expr, ctx);
                let absolute_range_pointer = self.emit_compute_effective_address_to_register(
                    target_destination,
                    node,
                    "create range target pointer",
                );
                self.builder.add_range_init(
                    &absolute_range_pointer,
                    start_reg,
                    &end_reg,
                    &is_inclusive_reg,
                    node,
                    "create a range",
                );
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
                let ArgumentExpression::Expression(float_arg_expr) = float_arg else {
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
                let ArgumentExpression::Expression(float_arg_expr) = float_arg else {
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
                let ArgumentExpression::Expression(float_arg_expr) = float_arg else {
                    panic!();
                };
                let float_region = self.emit_scalar_rvalue(float_arg_expr, ctx);

                let float_b = &arguments[1];
                let ArgumentExpression::Expression(float_b_expr) = float_b else {
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
                let int_arg = &arguments[0];
                let ArgumentExpression::Expression(int_arg_expr) = int_arg else {
                    panic!();
                };
                let int_register = self.emit_scalar_rvalue(int_arg_expr, ctx);

                self.builder.add_int_max(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &int_register,
                    node,
                    "int max",
                );
            }
            IntrinsicFunction::IntMin => {
                let int_arg = &arguments[0];
                let ArgumentExpression::Expression(int_arg_expr) = int_arg else {
                    panic!();
                };
                let int_register = self.emit_scalar_rvalue(int_arg_expr, ctx);
                self.builder.add_int_min(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &int_register,
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
            IntrinsicFunction::VecPush => {
                let maybe_element_expr = &arguments[0];
                let ArgumentExpression::Expression(element_expr) = maybe_element_expr else {
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
                let ArgumentExpression::Expression(index_expr) = maybe_index_argument else {
                    panic!();
                };
                let index_region = self.emit_scalar_rvalue(index_expr, ctx);
                let element_type = self_basic_type.unwrap().element().unwrap();

                self.builder.add_vec_remove_index(
                    self_addr.unwrap(),
                    &index_region,
                    &element_type.total_size,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let maybe_key_argument = &arguments[0];
                let ArgumentExpression::Expression(key_expr) = maybe_key_argument else {
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
                let temp_element_count_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    "vec_clear zero",
                );
                self.builder.add_mov_16_immediate_value(
                    temp_element_count_reg.register(),
                    0,
                    node,
                    "set to zero",
                );
                self.builder.add_st16_using_ptr_with_offset(
                    target_destination.grab_memory_location(),
                    temp_element_count_reg.register(),
                    node,
                    "set element_count to zero",
                );
            }
            IntrinsicFunction::VecGet => {
                let maybe_key_argument = &arguments[0];
                let ArgumentExpression::Expression(key_expr) = maybe_key_argument else {
                    panic!();
                };
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);

                self.builder.add_vec_subscript(
                    maybe_target.unwrap(),
                    self_addr.unwrap(), // mut self
                    &key_region,
                    MemorySize(0),
                    node,
                    "vec get",
                );
            }
            IntrinsicFunction::VecWhile => todo!(), // Low prio
            IntrinsicFunction::VecFindMap => todo!(), // Low prio

            IntrinsicFunction::MapLen | IntrinsicFunction::VecLen => {
                self.builder.add_ld16_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    COLLECTION_ELEMENT_COUNT_OFFSET,
                    node,
                    "get the collection element_count",
                );
            }
            IntrinsicFunction::MapCapacity | IntrinsicFunction::VecCapacity => {
                self.builder.add_ld16_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    COLLECTION_CAPACITY_OFFSET,
                    node,
                    "get the collection capacity",
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
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::Filter,
                    self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            }

            IntrinsicFunction::VecFor => {
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::For,
                    self_addr.unwrap(),
                    &self_type.unwrap(),
                    &arguments[0],
                    ctx,
                );
            } // Low prio      IntrinsicFunction::VecFor => {

            IntrinsicFunction::VecFind => {
                info!(?self_type, "what is the self type for vec find");
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
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
            IntrinsicFunction::MapHas => {
                let ArgumentExpression::Expression(key_argument) = &arguments[0] else {
                    panic!("must be expression for key");
                };
                let key = self.emit_scalar_rvalue(key_argument, ctx);
                self.builder.add_map_has(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    &key,
                    node,
                    "map_has",
                );
                t_flag_result.kind = FlagStateKind::TFlagIsTrueWhenSet;
            }
            IntrinsicFunction::MapRemove => {
                let ArgumentExpression::Expression(key_argument) = &arguments[0] else {
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
                self.builder.add_ld16_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    MAP_HEADER_ELEMENT_COUNT_OFFSET,
                    node,
                    "get the map length",
                );
            }
            IntrinsicFunction::MapIsEmpty => {
                self.builder.add_ld16_from_pointer_with_offset_u16(
                    maybe_target.unwrap(),
                    self_addr.unwrap(),
                    MAP_HEADER_ELEMENT_COUNT_OFFSET,
                    node,
                    "get the map length for testing if it is empty",
                );
                self.builder.add_boolean_not(
                    maybe_target.unwrap(),
                    node,
                    "convert the map length to inverted bool",
                );
            }

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
        key_expression: &Expression,
        ctx: &Context,
    ) {
        let key_register =
            self.emit_aggregate_pointer_or_pointer_to_scalar_memory(key_expression, ctx);

        self.builder
            .add_map_remove(map_region, &key_register, &key_expression.node, "");
    }
}
