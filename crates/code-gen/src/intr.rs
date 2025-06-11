/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use crate::prelude::layout_type;
use crate::{Collection, FlagState, Transformer};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{ArgumentExpression, Expression, ExpressionKind, VariableRef};
use swamp_vm_types::types::{
    Destination, RValueOrLValue, TypedRegister, VmType, pointer_type, u8_type, u16_type, u32_type,
};
use swamp_vm_types::{
    AggregateMemoryLocation, COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET,
    MemoryLocation, MemoryOffset, MemorySize, PointerLocation, STRING_HEADER_COUNT_OFFSET,
};

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
    ) {
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
                self_arg.as_ref(),
                rest_args,
                ctx,
                "single intrinsic call",
            );
        }
    }

    pub fn emit_intrinsic_map(
        &mut self,
        output_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_ptr_reg: &PointerLocation,
        arguments: &[Expression],
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        match intrinsic_fn {
            IntrinsicFunction::MapHas => {
                let key_argument = &arguments[0];
                let key = self.emit_scalar_rvalue(key_argument, ctx);
                self.builder.add_map_has(
                    output_destination.register().unwrap(),
                    self_ptr_reg,
                    &key,
                    node,
                    "map_has",
                );
            }
            IntrinsicFunction::MapRemove => {
                let key_argument = &arguments[0];
                self.emit_intrinsic_map_remove(self_ptr_reg, key_argument, ctx);
            }
            _ => todo!("missing intrinsic_map {intrinsic_fn}"),
        }
    }

    pub fn emit_intrinsic_sparse(
        &mut self,
        output_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_ptr_reg: &PointerLocation,
        arguments: &[Expression],
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        match intrinsic_fn {
            IntrinsicFunction::SparseAdd => {
                let element_to_add_expression = &arguments[0];

                self.emit_sparse_add(
                    &output_destination.register().unwrap().clone(),
                    self_ptr_reg,
                    element_to_add_expression,
                    node,
                    ctx,
                );
            }

            IntrinsicFunction::SparseRemove => {
                let sparse_id_int_expression = &arguments[0];
                self.emit_sparse_remove(self_ptr_reg, sparse_id_int_expression, node, ctx);
            }

            IntrinsicFunction::SparseIsAlive => {
                let sparse_id_int_expression = &arguments[0];
                self.emit_sparse_is_alive(
                    &output_destination.register().unwrap().clone(),
                    self_ptr_reg,
                    sparse_id_int_expression,
                    node,
                    ctx,
                );
            }
            _ => todo!("unknown sparse {intrinsic_fn}"),
        }
    }
    pub fn emit_intrinsic_grid(
        &mut self,
        output_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_ptr_reg: &PointerLocation,
        arguments: &[Expression],
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        match intrinsic_fn {
            IntrinsicFunction::GridSet => {
                let x_expr = &arguments[0];
                let y_expr = &arguments[1];
                let value_expr = &arguments[2];

                let x_reg = self.emit_scalar_rvalue(x_expr, ctx);
                let y_reg = self.emit_scalar_rvalue(y_expr, ctx);
                let element_gen_type = self_ptr_reg.ptr_reg.ty.basic_type.element().unwrap();

                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(element_gen_type.clone()),
                    "temporary scalar",
                );

                self.builder.add_grid_get_entry_addr(
                    &temp_element_ptr.register,
                    self_ptr_reg,
                    &x_reg,
                    &y_reg,
                    element_gen_type.total_size,
                    node,
                    comment,
                );

                let location = AggregateMemoryLocation {
                    location: MemoryLocation {
                        base_ptr_reg: temp_element_ptr.register,
                        offset: MemoryOffset(0),
                        ty: VmType::new_unknown_placement(element_gen_type.clone()),
                    },
                };

                self.emit_expression_into_target_memory(
                    &location.location,
                    value_expr,
                    "grid set",
                    ctx,
                );
            }
            IntrinsicFunction::GridGet => {
                let x_expr = &arguments[0];
                let y_expr = &arguments[1];

                let x_reg = self.emit_scalar_rvalue(x_expr, ctx);
                let y_reg = self.emit_scalar_rvalue(y_expr, ctx);

                let element_type = self_ptr_reg.ptr_reg.ty.basic_type.element().unwrap();

                let (temp_reg, target_reg) = if element_type.is_scalar() {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(element_type.clone()),
                        "temporary scalar",
                    );
                    (Some(temp_reg.register.clone()), temp_reg.register)
                } else {
                    (None, output_destination.register().unwrap().clone())
                };

                self.builder.add_grid_get_entry_addr(
                    &target_reg,
                    self_ptr_reg,
                    &x_reg,
                    &y_reg,
                    element_type.total_size,
                    node,
                    comment,
                );

                if let Some(temp_reg) = temp_reg {
                    let source_location =
                        MemoryLocation::new_copy_over_whole_type_with_zero_offset(temp_reg);
                    self.emit_load_scalar_from_memory_offset_instruction(
                        &output_destination.register().unwrap().clone(),
                        &source_location,
                        node,
                        comment,
                    );
                }
            }
            _ => todo!("wrong grid {intrinsic_fn}"),
        }
    }

    #[allow(clippy::too_many_lines)]
    fn emit_intrinsic_call_vec(
        &mut self,
        output_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_ptr_reg: &PointerLocation,
        arguments: &[Expression],
        node: &Node,
        ctx: &Context,
    ) {
        let self_basic_type = &self_ptr_reg.ptr_reg.ty.basic_type;
        match intrinsic_fn {
            IntrinsicFunction::VecPush => {
                let element_expr = &arguments[0];

                let element_gen_type = layout_type(element_expr.ty.underlying());

                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(pointer_type()),
                    "pointer to new element",
                );

                self.builder.add_vec_push_addr(
                    temp_element_ptr.register(),
                    &self_ptr_reg.ptr_reg,
                    element_gen_type.total_size,
                    node,
                    "set pointer to new element",
                );

                let location = AggregateMemoryLocation {
                    location: MemoryLocation {
                        base_ptr_reg: temp_element_ptr.register,
                        offset: MemoryOffset(0),
                        ty: VmType::new_unknown_placement(element_gen_type.clone()),
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
                let element_type = self_basic_type.element().unwrap();
                self.builder.add_vec_pop(
                    output_destination.clone().register().unwrap(),
                    &self_ptr_reg.ptr_reg, // mut self
                    element_type.total_size,
                    node,
                    "vec pop",
                );
                let source_memory_location =
                    MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        output_destination.clone().register().unwrap().clone(),
                    );
                if element_type.is_scalar() {
                    self.emit_load_scalar_from_memory_offset_instruction(
                        output_destination.clone().register().unwrap(),
                        &source_memory_location,
                        node,
                        "load scalar from popped value",
                    );
                }
            }
            IntrinsicFunction::VecRemoveIndex => {
                let index_region_expr = &arguments[0];
                let index_region = self.emit_scalar_rvalue(index_region_expr, ctx);

                let element_type = self_basic_type.element().unwrap();

                self.builder.add_vec_remove_index(
                    &self_ptr_reg.ptr_reg,
                    &index_region,
                    &element_type.total_size,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let key_expr = &arguments[0];
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);

                self.builder.add_vec_remove_index_get_value(
                    output_destination.clone().register().unwrap(),
                    &self_ptr_reg.ptr_reg, // mut self
                    &key_region,
                    node,
                    "vec remove index get value",
                );
            }
            IntrinsicFunction::VecRemoveFirstIndexGetValue => {
                let zero_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u8_type()),
                    "vec remove first. set index 0",
                );
                self.builder
                    .add_mov8_immediate(zero_reg.register(), 0, node, "zero index");
                let value_addr_reg = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u32_type()),
                    "vec entry addr to copy from",
                );
                let element_type = self_basic_type.element().unwrap();
                self.builder.add_vec_subscript(
                    value_addr_reg.register(),
                    &self_ptr_reg.ptr_reg,
                    zero_reg.register(),
                    element_type.total_size,
                    node,
                    "lookup first entry in vec",
                );

                let source_memory_location =
                    MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        value_addr_reg.register,
                    );
                self.emit_load_value_from_memory_source(
                    output_destination.register().unwrap(),
                    &source_memory_location,
                    node,
                    "load the vec entry to target register",
                );

                self.builder.add_vec_remove_index(
                    &self_ptr_reg.ptr_reg, // mut self
                    zero_reg.register(),
                    &element_type.total_size,
                    node,
                    "vec remove first index",
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
                    output_destination.grab_memory_location(),
                    temp_element_count_reg.register(),
                    node,
                    "set element_count to zero",
                );
            }
            IntrinsicFunction::VecGet => {
                let key_expr = &arguments[0];
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);

                self.builder.add_vec_subscript(
                    output_destination.register().unwrap(),
                    &self_ptr_reg.ptr_reg, // mut self
                    &key_region,
                    MemorySize(0),
                    node,
                    "vec get",
                );
            }
            _ => todo!("Vec {intrinsic_fn}"),
        }

        /*
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

        */
    }

    fn emit_intrinsic_call_int(
        &mut self,
        target_reg: &TypedRegister,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[TypedRegister],
        node: &Node,
    ) {
        let first_argument = &arguments[0];
        match intrinsic_fn {
            IntrinsicFunction::IntAbs => {
                self.builder
                    .add_int_abs(target_reg, first_argument, node, "int abs");
            }

            IntrinsicFunction::IntRnd => {
                self.builder
                    .add_int_rnd(target_reg, first_argument, node, "int pseudo random");
            }
            IntrinsicFunction::IntMax => {
                let int_register = &arguments[1];

                self.builder
                    .add_int_max(target_reg, first_argument, int_register, node, "int max");
            }
            IntrinsicFunction::IntMin => {
                let int_register = &arguments[1];

                self.builder
                    .add_int_min(target_reg, first_argument, int_register, node, "int min");
            }
            IntrinsicFunction::IntClamp => {
                let min_reg = &arguments[1];
                let max_reg = &arguments[2];
                self.builder.add_int_clamp(
                    target_reg,
                    first_argument,
                    min_reg,
                    max_reg,
                    node,
                    "int clamp",
                );
            }
            IntrinsicFunction::IntToFloat => {
                self.builder.add_int_to_float(
                    target_reg,
                    first_argument,
                    node,
                    &format!("int to float {}", first_argument.comment()),
                );
            }
            IntrinsicFunction::IntToString => {
                self.builder
                    .add_int_to_string(target_reg, first_argument, node, "int_to_string")
            }
            _ => {}
        }
    }

    #[allow(clippy::too_many_lines)]
    fn emit_intrinsic_call_fixed(
        &mut self,
        target_reg: &TypedRegister,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[TypedRegister],
        node: &Node,
    ) {
        let first_argument_reg = &arguments[0];
        match intrinsic_fn {
            IntrinsicFunction::FloatRound => {
                self.builder
                    .add_float_round(target_reg, first_argument_reg, node, "float round")
            }
            IntrinsicFunction::FloatFloor => {
                self.builder
                    .add_float_floor(target_reg, first_argument_reg, node, "float floor")
            }
            IntrinsicFunction::FloatSqrt => {
                self.builder
                    .add_float_sqrt(target_reg, first_argument_reg, node, "float sqr");
            }
            IntrinsicFunction::FloatSign => {
                self.builder
                    .add_float_sign(target_reg, first_argument_reg, node, "float sign");
            }
            IntrinsicFunction::FloatAbs => {
                self.builder
                    .add_float_abs(target_reg, first_argument_reg, node, "float abs");
            }
            IntrinsicFunction::FloatRnd => {
                self.builder.add_float_prnd(
                    target_reg,
                    first_argument_reg,
                    node,
                    "float pseudo random",
                );
            }
            IntrinsicFunction::FloatCos => {
                self.builder
                    .add_float_cos(target_reg, first_argument_reg, node, "float cos");
            }
            IntrinsicFunction::FloatSin => {
                self.builder
                    .add_float_sin(target_reg, first_argument_reg, node, "float sin");
            }
            IntrinsicFunction::FloatAcos => {
                self.builder
                    .add_float_acos(target_reg, first_argument_reg, node, "float acos")
            }
            IntrinsicFunction::FloatAsin => {
                self.builder
                    .add_float_asin(target_reg, first_argument_reg, node, "float asin")
            }
            IntrinsicFunction::FloatAtan2 => {
                self.builder
                    .add_float_atan2(target_reg, first_argument_reg, node, "float atan2")
            }
            IntrinsicFunction::FloatMin => {
                let float_region = &arguments[1];
                self.builder.add_float_min(
                    target_reg,
                    first_argument_reg,
                    &float_region,
                    node,
                    "float min",
                );
            }
            IntrinsicFunction::FloatMax => {
                let float_region = &arguments[1];
                self.builder.add_float_max(
                    target_reg,
                    first_argument_reg,
                    &float_region,
                    node,
                    "float max",
                );
            }
            IntrinsicFunction::FloatClamp => {
                let float_region = &arguments[1];
                let float_b_region = &arguments[2];

                self.builder.add_float_clamp(
                    target_reg,
                    &float_region,
                    first_argument_reg,
                    &float_b_region,
                    node,
                    "float round",
                );
            }
            IntrinsicFunction::FloatToString => self.builder.float_to_string(
                target_reg,
                first_argument_reg,
                node,
                "float_to_string",
            ),
            _ => panic!("wasn't a fixed operation"),
        }
    }

    pub fn emit_intrinsic_transformer(
        &mut self,
        target_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_addr: &PointerLocation,
        lambda: (Vec<VariableRef>, &Expression),
        node: &Node,
        ctx: &Context,
    ) {
        match intrinsic_fn {
            IntrinsicFunction::TransformerFold => { // Low prio
            }
            IntrinsicFunction::TransformerFilter => {
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::Filter,
                    &self_addr.ptr_reg,
                    lambda,
                    ctx,
                );
            }

            IntrinsicFunction::TransformerFor => {
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::For,
                    &self_addr.ptr_reg,
                    lambda,
                    ctx,
                );
            } // Low prio      IntrinsicFunction::VecFor => {

            IntrinsicFunction::TransformerFind => {
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::Find,
                    &self_addr.ptr_reg,
                    lambda,
                    ctx,
                );
            }
            _ => todo!(),
        }
    }

    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn emit_single_intrinsic_call_with_self(
        &mut self,
        target_destination: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_addr_l_or_rvalue: Option<&RValueOrLValue>,
        arguments: &[ArgumentExpression],
        ctx: &Context,
        comment: &str,
    ) {
        let maybe_target = target_destination.register();
        let self_addr: Option<&TypedRegister> = self_addr_l_or_rvalue.and_then(|s| s.rvalue());

        let mut t_flag_result = FlagState::default();
        match intrinsic_fn {
            IntrinsicFunction::Float2Magnitude
            | IntrinsicFunction::FloatAbs
            | IntrinsicFunction::FloatRound
            | IntrinsicFunction::FloatFloor
            | IntrinsicFunction::FloatSqrt
            | IntrinsicFunction::FloatSign
            | IntrinsicFunction::FloatRnd
            | IntrinsicFunction::FloatCos
            | IntrinsicFunction::FloatSin
            | IntrinsicFunction::FloatAcos
            | IntrinsicFunction::FloatAsin
            | IntrinsicFunction::FloatAtan2
            | IntrinsicFunction::FloatMin
            | IntrinsicFunction::FloatMax
            | IntrinsicFunction::FloatClamp
            | IntrinsicFunction::FloatToString => {
                // Float
                let (temp_reg, dest_reg) = if target_destination.is_register() {
                    (None, target_destination.register().unwrap().clone())
                } else {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(u32_type()),
                        "temporary destination for low level intrinsic",
                    );

                    (Some(temp_reg.register.clone()), temp_reg.register.clone())
                };

                let converted: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression
                    })
                    .collect();

                let mut converted_regs: Vec<_> = converted
                    .iter()
                    .map(|expr| self.emit_scalar_rvalue(expr, ctx))
                    .collect();

                converted_regs.insert(0, self_addr.unwrap().clone());

                self.emit_intrinsic_call_fixed(&dest_reg, intrinsic_fn, &converted_regs, node);

                if let Some(temp_reg) = temp_reg {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        target_destination.grab_memory_location(),
                        &temp_reg,
                        node,
                        "put the low level intrinsic fixed (float) back to memory",
                    );
                }
            }

            IntrinsicFunction::IntAbs
            | IntrinsicFunction::IntRnd
            | IntrinsicFunction::IntMax
            | IntrinsicFunction::IntMin
            | IntrinsicFunction::IntClamp
            | IntrinsicFunction::IntToFloat
            | IntrinsicFunction::IntToString => {
                // Int
                let (temp_reg, dest_reg) = if target_destination.is_register() {
                    (None, target_destination.register().unwrap().clone())
                } else {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(u32_type()),
                        "temporary destination for low level intrinsic",
                    );

                    (Some(temp_reg.register.clone()), temp_reg.register.clone())
                };

                let converted: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression
                    })
                    .collect();

                let mut converted_regs: Vec<_> = converted
                    .iter()
                    .map(|expr| self.emit_scalar_rvalue(expr, ctx))
                    .collect();
                converted_regs.insert(0, self_addr.unwrap().clone());

                self.emit_intrinsic_call_int(&dest_reg, intrinsic_fn, &converted_regs, node);

                if let Some(temp_reg) = temp_reg {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        target_destination.grab_memory_location(),
                        &temp_reg,
                        node,
                        "put the low level intrinsic fixed (float) back to memory",
                    );
                }
            }

            IntrinsicFunction::VecPush
            | IntrinsicFunction::VecPop
            | IntrinsicFunction::VecRemoveIndex
            | IntrinsicFunction::VecRemoveIndexGetValue
            | IntrinsicFunction::VecRemoveFirstIndexGetValue
            | IntrinsicFunction::VecClear
            | IntrinsicFunction::VecSwap
            | IntrinsicFunction::VecInsert
            | IntrinsicFunction::VecFirst
            | IntrinsicFunction::VecGet
            | IntrinsicFunction::VecLast => {
                // Vec
                // Self is assumed to be a flattened pointer:
                let vec_self_ptr_reg = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                let converted_to_expressions: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression.clone()
                    })
                    .collect();

                self.emit_intrinsic_call_vec(
                    target_destination,
                    intrinsic_fn,
                    &vec_self_ptr_reg,
                    &converted_to_expressions,
                    node,
                    ctx,
                );
            }

            IntrinsicFunction::GridGet | IntrinsicFunction::GridSet => {
                // Grid
                // Self is assumed to be a flattened pointer:
                let grid_self_ptr_reg = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                let converted_to_expressions: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression.clone()
                    })
                    .collect();
                self.emit_intrinsic_grid(
                    target_destination,
                    intrinsic_fn,
                    &grid_self_ptr_reg,
                    &converted_to_expressions,
                    node,
                    comment,
                    ctx,
                );
            }

            IntrinsicFunction::SparseIsAlive
            | IntrinsicFunction::SparseRemove
            | IntrinsicFunction::SparseAdd => {
                // Sparse
                // Self is assumed to be a flattened pointer:
                let grid_self_ptr_reg = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                let converted_to_expressions: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression.clone()
                    })
                    .collect();
                self.emit_intrinsic_sparse(
                    target_destination,
                    intrinsic_fn,
                    &grid_self_ptr_reg,
                    &converted_to_expressions,
                    node,
                    comment,
                    ctx,
                );
            }

            IntrinsicFunction::TransformerFor
            | IntrinsicFunction::TransformerWhile
            | IntrinsicFunction::TransformerFindMap
            | IntrinsicFunction::TransformerAny
            | IntrinsicFunction::TransformerAll
            | IntrinsicFunction::TransformerMap
            | IntrinsicFunction::TransformerFilter
            | IntrinsicFunction::TransformerFilterMap
            | IntrinsicFunction::TransformerFind
            | IntrinsicFunction::TransformerFold => {
                // Self is assumed to be a flattened pointer:
                let collection_self_ptr_reg = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };

                let lambda_expression = &arguments[0];

                // Take out lambda and other lookups before generating the code
                let ArgumentExpression::Expression(expr) = lambda_expression else {
                    panic!("internal error");
                };

                let ExpressionKind::Lambda(lambda_variables, lambda_expr) = &expr.kind else {
                    panic!("must have lambda for transformers");
                };

                self.emit_intrinsic_transformer(
                    target_destination,
                    intrinsic_fn,
                    &collection_self_ptr_reg,
                    (lambda_variables.clone(), lambda_expr),
                    node,
                    ctx,
                );
            }

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

            // Common Collection
            IntrinsicFunction::MapIsEmpty | IntrinsicFunction::VecIsEmpty => {
                let collection_pointer = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                self.emit_collection_is_empty(
                    maybe_target.unwrap().clone(),
                    &collection_pointer,
                    node,
                    "vec empty",
                );
            }

            IntrinsicFunction::MapLen | IntrinsicFunction::VecLen => {
                let collection_pointer = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                self.emit_collection_len(
                    maybe_target.unwrap(),
                    &collection_pointer,
                    node,
                    "get the collection element_count",
                );
            }
            IntrinsicFunction::MapCapacity | IntrinsicFunction::VecCapacity => {
                let collection_pointer = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                self.emit_collection_capacity(
                    maybe_target.unwrap(),
                    &collection_pointer,
                    node,
                    "get the collection element_count",
                );
            }

            IntrinsicFunction::MapRemove | IntrinsicFunction::MapHas => {
                // Map
                // Self is assumed to be a flattened pointer:
                let grid_self_ptr_reg = PointerLocation {
                    ptr_reg: self_addr.unwrap().clone(),
                };
                let converted_to_expressions: Vec<_> = arguments
                    .iter()
                    .map(|arg| {
                        let ArgumentExpression::Expression(found_expression) = arg else {
                            panic!("must be expression");
                        };
                        found_expression.clone()
                    })
                    .collect();
                self.emit_intrinsic_map(
                    target_destination,
                    intrinsic_fn,
                    &grid_self_ptr_reg,
                    &converted_to_expressions,
                    node,
                    comment,
                    ctx,
                );
            }

            // Other
            IntrinsicFunction::Float2Magnitude => todo!(),
        }
    }

    fn emit_intrinsic_map_remove(
        &mut self,
        map_header_reg: &PointerLocation,
        key_expression: &Expression,
        ctx: &Context,
    ) {
        let key_register =
            self.emit_aggregate_pointer_or_pointer_to_scalar_memory(key_expression, ctx);

        self.builder
            .add_map_remove(map_header_reg, &key_register, &key_expression.node, "");
    }

    fn emit_collection_capacity(
        &mut self,
        output_reg: &TypedRegister,
        collection_addr: &PointerLocation,
        node: &Node,
        comment: &str,
    ) {
        self.builder.add_ld16_from_pointer_with_offset_u16(
            output_reg,
            &collection_addr.ptr_reg,
            COLLECTION_CAPACITY_OFFSET,
            node,
            comment,
        );
    }

    fn emit_collection_len(
        &mut self,
        output_reg: &TypedRegister,
        collection_addr: &PointerLocation,
        node: &Node,
        comment: &str,
    ) {
        self.builder.add_ld16_from_pointer_with_offset_u16(
            output_reg,
            &collection_addr.ptr_reg,
            COLLECTION_ELEMENT_COUNT_OFFSET,
            node,
            &format!("{comment} - collection element_count"),
        );
    }

    fn emit_collection_is_empty(
        &mut self,
        output_reg: TypedRegister,
        collection_addr: &PointerLocation,
        node: &Node,
        comment: &str,
    ) {
        self.builder.add_ld16_from_pointer_with_offset_u16(
            &output_reg,
            &collection_addr.ptr_reg,
            COLLECTION_ELEMENT_COUNT_OFFSET,
            node,
            "get the map length for testing if it is empty",
        );
        self.builder
            .add_boolean_not(&output_reg, node, "convert the map length to inverted bool");
    }
}
