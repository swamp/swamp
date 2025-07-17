/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::code_bld::CodeBuilder;
use crate::ctx::Context;

use crate::transformer::{Collection, Transformer};
use source_map_node::Node;
use swamp_semantic::intr::IntrinsicFunction;
use swamp_semantic::{ArgumentExpression, Expression, ExpressionKind, VariableRef};
use swamp_vm_types::types::{
    float_type, int_type, pointer_type, u16_type, u32_type, u8_type, Destination, TypedRegister,
    VmType,
};
use swamp_vm_types::{
    AggregateMemoryLocation, MemoryLocation, MemoryOffset,
    PointerLocation, COLLECTION_CAPACITY_OFFSET, COLLECTION_ELEMENT_COUNT_OFFSET, GRID_HEADER_HEIGHT_OFFSET,
    GRID_HEADER_WIDTH_OFFSET,
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
            // For primitive intrinsics, materialize the self argument to a register early
            let self_reg = if arguments.is_empty() {
                None
            } else {
                let ArgumentExpression::Expression(self_expr) = &arguments[0] else {
                    panic!("Expected expression for self argument");
                };
                Some(self.emit_scalar_rvalue(self_expr, ctx))
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
                self_reg.as_ref(),
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
                // We have to get the key materialized in a temporary storage, so the map can calculate the hash for it.
                let key_temp_storage_reg =
                    self.emit_aggregate_pointer_or_pointer_to_scalar_memory(key_argument, ctx);

                self.builder.add_map_has(
                    output_destination.register().unwrap(),
                    self_ptr_reg,
                    &key_temp_storage_reg,
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
        target_destination: &Destination,
        intrinsic_fn: &IntrinsicFunction,
        self_ptr_reg: &PointerLocation,
        arguments: &[Expression],
        node: &Node,
        comment: &str,
        ctx: &Context,
    ) {
        let (temp_reg, dest_reg) = if target_destination.is_register() {
            (None, target_destination.register().unwrap().clone())
        } else {
            let temp_reg = self.temp_registers.allocate(
                VmType::new_contained_in_register(float_type()),
                "temporary destination for low level intrinsic",
            );

            (Some(temp_reg.register.clone()), temp_reg.register)
        };
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

                // Initialize the allocated space first (like variable definition)
                if element_gen_type.is_aggregate() {
                    self.emit_initialize_memory_for_any_type(
                        &location.location,
                        node,
                        "initialize grid set allocated space",
                    );
                }

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

                // Allocate a temporary register to hold the address of the grid element
                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(pointer_type()),
                    "temp for grid element address",
                );

                // Get the address of the grid element using the opcode
                self.builder.add_grid_get_entry_addr(
                    &temp_element_ptr.register,
                    self_ptr_reg,
                    &x_reg,
                    &y_reg,
                    element_type.total_size,
                    node,
                    comment,
                );

                // Create a memory location from the element address with correct type information
                let element_memory_location = MemoryLocation {
                    base_ptr_reg: temp_element_ptr.register,
                    offset: MemoryOffset(0),
                    ty: VmType::new_unknown_placement(element_type),
                };

                // Use emit_copy_value_from_memory_location to handle both register and memory destinations
                // This will properly handle aggregates (like optionals) vs scalars
                self.emit_copy_value_from_memory_location(
                    target_destination,
                    &element_memory_location,
                    node,
                    "copy grid element value to destination",
                );
            }

            IntrinsicFunction::GridWidth => {
                // Allocate a temporary register for the width value
                let temp = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    "temp for grid width",
                );

                // Get the memory location of the width field in the grid header
                let self_memory_location = AggregateMemoryLocation::new(
                    MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        self_ptr_reg.ptr_reg.clone(),
                    ),
                );
                let width_location =
                    self_memory_location.offset(GRID_HEADER_WIDTH_OFFSET, int_type());

                // Load the width value from the grid header into the temporary register
                self.builder.add_ld16_from_pointer_from_memory_location(
                    &temp.register,
                    &width_location.location,
                    node,
                    comment,
                );

                // Create a source destination from the temporary register
                let value_source = Destination::Register(temp.register);

                // Use emit_copy_value_between_destinations to handle both register and memory destinations
                self.emit_copy_value_between_destinations(
                    target_destination,
                    &value_source,
                    node,
                    "store grid width to destination",
                );
            }
            IntrinsicFunction::GridHeight => {
                // Allocate a temporary register for the height value
                let temp = self.temp_registers.allocate(
                    VmType::new_contained_in_register(u16_type()),
                    "temp for grid height",
                );

                // Get the memory location of the height field in the grid header
                let self_memory_location = AggregateMemoryLocation::new(
                    MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                        self_ptr_reg.ptr_reg.clone(),
                    ),
                );
                let height_location =
                    self_memory_location.offset(GRID_HEADER_HEIGHT_OFFSET, int_type());

                // Load the height value from the grid header into the temporary register
                self.builder.add_ld16_from_pointer_from_memory_location(
                    &temp.register,
                    &height_location.location,
                    node,
                    comment,
                );

                // Create a source destination from the temporary register
                let value_source = Destination::Register(temp.register);

                // Use emit_copy_value_between_destinations to handle both register and memory destinations
                self.emit_copy_value_between_destinations(
                    target_destination,
                    &value_source,
                    node,
                    "store grid height to destination",
                );
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

                let element_gen_type = self.state.layout_cache.layout(&element_expr.ty);

                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(pointer_type()),
                    "pointer to new element",
                );

                self.builder.add_vec_push_addr(
                    temp_element_ptr.register(),
                    &self_ptr_reg.ptr_reg,
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

                // Initialize the allocated space first (like variable definition)
                if element_gen_type.is_aggregate() {
                    self.emit_initialize_memory_for_any_type(
                        &location.location,
                        node,
                        "initialize vec.push allocated space",
                    );
                }

                self.emit_expression_into_target_memory(
                    &location.location,
                    element_expr,
                    "vec push",
                    ctx,
                );
            }

            IntrinsicFunction::VecPop => {
                let element_type = self_basic_type.element().unwrap();
                let pop_target_reg = if let Some(found_target_reg) = output_destination.register() {
                    found_target_reg.clone()
                } else {
                    let temp = self.temp_registers.allocate(
                        VmType::new_contained_in_register(element_type.clone()),
                        "temp for vec pop",
                    );
                    temp.register
                };
                self.builder.add_vec_pop(
                    &pop_target_reg,
                    &self_ptr_reg.ptr_reg, // mut self
                    element_type.total_size,
                    node,
                    "vec pop",
                );
                let source_memory_location = MemoryLocation {
                    base_ptr_reg: pop_target_reg,
                    offset: MemoryOffset(0),
                    ty: VmType::new_unknown_placement(element_type),
                };

                self.emit_copy_value_from_memory_location(
                    output_destination,
                    &source_memory_location,
                    node,
                    "copy from vec pop",
                );
            }

            IntrinsicFunction::VecSlice => {
                let range_expr = &arguments[0];
                let range_region = self.emit_scalar_rvalue(range_expr, ctx);

                let output_pointer = self.emit_compute_effective_address_to_register(
                    output_destination,
                    node,
                    "get absolute pointer for vec slice destination",
                );
                let output_pointer_location = PointerLocation::new(output_pointer);

                self.builder.add_vec_copy_range(&output_pointer_location, self_ptr_reg, &range_region, node, "vec slice");
            }

            IntrinsicFunction::VecRemoveIndex => {
                let index_region_expr = &arguments[0];
                let index_region = self.emit_scalar_rvalue(index_region_expr, ctx);

                let element_type = self_basic_type.element().unwrap();

                self.builder.add_vec_remove_index(
                    &self_ptr_reg.ptr_reg,
                    &index_region,
                    node,
                    "remove index",
                );
            }
            IntrinsicFunction::VecRemoveIndexGetValue => {
                let key_expr = &arguments[0];
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);
                let element_type = self_basic_type.element().unwrap();

                // Handle both register and memory destinations
                if let Some(target_reg) = output_destination.register() {
                    // Direct register destination
                    self.builder.add_vec_remove_index_get_value(
                        target_reg,
                        &self_ptr_reg.ptr_reg, // mut self
                        &key_region,
                        node,
                        "vec remove index get value to register",
                    );
                } else {
                    // Memory destination or other
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(element_type),
                        "temp for vec remove index get value",
                    );

                    self.builder.add_vec_remove_index_get_value(
                        &temp_reg.register,
                        &self_ptr_reg.ptr_reg,
                        &key_region,
                        node,
                        "vec remove index get value to temp",
                    );

                    // Copy from temporary register to destination
                    let source = Destination::Register(temp_reg.register);
                    self.emit_copy_value_between_destinations(
                        output_destination,
                        &source,
                        node,
                        "copy vec element to destination",
                    );
                }
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

                let source_memory_location = MemoryLocation {
                    base_ptr_reg: value_addr_reg.register,
                    offset: MemoryOffset(0),
                    ty: VmType::new_unknown_placement(element_type),
                };

                self.emit_copy_value_from_memory_location(
                    output_destination,
                    &source_memory_location,
                    node,
                    "load the vec entry to target register",
                );

                self.builder.add_vec_remove_index(
                    &self_ptr_reg.ptr_reg, // mut self
                    zero_reg.register(),
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

                let self_memory_location = AggregateMemoryLocation::new(MemoryLocation::new_copy_over_whole_type_with_zero_offset(
                    self_ptr_reg.ptr_reg.clone(),
                ));

                self.builder.add_st16_using_ptr_with_offset(
                    &self_memory_location.offset(COLLECTION_ELEMENT_COUNT_OFFSET, u16_type()).location,
                    temp_element_count_reg.register(),
                    node,
                    "set element_count to zero",
                );
            }

            IntrinsicFunction::VecGet => {
                let key_expr = &arguments[0];
                let key_region = self.emit_scalar_rvalue(key_expr, ctx);
                let element_type = self_ptr_reg.ptr_reg.ty.basic_type.element().unwrap();

                // Similar approach as GridGet - get pointer to element and use copy helpers
                let temp_element_ptr = self.temp_registers.allocate(
                    VmType::new_contained_in_register(pointer_type()),
                    "temp for vec element address",
                );

                // Get the address of the vector element
                self.builder.add_vec_subscript(
                    temp_element_ptr.register(),
                    &self_ptr_reg.ptr_reg,
                    &key_region,
                    element_type.total_size,
                    node,
                    "get vec element address",
                );

                // Create a memory location for the element
                let element_memory_location = MemoryLocation {
                    base_ptr_reg: temp_element_ptr.register,
                    offset: MemoryOffset(0),
                    ty: VmType::new_unknown_placement(element_type),
                };

                // Copy from memory location to destination (works for both register and memory)
                self.emit_copy_value_from_memory_location(
                    output_destination,
                    &element_memory_location,
                    node,
                    "copy vec element to destination",
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

        // Intrinsics can operate on any register directly, no need for register protection
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
                    .add_int_to_string(target_reg, first_argument, node, "int_to_string");
            }
            _ => {}
        }
        // No need to copy from a temporary register as we're using target_reg directly
    }

    #[allow(clippy::too_many_lines)]
    fn emit_intrinsic_call_fixed(
        &mut self,
        target_reg: &TypedRegister,
        intrinsic_fn: &IntrinsicFunction,
        arguments: &[TypedRegister],
        node: &Node,
    ) {
        // Intrinsics can operate directly on any register, no need for temporary registers
        let first_argument_reg = &arguments[0];
        match intrinsic_fn {
            IntrinsicFunction::FloatRound => {
                self.builder
                    .add_float_round(target_reg, first_argument_reg, node, "float round");
            }
            IntrinsicFunction::FloatFloor => {
                self.builder
                    .add_float_floor(target_reg, first_argument_reg, node, "float floor");
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
                    .add_float_acos(target_reg, first_argument_reg, node, "float acos");
            }
            IntrinsicFunction::FloatAsin => {
                self.builder
                    .add_float_asin(target_reg, first_argument_reg, node, "float asin");
            }
            IntrinsicFunction::FloatAtan2 => {
                self.builder
                    .add_float_atan2(target_reg, first_argument_reg, node, "float atan2");
            }
            IntrinsicFunction::FloatMin => {
                let float_region = &arguments[1];
                self.builder.add_float_min(
                    target_reg,
                    first_argument_reg,
                    float_region,
                    node,
                    "float min",
                );
            }
            IntrinsicFunction::FloatMax => {
                let float_region = &arguments[1];
                self.builder.add_float_max(
                    target_reg,
                    first_argument_reg,
                    float_region,
                    node,
                    "float max",
                );
            }
            IntrinsicFunction::FloatClamp => {
                let float_region = &arguments[1];
                let float_b_region = &arguments[2];

                self.builder.add_float_clamp(
                    target_reg,
                    float_region,
                    first_argument_reg,
                    float_b_region,
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
        // No need to copy from temp register to target as we're using target_reg directly
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
            }
            IntrinsicFunction::TransformerWhile => {
                self.emit_iterate_over_collection_with_lambda(
                    target_destination,
                    node,
                    Collection::Vec,
                    Transformer::While,
                    &self_addr.ptr_reg,
                    lambda,
                    ctx,
                );
            }

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
            _ => todo!("{intrinsic_fn}"),
        }
    }

    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn emit_single_intrinsic_call_with_self_destination(
        &mut self,
        target_destination: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_destination: Option<&Destination>,
        arguments: &[ArgumentExpression],
        ctx: &Context,
        comment: &str,
    ) {
        // Use the helper function to properly materialize the self argument
        let self_reg = if let Some(self_dest) = self_destination {
            self.emit_load_scalar_or_absolute_aggregate_pointer(self_dest, node, comment)
        } else {
            None
        };

        // Delegate to the existing function
        self.emit_single_intrinsic_call_with_self(
            target_destination,
            node,
            intrinsic_fn,
            self_reg.as_ref(),
            arguments,
            ctx,
            comment,
        );
    }

    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    pub fn emit_single_intrinsic_call_with_self(
        &mut self,
        target_destination: &Destination,
        node: &Node,
        intrinsic_fn: &IntrinsicFunction,
        self_reg: Option<&TypedRegister>,
        arguments: &[ArgumentExpression],
        ctx: &Context,
        comment: &str,
    ) {
        let maybe_target = target_destination.register();

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
                        VmType::new_contained_in_register(float_type()),
                        "temporary destination for low level intrinsic",
                    );

                    (Some(temp_reg.register.clone()), temp_reg.register)
                };

                // Materialize self to ensure we have the actual scalar value
                let mut converted_regs = vec![self_reg.unwrap().clone()];
                for arg in arguments {
                    let ArgumentExpression::Expression(found_expression) = arg else {
                        panic!("must be expression");
                    };
                    let materialized_arg = self.emit_scalar_rvalue(found_expression, ctx);
                    converted_regs.push(materialized_arg);
                }

                self.emit_intrinsic_call_fixed(&dest_reg, intrinsic_fn, &converted_regs, node);

                if let Some(temp_reg) = temp_reg {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        target_destination.grab_memory_location(),
                        &temp_reg,
                        node,
                        "store the fixed point value into memory",
                    );
                }
            }

            IntrinsicFunction::IntToFloat => {
                // IntToFloat - special case because it returns a float, not an int
                let (temp_reg, dest_reg) = if target_destination.is_register() {
                    (None, target_destination.register().unwrap().clone())
                } else {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(float_type()),
                        "temporary destination for int to float intrinsic",
                    );

                    (Some(temp_reg.register.clone()), temp_reg.register)
                };

                // Self is already materialized as a register
                let int_value_reg = self_reg.unwrap();

                // Now convert the materialized integer value to float
                self.builder.add_int_to_float(
                    &dest_reg,
                    int_value_reg,
                    node,
                    &format!("int to float {}", int_value_reg.comment()),
                );

                if let Some(temp_reg) = temp_reg {
                    self.emit_store_scalar_to_memory_offset_instruction(
                        target_destination.grab_memory_location(),
                        &temp_reg,
                        node,
                        "store the float result from int to float conversion",
                    );
                }
            }

            IntrinsicFunction::IntAbs
            | IntrinsicFunction::IntRnd
            | IntrinsicFunction::IntMax
            | IntrinsicFunction::IntMin
            | IntrinsicFunction::IntClamp
            | IntrinsicFunction::IntToString => {
                // Int
                let (temp_reg, dest_reg) = if target_destination.is_register() {
                    let target_reg = target_destination.register().unwrap();
                    // Intrinsics can operate on any register directly, no special treatment needed
                    (None, target_reg.clone())
                } else {
                    let temp_reg = self.temp_registers.allocate(
                        VmType::new_contained_in_register(u32_type()),
                        "temporary destination for low level intrinsic",
                    );

                    (Some(temp_reg.register.clone()), temp_reg.register)
                };

                // Materialize additional arguments (self is already materialized)
                let mut converted_regs = vec![self_reg.unwrap().clone()];
                for arg in arguments {
                    let ArgumentExpression::Expression(found_expression) = arg else {
                        panic!("must be expression");
                    };
                    let materialized_arg = self.emit_scalar_rvalue(found_expression, ctx);
                    converted_regs.push(materialized_arg);
                }

                self.emit_intrinsic_call_int(&dest_reg, intrinsic_fn, &converted_regs, node);

                if let Some(temp_reg) = temp_reg {
                    if target_destination.is_register() {
                        // Copy from temp to target register
                        self.builder.add_mov_reg(
                            target_destination.register().unwrap(),
                            &temp_reg,
                            node,
                            "copy intrinsic result from temp to target register",
                        );
                    } else {
                        // Store to memory location
                        self.emit_store_scalar_to_memory_offset_instruction(
                            target_destination.grab_memory_location(),
                            &temp_reg,
                            node,
                            "put the low level intrinsic fixed (int) back to memory",
                        );
                    }
                }
            }

            IntrinsicFunction::VecPush
            | IntrinsicFunction::VecPop
            | IntrinsicFunction::VecRemoveIndex
            | IntrinsicFunction::VecRemoveIndexGetValue
            | IntrinsicFunction::VecRemoveFirstIndexGetValue
            | IntrinsicFunction::VecClear
            | IntrinsicFunction::VecSlice
            | IntrinsicFunction::VecSwap
            | IntrinsicFunction::VecInsert
            | IntrinsicFunction::VecFirst
            | IntrinsicFunction::VecGet
            | IntrinsicFunction::VecLast => {
                // Vec
                // Self is assumed to be a flattened pointer:
                let vec_self_ptr_reg = PointerLocation {
                    ptr_reg: self_reg.unwrap().clone(),
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

            IntrinsicFunction::GridGet
            | IntrinsicFunction::GridSet
            | IntrinsicFunction::GridWidth
            | IntrinsicFunction::GridHeight => {
                // Grid
                // Self is assumed to be a flattened pointer:
                let grid_self_ptr_reg = PointerLocation {
                    ptr_reg: self_reg.unwrap().clone(),
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
                    ptr_reg: self_reg.unwrap().clone(),
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
                    ptr_reg: self_reg.unwrap().clone(),
                };

                let lambda_expression = &arguments[0];

                // Take out lambda and other lookups before generating the code
                let ArgumentExpression::Expression(expr) = lambda_expression else {
                    panic!("err");
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
                    .add_panic(self_reg.unwrap(), node, "intrinsic panic");
            }

            IntrinsicFunction::RuntimeHalt => {
                self.builder.add_halt(node, "intrinsic halt");
            }

            IntrinsicFunction::RuntimeStep => {
                self.builder.add_step(node, "intrinsic step");
            }

            IntrinsicFunction::RangeInit => {
                let start_reg = self_reg.unwrap();
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
            IntrinsicFunction::CodepointToString => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                self.builder.codepoint_to_string(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "char_to_string",
                );
            }

            IntrinsicFunction::CodepointToInt => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                self.builder.add_mov_reg(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "char_to_int",
                );
            }

            // Bool
            IntrinsicFunction::ByteToString => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                self.builder.byte_to_string(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "byte_to_string",
                );
            }

            IntrinsicFunction::ByteToInt => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                // It is safe to "upcast" to an i32 from a u8, so just copy the register
                // TODO: Make something smarter so we don't have to copy
                self.builder.add_mov_reg(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "byte_to_int",
                );
            }

            // Bool
            IntrinsicFunction::BoolToString => {
                if maybe_target.is_none() {
                    eprintln!("problem");
                }
                self.builder.bool_to_string(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "bool_to_string",
                );
            }

            IntrinsicFunction::StringToString => {
                self.builder.string_to_string(
                    maybe_target.unwrap(),
                    self_reg.unwrap(),
                    node,
                    "string_to_string",
                );
            }

            // Common Collection
            IntrinsicFunction::MapIsEmpty | IntrinsicFunction::VecIsEmpty => {
                let collection_pointer = PointerLocation {
                    ptr_reg: self_reg.unwrap().clone(),
                };
                self.emit_collection_is_empty(
                    maybe_target.unwrap().clone(),
                    &collection_pointer,
                    node,
                    "vec empty",
                );
            }

            IntrinsicFunction::StringLen
            | IntrinsicFunction::MapLen
            | IntrinsicFunction::VecLen => {
                let collection_pointer = PointerLocation {
                    ptr_reg: self_reg.unwrap().clone(),
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
                    ptr_reg: self_reg.unwrap().clone(),
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
                    ptr_reg: self_reg.unwrap().clone(),
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
            } // All intrinsic cases are now handled above
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
        _comment: &str,
    ) {
        self.builder.add_ld16_from_pointer_with_offset_u16(
            &output_reg,
            &collection_addr.ptr_reg,
            COLLECTION_ELEMENT_COUNT_OFFSET,
            node,
            "get the map length for testing if it is empty",
        );
        self.builder.add_meqz(
            &output_reg,
            &output_reg,
            node,
            "convert the map length to inverted bool",
        );
    }
}
