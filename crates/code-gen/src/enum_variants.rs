/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::EnumLiteralExpressions;
use swamp_types::prelude::EnumVariantType;
use swamp_types::{TypeKind, TypeRef};
use swamp_vm_types::AggregateMemoryLocation;
use swamp_vm_types::types::{BasicTypeKind, VmType, u8_type};

impl CodeBuilder<'_> {
    pub fn emit_enum_variant_to_memory_location(
        &mut self,
        target_memory_location: &AggregateMemoryLocation,
        enum_type: &TypeRef,
        variant_type: &EnumVariantType,
        sorted_expressions: &EnumLiteralExpressions,
        node: &Node,
        ctx: &Context,
    ) {
        let variant_index = variant_type.common().container_index as usize;
        let layout_gen_enum = self.state.layout_cache.layout(enum_type);
        let BasicTypeKind::TaggedUnion(layout_enum) = &layout_gen_enum.kind else {
            panic!("wrong")
        };
        let layout_variant = layout_enum.get_variant_by_index(variant_index);

        let hwm = self.temp_registers.save_mark();

        let temp_payload_reg = self.temp_registers.allocate(
            VmType::new_unknown_placement(layout_variant.ty.clone()),
            "variant literal payload",
        );

        self.builder.add_mov8_immediate(
            temp_payload_reg.register(),
            variant_index as u8,
            node,
            &format!("enum variant `{}` tag", variant_type.common().assigned_name),
        );

        let tag_memory_location = target_memory_location.offset(layout_enum.tag_offset, u8_type());
        self.builder.add_st8_using_ptr_with_offset(
            &tag_memory_location.location,
            temp_payload_reg.register(),
            node,
            &format!("put enum tag in place {tag_memory_location} <- {temp_payload_reg}"),
        );

        // Get payload memory location
        let payload_basic_type = self.state.layout_cache.layout(&variant_type.payload_type);
        let payload_memory_location =
            target_memory_location.offset(layout_enum.payload_offset, payload_basic_type.clone());

        // Initialize payload memory
        println!(
            "DEBUG: enum variant - Initializing payload memory for type {:?}",
            payload_basic_type.kind
        );
        self.emit_initialize_target_memory_first_time(
            &payload_memory_location.location,
            node,
            &format!(
                "initialize enum variant payload for {}",
                variant_type.common().assigned_name
            ),
        );

        // If this is a tuple with a Vec inside it, we need to directly initialize the Vec's capacity
        if let BasicTypeKind::Tuple(tuple_type) = &payload_basic_type.kind {
            if !tuple_type.fields.is_empty() {
                let first_field = &tuple_type.fields[0];
                if let BasicTypeKind::VecStorage(_, capacity) = &first_field.ty.kind {
                    println!(
                        "DEBUG: enum variant - Found Vec in tuple field with capacity {}",
                        capacity
                    );
                    // Calculate offset to the Vec field inside the tuple
                    let vec_memory_location =
                        payload_memory_location.offset(first_field.offset, first_field.ty.clone());

                    // Initialize the Vec capacity directly
                    self.emit_initialize_target_memory_first_time(
                        &vec_memory_location.location,
                        node,
                        &format!(
                            "initialize Vec inside tuple for enum variant {}",
                            variant_type.common().assigned_name
                        ),
                    );
                }
            }
        }
        println!("DEBUG: enum variant - Finished initializing payload memory");

        match &*variant_type.payload_type.kind {
            TypeKind::Unit => {}
            TypeKind::Tuple(expressions) => {
                let EnumLiteralExpressions::Tuple(tuple_expressions) = sorted_expressions else {
                    panic!("internal error");
                };
                self.emit_tuple_literal_into_memory(
                    &payload_memory_location,
                    &variant_type.payload_type,
                    tuple_expressions,
                    ctx,
                    node,
                );
            }
            TypeKind::AnonymousStruct(anon_struct_type) => {
                let EnumLiteralExpressions::Struct(sorted_field_expressions) = sorted_expressions
                else {
                    panic!("internal error");
                };

                self.emit_anonymous_struct_into_memory(
                    &payload_memory_location,
                    &variant_type.payload_type,
                    sorted_field_expressions,
                    node,
                    ctx,
                );
            }
            _ => {
                // Handle single expression payloads (like Vec, primitives, etc.)
                // These come as Tuple with single element due to syntax RefreshUnits(expr)
                let EnumLiteralExpressions::Tuple(expressions) = sorted_expressions else {
                    panic!(
                        "expected tuple expressions for payload type {:?}",
                        variant_type.payload_type.kind
                    );
                };

                if expressions.len() != 1 {
                    panic!(
                        "expected exactly one expression for single payload type, got {}",
                        expressions.len()
                    );
                }

                self.emit_expression_into_target_memory(
                    &payload_memory_location.location,
                    &expressions[0],
                    &format!(
                        "enum variant payload for {}",
                        variant_type.common().assigned_name
                    ),
                    ctx,
                );
            }
        }

        self.temp_registers.restore_to_mark(hwm);
    }
}
