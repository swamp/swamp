/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::code_bld::CodeBuilder;
use crate::ctx::Context;
use source_map_node::Node;
use swamp_semantic::EnumLiteralData;
use swamp_types::prelude::{EnumType, EnumVariantType};
use swamp_vm_types::AggregateMemoryLocation;
use swamp_vm_types::types::{VmType, u8_type};

impl CodeBuilder<'_> {
    pub fn emit_enum_variant_to_memory_location(
        &mut self,
        target_memory_location: &AggregateMemoryLocation,
        enum_type: &EnumType,
        a: &EnumVariantType,
        b: &EnumLiteralData,
        node: &Node,
        ctx: &Context,
    ) {
        let variant_index = a.common().container_index as usize;
        let variants = enum_type.variants.values().cloned().collect::<Vec<_>>(); // TODO: @perf: takes a lot of performance
        let layout_enum = layout_enum_into_tagged_union(&enum_type.assigned_name, &variants);
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
            &format!("enum variant `{}` tag", a.common().assigned_name),
        );

        let tag_memory_location = target_memory_location.offset(layout_enum.tag_offset, u8_type());
        self.builder.add_st8_using_ptr_with_offset(
            &tag_memory_location.location,
            temp_payload_reg.register(),
            node,
            &format!("put enum tag in place {tag_memory_location} <- {temp_payload_reg}"),
        );

        let payload_memory_location =
            target_memory_location.offset(layout_enum.payload_offset, u8_type());
        match b {
            EnumLiteralData::Nothing => {}
            EnumLiteralData::Tuple(expressions) => {
                let EnumVariantType::Tuple(tuple_type) = a else {
                    panic!();
                };
                self.emit_tuple_literal_into_memory(
                    &payload_memory_location,
                    &tuple_type.fields_in_order,
                    expressions,
                    ctx,
                    node,
                );
            }
            EnumLiteralData::Struct(sorted_expressions) => {
                let EnumVariantType::Struct(variant_struct_type) = a else {
                    panic!()
                };

                self.emit_anonymous_struct_into_memory(
                    &payload_memory_location,
                    &variant_struct_type.anon_struct,
                    sorted_expressions,
                    node,
                    ctx,
                );
            }
        }

        self.temp_registers.restore_to_mark(hwm);
    }
}
