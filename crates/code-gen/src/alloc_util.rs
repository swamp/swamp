/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::alloc::ScopeAllocator;
use crate::layout::type_size_and_alignment;
use seq_map::SeqMap;
use swamp_types::{EnumType, EnumVariantType, NamedStructType, Type};
use swamp_vm_types::{
    FrameMemoryRegion, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE, MAP_HEADER_ALIGNMENT,
    MAP_HEADER_SIZE, MemoryAlignment, MemoryOffset, MemorySize, RANGE_HEADER_ALIGNMENT,
    RANGE_HEADER_SIZE, VEC_HEADER_ALIGNMENT, VEC_HEADER_SIZE,
};
/*
#[must_use]
pub fn layout_tuple_old(types: &Vec<Type>) -> (MemorySize, MemoryAlignment) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;
    for ty in types {
        let (field_size, field_alignment) = type_size_and_alignment(&ty);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        calculated_offset.space(field_size, field_alignment);
    }
    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);
    (total_offset.as_size(), largest_alignment)
}

#[must_use]
pub fn layout_tuple_elements(
    types: &Vec<Type>,
) -> (MemorySize, MemoryAlignment, Vec<(MemoryOffset, MemorySize)>) {
    let mut calculated_offset = MemoryOffset(0);
    let mut largest_alignment = MemoryAlignment::U8;

    let mut elements = Vec::new();
    for ty in types {
        let (field_size, field_alignment) = type_size_and_alignment(&ty);
        if field_alignment.greater_than(largest_alignment) {
            largest_alignment = field_alignment;
        }
        elements.push((calculated_offset, field_size));
        calculated_offset.space(field_size, field_alignment);
    }
    let total_offset = calculated_offset.space(MemorySize(0), largest_alignment);
    (total_offset.as_size(), largest_alignment, elements)
}

pub fn layout_union_old(
    variants: &SeqMap<String, EnumVariantType>,
) -> (MemorySize, MemoryAlignment, Vec<(MemoryOffset, MemorySize)>) {
    let mut max_variant_alignment = MemoryAlignment::U8;
    let mut max_variant_size = MemorySize(0);
    let mut calculated_offset = MemoryOffset(0);

    let mut elements = Vec::new();
    for (_name, variant) in variants {
        let (variant_size, variant_alignment) = match variant {
            EnumVariantType::Struct(anon_struct) => {
                let (struct_size, struct_alignment, _elements) =
                    layout_struct(&anon_struct.anon_struct);
                (struct_size, struct_alignment)
            }
            EnumVariantType::Tuple(types) => layout_tuple(&types.fields_in_order),
            EnumVariantType::Nothing(_) => (MemorySize(0), MemoryAlignment::U8),
        };

        elements.push((calculated_offset, variant_size));

        if variant_alignment.greater_than(max_variant_alignment) {
            max_variant_alignment = variant_alignment;
        }

        if variant_size.0 > max_variant_size.0 {
            max_variant_size = variant_size;
        }
    }

    (max_variant_size, max_variant_alignment, elements)
}
*/

pub fn type_with_tag_size_and_alignment(inner_type: &Type) -> (MemorySize, MemoryAlignment) {
    let (offset, alignment) = type_size_and_alignment(inner_type);

    let alignment_octets: usize = alignment.into();

    (MemorySize(offset.0 + alignment_octets as u16), alignment) // Add the alignment for the tag
}

/*
pub fn layout_union_with_tag(
    enum_type: &EnumType,
) -> (MemorySize, MemoryAlignment, Vec<(MemoryOffset, MemorySize)>) {
    enum_type.variants.iter().map(|(name, variant)| (name, variant))

    let (offset, alignment, offsets) = layout_union(&enum_type.assigned_name, );

    let mut converted_offsets = Vec::new();

    let alignment_octets_usize: usize = alignment.into();
    let alignment_octets: u16 = alignment_octets_usize as u16;

    converted_offsets.push((MemoryOffset(0), MemorySize(alignment_octets)));
    for (offset, size) in offsets {
        converted_offsets.push((MemoryOffset(offset.0 + alignment_octets), size));
    }

    (
        MemorySize(offset.0 + alignment_octets),
        alignment,
        converted_offsets,
    )
}

 */

pub fn reserve_space_for_type(ty: &Type, allocator: &mut ScopeAllocator) -> FrameMemoryRegion {
    let (size, alignment) = type_size_and_alignment(ty);

    allocator.reserve(size, alignment)
}
