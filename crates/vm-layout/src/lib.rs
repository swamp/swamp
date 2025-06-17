/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

//! Layouts analyzed into Vm Types (`BasicType`)
use seq_map::SeqMap;
use std::cmp::max;
use std::rc::Rc;
use swamp_types::prelude::{AnonymousStructType, EnumType, EnumVariantType, NamedStructType};
use swamp_types::{TypeId, TypeKind, TypeRef};
use swamp_vm_types::types::{
    BasicType, BasicTypeId, BasicTypeKind, BasicTypeRef, OffsetMemoryItem, StructType, TaggedUnion,
    TaggedUnionVariant, TupleType,
};
use swamp_vm_types::{
    CountU16, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE, MAP_HEADER_ALIGNMENT, MAP_HEADER_SIZE,
    MemoryAlignment, MemoryOffset, MemorySize, PTR_ALIGNMENT, PTR_SIZE, STRING_PTR_ALIGNMENT,
    STRING_PTR_SIZE, VEC_HEADER_SIZE, adjust_size_to_alignment, align_to,
};

#[derive(Copy, Clone)]
struct VariantLayout {
    pub size: MemorySize,
    pub alignment: MemoryAlignment,
}

#[derive(Copy, Clone)]
pub struct TaggedUnionLayout {
    pub tag_offset: MemoryOffset,
    pub tag_size: MemorySize,
    pub tag_alignment: MemoryAlignment,
    pub payload_offset: MemoryOffset,
    pub payload_size: MemorySize,
    pub payload_alignment: MemoryAlignment,
    pub total_size: MemorySize,
    pub max_alignment: MemoryAlignment,
}

pub struct LayoutCache {
    pub id_to_layout: SeqMap<TypeId, BasicTypeRef>,
    pub kind_to_layout: SeqMap<TypeKind, BasicTypeRef>,
}

impl Default for LayoutCache {
    fn default() -> Self {
        Self::new()
    }
}

impl LayoutCache {
    #[must_use] pub fn new() -> Self {
        Self {
            id_to_layout: SeqMap::default(),
            kind_to_layout: SeqMap::default(),
        }
    }
    pub fn layout(&mut self, analyzed_type: TypeRef) -> BasicTypeRef {
        // First check if we already have a layout for this type ID
        if let Some(x) = self.id_to_layout.get(&analyzed_type.id) {
            return x.clone();
        }

        // Check if we already have a layout for this kind of type
        if let Some(existing_layout) = self.kind_to_layout.get(&analyzed_type.kind) {
            // For deduplication, we reuse the existing layout directly
            // This ensures pointer equality for structurally identical types
            let _ = self
                .id_to_layout
                .insert(analyzed_type.id, existing_layout.clone());
            return existing_layout.clone();
        }

        let basic_type = self.layout_type(&analyzed_type);

        let _ = self
            .id_to_layout
            .insert(analyzed_type.id, basic_type.clone());
        // Also store in kind_to_layout for future deduplication
        let _ = self
            .kind_to_layout
            .insert((*analyzed_type.kind).clone(), basic_type.clone());

        basic_type
    }

    fn layout_tagged_union(variants: &[VariantLayout]) -> TaggedUnionLayout {
        let num_variants = variants.len();
        let (tag_size, tag_alignment) = if num_variants <= 0xFF {
            (MemorySize(1), MemoryAlignment::U8)
        } else if num_variants <= 0xFFFF {
            (MemorySize(2), MemoryAlignment::U16)
        } else {
            (MemorySize(4), MemoryAlignment::U32)
        };

        let max_payload_size = variants
            .iter()
            .map(|v| v.size)
            .max()
            .unwrap_or(MemorySize(0));
        let max_payload_alignment = variants
            .iter()
            .map(|v| v.alignment)
            .max()
            .unwrap_or(MemoryAlignment::U8);

        let payload_offset = align_to(MemoryOffset(tag_size.0), max_payload_alignment);
        let max_alignment = max(tag_alignment, max_payload_alignment);

        let complete_size_before_alignment = MemorySize(payload_offset.0 + max_payload_size.0);
        let total_size = adjust_size_to_alignment(complete_size_before_alignment, max_alignment);

        TaggedUnionLayout {
            tag_offset: MemoryOffset(0),
            tag_size,
            tag_alignment,
            payload_offset,
            payload_size: max_payload_size,
            payload_alignment: max_payload_alignment,
            total_size,
            max_alignment,
        }
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn layout_enum_into_tagged_union(
        &mut self,
        name: &str,
        variants: &[EnumVariantType],
    ) -> TaggedUnion {
        let variant_infos = variants.iter().map(|variant| match variant {
            EnumVariantType::Struct(s) => {
                // Get the struct type from the TypeRef
                let struct_type_ref = s.struct_type.clone();

                // Check if we already have a layout for this type
                let struct_basic_type = self.layout_type(&struct_type_ref);

                // No need to add to kind_to_layout cache as layout_type already does this

                (
                    VariantLayout {
                        size: struct_basic_type.total_size,
                        alignment: struct_basic_type.max_alignment,
                    },
                    TaggedUnionVariant {
                        name: s.common.assigned_name.clone(),
                        ty: struct_basic_type,
                    },
                )
            }
            EnumVariantType::Tuple(t) => {
                // Check if we already have a layout for this tuple type
                let tuple_kind = TypeKind::Tuple(t.fields_in_order.clone());

                let tuple_basic_type =
                    if let Some(existing_layout) = self.kind_to_layout.get(&tuple_kind) {
                        // Use the existing layout if we have one
                        existing_layout.clone()
                    } else {
                        // Otherwise create a new layout
                        let tuple_layout = self.layout_tuple_items(&t.fields_in_order);
                        let new_tuple_basic_type = Rc::new(BasicType {
                            id: BasicTypeId(0), // Using 0 as a consistent ID for all intermediate types
                            total_size: tuple_layout.total_size,
                            max_alignment: tuple_layout.max_alignment,
                            kind: BasicTypeKind::Tuple(tuple_layout),
                        });

                        // Add to kind_to_layout cache for deduplication
                        let _ = self
                            .kind_to_layout
                            .insert(tuple_kind, new_tuple_basic_type.clone());
                        new_tuple_basic_type
                    };

                (
                    VariantLayout {
                        size: tuple_basic_type.total_size,
                        alignment: tuple_basic_type.max_alignment,
                    },
                    TaggedUnionVariant {
                        name: t.common.assigned_name.clone(),
                        ty: tuple_basic_type,
                    },
                )
            }

            EnumVariantType::Nothing(n) => {
                let empty_type = Rc::new(BasicType {
                    id: BasicTypeId(0), // Using 0 as a consistent ID for all intermediate types
                    total_size: MemorySize(0),
                    max_alignment: MemoryAlignment::U8,
                    kind: BasicTypeKind::Empty,
                });

                (
                    VariantLayout {
                        size: MemorySize(0),
                        alignment: MemoryAlignment::U8,
                    },
                    TaggedUnionVariant {
                        name: n.common.assigned_name.clone(),
                        ty: empty_type,
                    },
                )
            }
        });

        let (variant_layouts, tagged_variants): (Vec<VariantLayout>, Vec<TaggedUnionVariant>) =
            variant_infos.into_iter().unzip();
        let tagged_union_layout = Self::layout_tagged_union(&variant_layouts);

        TaggedUnion {
            name: name.to_string(),
            tag_offset: tagged_union_layout.tag_offset,
            tag_size: tagged_union_layout.tag_size,
            payload_max_size: tagged_union_layout.payload_size,
            payload_offset: tagged_union_layout.payload_offset,
            variants: tagged_variants,
            total_size: tagged_union_layout.total_size,
            max_alignment: tagged_union_layout.max_alignment,
        }
    }

    #[must_use]
    pub fn layout_enum(
        &mut self,
        name: &str,
        variants: &[EnumVariantType],
        type_id: TypeId,
    ) -> BasicTypeRef {
        // Check if we already have a layout for this kind
        let enum_kind = TypeKind::Enum(EnumType::new(
            source_map_node::Node::default(),
            name,
            vec![String::new()],
        ));
        if let Some(existing_layout) = self.kind_to_layout.get(&enum_kind) {
            return existing_layout.clone();
        }

        let tagged_union = self.layout_enum_into_tagged_union(name, variants);

        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(type_id.inner()),
            total_size: tagged_union.total_size,
            max_alignment: tagged_union.max_alignment,
            kind: BasicTypeKind::TaggedUnion(tagged_union),
        });

        // Store in kind_to_layout
        let _ = self.kind_to_layout.insert(enum_kind, basic_type.clone());

        basic_type
    }

    fn layout_vec_like(
        &mut self,
        element_type: &TypeRef,
        capacity: usize,
    ) -> (BasicTypeRef, MemorySize, MemoryAlignment) {
        let element_type_basic = self.layout_type(element_type);
        let total_size =
            element_type_basic.total_size.0 as usize * capacity + VEC_HEADER_SIZE.0 as usize;
        let max_alignment = max(element_type_basic.max_alignment, MemoryAlignment::U16);

        (
            element_type_basic,
            MemorySize(total_size as u32),
            max_alignment,
        )
    }

    /// Computes the memory layout for a type in the target architecture.
    ///
    /// In compiler terminology:
    ///
    /// - "layout" determines size, alignment, and field offsets of types
    /// - "materialization" refers to how types are represented in memory
    /// - "lowering" is the process of mapping source types to machine types
    ///
    /// This function performs type lowering by mapping high-level types to their
    /// concrete memory representations, handling:
    ///
    /// - Primitive, scalar types (integers, floats, booleans)
    /// - Aggregate types (structs, tuples, enums)
    /// - Reference types (pointers, slices)
    /// - Collection types (vectors, maps)
    ///
    /// The layout process considers:
    ///
    /// - Size: Total bytes needed for the type
    /// - Alignment: Memory boundary requirements
    /// - Padding: Gaps needed for proper field alignment
    /// - ABI: Target architecture requirements
    ///
    /// # Panics
    ///
    #[allow(clippy::too_many_lines)]
    #[must_use]
    fn layout_type(&mut self, ty: &TypeRef) -> BasicTypeRef {
        // First check if we already have a layout for this type ID
        if let Some(x) = self.id_to_layout.get(&ty.id) {
            return x.clone();
        }

        // Check if we already have a layout for this kind of type
        if let Some(existing_layout) = self.kind_to_layout.get(&ty.kind) {
            // For deduplication, we need to create a new BasicType with the same structure
            // but with the original TypeId, so that pointer equality works for the same
            // structural types but we still have separate entries for each TypeId
            let new_basic_type = Rc::new(BasicType {
                id: BasicTypeId(ty.id.inner()),
                total_size: existing_layout.total_size,
                max_alignment: existing_layout.max_alignment,
                kind: existing_layout.kind.clone(),
            });

            // Store the mapping from this type ID to the new layout
            let _ = self.id_to_layout.insert(ty.id, new_basic_type.clone());

            // For nested types, we need to properly track all component types
            // This is essential for the deduplication to work correctly
            match &*ty.kind {
                TypeKind::AnonymousStruct(struct_type) => {
                    // Process each field type
                    for field in struct_type.field_name_sorted_fields.values() {
                        let field_type = &field.field_type;
                        let field_layout = self.layout(field_type.clone());
                        let _ = self.id_to_layout.insert(field_type.id, field_layout);
                    }
                }
                TypeKind::NamedStruct(named_struct) => {
                    // Process each field type in the anonymous struct
                    for field in named_struct
                        .anon_struct_type
                        .field_name_sorted_fields
                        .values()
                    {
                        let field_type = &field.field_type;
                        let field_layout = self.layout(field_type.clone());
                        let _ = self.id_to_layout.insert(field_type.id, field_layout);
                    }
                }
                TypeKind::Tuple(tuple_types) => {
                    // Process each tuple element type
                    for elem_type in tuple_types {
                        let elem_layout = self.layout(elem_type.clone());
                        let _ = self.id_to_layout.insert(elem_type.id, elem_layout);
                    }
                }
                _ => {}
            }

            return new_basic_type;
        }

        let basic_type = match &*ty.kind {
            TypeKind::Int => create_basic_type(
                ty.id,
                BasicTypeKind::S32,
                MemorySize(4),
                MemoryAlignment::U32,
            ),
            TypeKind::Float => create_basic_type(
                ty.id,
                BasicTypeKind::Fixed32,
                MemorySize(4),
                MemoryAlignment::U32,
            ),
            TypeKind::Bool => {
                create_basic_type(ty.id, BasicTypeKind::B8, MemorySize(1), MemoryAlignment::U8)
            }
            TypeKind::String => create_basic_type(
                ty.id,
                BasicTypeKind::InternalStringPointer,
                STRING_PTR_SIZE,
                STRING_PTR_ALIGNMENT,
            ),
            TypeKind::AnonymousStruct(struct_type) => {
                self.layout_struct(struct_type, "anonymous", ty.id)
            }
            TypeKind::NamedStruct(named_struct) => self.layout_named_struct(named_struct, ty.id),

            TypeKind::Tuple(tuple_types) => self.layout_tuple(tuple_types, ty.id),
            TypeKind::Optional(inner_type) => self.layout_optional_type(inner_type, ty.id),
            TypeKind::Enum(enum_type) => {
                let variants = enum_type.variants.values().cloned().collect::<Vec<_>>();
                self.layout_enum(&enum_type.assigned_name, &variants, ty.id)
            }
            TypeKind::FixedCapacityAndLengthArray(element_type, capacity) => {
                let (element_layout, element_size, element_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let array_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::FixedCapacityArray(element_layout.clone(), *capacity),
                    total_size: MemorySize(element_size.0 * (*capacity as u32)),
                    max_alignment: element_alignment,
                });

                // Also store the element type in id_to_layout
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout);

                array_type
            }
            TypeKind::DynamicLengthVecView(element_type) => {
                let (element_layout, _, _) = self.layout_vec_like(element_type, 0);

                let vec_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthVecView(element_layout.clone()),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                });

                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout);

                vec_type
            }
            TypeKind::VecStorage(element_type, capacity) => {
                let (element_layout, element_size, element_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let storage_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::VecStorage(element_layout.clone(), *capacity),
                    total_size: MemorySize(VEC_HEADER_SIZE.0 + element_size.0 * (*capacity as u32)),
                    max_alignment: max(MemoryAlignment::U16, element_alignment),
                });

                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout);

                storage_type
            }
            TypeKind::DynamicLengthMapView(key_type, value_type) => {
                // Layout key type
                let key_layout = self.layout(key_type.clone());
                let _ = self.id_to_layout.insert(key_type.id, key_layout.clone());

                // Layout value type
                let value_layout = self.layout(value_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(value_type.id, value_layout.clone());

                let key_item = OffsetMemoryItem {
                    offset: MemoryOffset(0),
                    size: key_layout.total_size,
                    name: "key".to_string(),
                    ty: key_layout.clone(),
                };

                let value_offset = align_to(
                    MemoryOffset(key_layout.total_size.0),
                    value_layout.max_alignment,
                );

                let value_item = OffsetMemoryItem {
                    offset: value_offset,
                    size: value_layout.total_size,
                    name: "value".to_string(),
                    ty: value_layout.clone(),
                };

                

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthMapView(
                        Box::new(key_item),
                        Box::new(value_item),
                    ),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::MapStorage(key_type, value_type, logical_limit) => {
                // Layout key type
                let key_layout = self.layout(key_type.clone());
                let _ = self.id_to_layout.insert(key_type.id, key_layout.clone());

                // Layout value type
                let value_layout = self.layout(value_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(value_type.id, value_layout.clone());

                let key_item = OffsetMemoryItem {
                    offset: MemoryOffset(0),
                    size: key_layout.total_size,
                    name: "key".to_string(),
                    ty: key_layout.clone(),
                };

                let value_offset = align_to(
                    MemoryOffset(key_layout.total_size.0),
                    value_layout.max_alignment,
                );

                let value_item = OffsetMemoryItem {
                    offset: value_offset,
                    size: value_layout.total_size,
                    name: "value".to_string(),
                    ty: value_layout.clone(),
                };

                let tuple_fields = vec![key_item, value_item];

                let tuple_size = value_offset.0 + value_layout.total_size.0;
                let tuple_alignment = max(key_layout.max_alignment, value_layout.max_alignment);
                let aligned_tuple_size =
                    adjust_size_to_alignment(MemorySize(tuple_size), tuple_alignment);

                // Create a tuple type for the key-value pair
                let tuple_type = TupleType {
                    fields: tuple_fields,
                    total_size: aligned_tuple_size,
                    max_alignment: tuple_alignment,
                };

                // Create the tuple type using the layout_tuple method
                // This will handle proper deduplication and caching
                let tuple_types = vec![key_type.clone(), value_type.clone()];
                let tuple_type_id =
                    TypeId::new(key_type.id.inner().wrapping_add(value_type.id.inner()));

                // Use layout_tuple to ensure consistent handling of tuple types
                let tuple_basic_type = self.layout_tuple(&tuple_types, tuple_type_id);

                // Make sure we're also storing the tuple type in the id_to_layout map
                let _ = self
                    .id_to_layout
                    .insert(tuple_type_id, tuple_basic_type);

                let logical_limit = *logical_limit;
                let actual_capacity = (logical_limit as u16).next_power_of_two();

                

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::MapStorage {
                        element_type: value_layout,
                        tuple_type: Box::new(tuple_type),
                        logical_limit,
                        capacity: CountU16(actual_capacity),
                        tuple_alignment,
                        bucket_size: aligned_tuple_size,
                    },
                    total_size: MemorySize(
                        MAP_HEADER_SIZE.0 + aligned_tuple_size.0 * u32::from(actual_capacity),
                    ),
                    max_alignment: max(MAP_HEADER_ALIGNMENT, tuple_alignment),
                })
            }
            TypeKind::Range(range_struct) => create_basic_type(
                ty.id,
                BasicTypeKind::InternalRangeHeader,
                MemorySize(12),
                MemoryAlignment::U32,
            ),
            TypeKind::MutableReference(inner_type) => self.layout_mutable_reference(inner_type),
            TypeKind::GridView(element_type) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::GridView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::GridStorage(element_type, width, height) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                let element_size = element_layout.total_size;
                let element_alignment = element_layout.max_alignment;

                

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::GridStorage(element_layout, *width, *height),
                    total_size: MemorySize(
                        GRID_HEADER_SIZE.0 + element_size.0 * (*width as u32) * (*height as u32),
                    ),
                    max_alignment: max(GRID_HEADER_ALIGNMENT, element_alignment),
                })
            }

            TypeKind::SliceView(element_type) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SliceView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::SparseStorage(element_type, capacity) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SparseStorage(element_layout, *capacity),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }

            TypeKind::SparseView(element_type) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SparseView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::StackStorage(element_type, capacity) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::StackStorage(element_layout, *capacity),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::StackView(element_type) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthVecView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::QueueStorage(element_type, capacity) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::QueueStorage(element_layout, *capacity),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::QueueView(element_type) => {
                let element_layout = self.layout(element_type.clone());
                let _ = self
                    .id_to_layout
                    .insert(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthVecView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }
            TypeKind::Function(_) => {
                panic!("function types can not be laid out")
            }
            TypeKind::Unit => create_basic_type(
                ty.id,
                BasicTypeKind::Empty,
                MemorySize(0),
                MemoryAlignment::U8,
            ),
        };

        // Store in both caches
        let _ = self.id_to_layout.insert(ty.id, basic_type.clone());
        let _ = self
            .kind_to_layout
            .insert((*ty.kind).clone(), basic_type.clone());

        basic_type
    }

    fn layout_mutable_reference(&mut self, analyzed_type: &TypeRef) -> BasicTypeRef {
        let inner_type = self.layout_type(analyzed_type);
        Rc::new(BasicType {
            id: BasicTypeId(inner_type.id.0), // TODO: Id must be correct
            total_size: inner_type.total_size,
            max_alignment: inner_type.max_alignment,
            kind: BasicTypeKind::MutablePointer(inner_type),
        })
    }

    fn layout_named_struct(
        &mut self,
        named_struct_type: &NamedStructType,
        type_id: TypeId,
    ) -> BasicTypeRef {
        // Check if we already have a layout for this kind
        let struct_kind = TypeKind::NamedStruct(named_struct_type.clone());
        if let Some(existing_layout) = self.kind_to_layout.get(&struct_kind) {
            return existing_layout.clone();
        }

        let inner_struct = self.layout_struct_type(
            &named_struct_type.anon_struct_type,
            &named_struct_type.assigned_name,
        );

        // Use the provided TypeId
        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(type_id.inner()), // Use the provided type ID
            total_size: inner_struct.total_size,
            max_alignment: inner_struct.max_alignment,
            kind: BasicTypeKind::Struct(inner_struct),
        });

        // Store in kind_to_layout for future deduplication
        let _ = self.kind_to_layout.insert(struct_kind, basic_type.clone());

        basic_type
    }

    #[must_use]
    pub fn layout_struct_type(
        &mut self,
        struct_type: &AnonymousStructType,
        name: &str,
    ) -> StructType {
        let mut offset = MemoryOffset(0);
        let mut max_alignment = MemoryAlignment::U8;
        let mut items = Vec::with_capacity(struct_type.field_name_sorted_fields.len());

        for (field_name, field_type) in &struct_type.field_name_sorted_fields {
            // Use layout instead of layout_type to ensure proper caching
            let field_layout = self.layout(field_type.field_type.clone());
            check_type_size(&field_layout, &format!("field {name}::{field_name}"));

            // Make sure the field type is in the id_to_layout map
            let _ = self
                .id_to_layout
                .insert(field_type.field_type.id, field_layout.clone());

            offset = align_to(offset, field_layout.max_alignment);

            items.push(OffsetMemoryItem {
                offset,
                size: field_layout.total_size,
                name: field_name.clone(),
                ty: field_layout.clone(),
            });

            offset = offset + field_layout.total_size;

            if field_layout.max_alignment > max_alignment {
                max_alignment = field_layout.max_alignment;
            }
        }

        let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

        StructType {
            name: name.to_string(),
            fields: items,
            total_size,
            max_alignment,
        }
    }

    pub fn layout_struct(
        &mut self,
        struct_type: &AnonymousStructType,
        name: &str,
        type_id: TypeId,
    ) -> BasicTypeRef {
        // Always process each field's type to ensure it's in the cache
        for (_field_name, struct_field) in &struct_type.field_name_sorted_fields {
            let field_type = &struct_field.field_type;
            let _field_layout = self.layout(field_type.clone());

            // The layout method already handles storing in both caches
        }

        // Check if we already have a layout for this kind
        let struct_kind = TypeKind::AnonymousStruct(struct_type.clone());
        if let Some(existing_layout) = self.kind_to_layout.get(&struct_kind) {
            // Store the mapping from this type ID to the existing layout
            let _ = self.id_to_layout.insert(type_id, existing_layout.clone());
            return existing_layout.clone();
        }

        let struct_layout = self.layout_struct_type(struct_type, name);

        // Use the provided type ID
        let struct_id = type_id;

        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(struct_id.inner()),
            total_size: struct_layout.total_size,
            max_alignment: struct_layout.max_alignment,
            kind: BasicTypeKind::Struct(struct_layout),
        });

        // Store in both caches
        let _ = self.id_to_layout.insert(struct_id, basic_type.clone());
        let _ = self.kind_to_layout.insert(struct_kind, basic_type.clone());

        basic_type
    }

    #[must_use]
    pub fn layout_optional_type(&mut self, inner_type: &TypeRef, type_id: TypeId) -> BasicTypeRef {
        // Check if we already have a layout for this kind
        let optional_kind = TypeKind::Optional(inner_type.clone());
        if let Some(existing_layout) = self.kind_to_layout.get(&optional_kind) {
            return existing_layout.clone();
        }

        // Layout the inner type first
        let inner_layout = self.layout(inner_type.clone());

        // Store the inner type in both caches
        let _ = self
            .id_to_layout
            .insert(inner_type.id, inner_layout.clone());
        let _ = self
            .kind_to_layout
            .insert((*inner_type.kind).clone(), inner_layout);

        // Create the optional type
        let optional_union = self.layout_optional_type_items(inner_type);

        // Use the provided type ID
        let optional_id = type_id;

        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(optional_id.inner()),
            total_size: optional_union.total_size,
            max_alignment: optional_union.max_alignment,
            kind: BasicTypeKind::Optional(optional_union),
        });

        // Store in both caches
        let _ = self.id_to_layout.insert(optional_id, basic_type.clone());
        let _ = self
            .kind_to_layout
            .insert(optional_kind, basic_type.clone());

        basic_type
    }

    #[must_use]
    pub fn layout_optional_type_items(&mut self, inner_type: &TypeRef) -> TaggedUnion {
        let gen_type = self.layout_type(inner_type);
        let payload_variant = VariantLayout {
            size: gen_type.total_size,
            alignment: gen_type.max_alignment,
        };
        let none_variant = VariantLayout {
            size: MemorySize(0),
            alignment: MemoryAlignment::U8,
        };
        let tagged = Self::layout_tagged_union(&[none_variant, payload_variant]);

        let payload_tagged_variant = TaggedUnionVariant {
            name: "Some".to_string(),
            ty: gen_type,
        };

        let none_tagged_variant = TaggedUnionVariant {
            name: "None".to_string(),
            ty: Rc::new(BasicType {
                id: BasicTypeId(0),
                kind: BasicTypeKind::Empty,
                total_size: MemorySize(0),
                max_alignment: MemoryAlignment::U8,
            }),
        };

        TaggedUnion {
            name: "option".to_string(),
            tag_offset: tagged.tag_offset,
            tag_size: tagged.tag_size,
            payload_max_size: tagged.payload_size,
            payload_offset: tagged.payload_offset,
            variants: vec![none_tagged_variant, payload_tagged_variant],
            total_size: tagged.total_size,
            max_alignment: tagged.max_alignment,
        }
    }

    #[must_use]
    pub fn layout_tuple_items(&mut self, types: &[TypeRef]) -> TupleType {
        // First pass: Determine maximum alignment requirement
        let mut max_alignment = MemoryAlignment::U8;
        for ty in types {
            // Use layout instead of layout_type to ensure proper caching
            let elem_layout = self.layout(ty.clone());

            // Make sure the element type is in the id_to_layout map
            let _ = self.id_to_layout.insert(ty.id, elem_layout.clone());

            if elem_layout.max_alignment > max_alignment {
                max_alignment = elem_layout.max_alignment;
            }
        }

        // Second pass: Layout fields using the maximum alignment
        let mut offset = MemoryOffset(0);
        let mut items = Vec::with_capacity(types.len());

        for (i, ty) in types.iter().enumerate() {
            // Reuse the layout from the cache
            let elem_layout = self.layout(ty.clone());

            offset = align_to(offset, elem_layout.max_alignment);

            items.push(OffsetMemoryItem {
                offset,
                size: elem_layout.total_size,
                name: i.to_string(),
                ty: elem_layout.clone(),
            });

            offset = offset + elem_layout.total_size;
        }

        // Ensure total size is aligned to max_alignment
        let total_size = adjust_size_to_alignment(offset.as_size(), max_alignment);

        TupleType {
            fields: items,
            total_size,
            max_alignment,
        }
    }
    #[must_use]
    pub fn layout_tuple(&mut self, types: &[TypeRef], tuple_id: TypeId) -> BasicTypeRef {
        // Always process each inner type to ensure it's in the cache
        for ty in types {
            let inner_layout = self.layout(ty.clone());
            let _ = self.id_to_layout.insert(ty.id, inner_layout.clone());
            let _ = self
                .kind_to_layout
                .insert((*ty.kind).clone(), inner_layout.clone());
        }

        // Check if we already have a layout for this kind
        let tuple_kind = TypeKind::Tuple(types.to_vec());
        if let Some(existing_layout) = self.kind_to_layout.get(&tuple_kind) {
            // Store the mapping from this type ID to the existing layout
            let _ = self.id_to_layout.insert(tuple_id, existing_layout.clone());
            return existing_layout.clone();
        }

        let tuple_layout = self.layout_tuple_items(types);

        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(tuple_id.inner()),
            total_size: tuple_layout.total_size,
            max_alignment: tuple_layout.max_alignment,
            kind: BasicTypeKind::Tuple(tuple_layout),
        });

        // Store in both caches
        let _ = self.id_to_layout.insert(tuple_id, basic_type.clone());
        let _ = self.kind_to_layout.insert(tuple_kind, basic_type.clone());

        basic_type
    }
}

fn create_basic_type(
    type_id: TypeId,
    kind: BasicTypeKind,
    size: MemorySize,
    alignment: MemoryAlignment,
) -> BasicTypeRef {
    let basic_type_id = BasicTypeId(type_id.inner());

    Rc::new(BasicType {
        id: basic_type_id,
        kind,
        total_size: size,
        max_alignment: alignment,
    })
}

const fn check_type_size(ty: &BasicType, _comment: &str) {
    if ty.total_size.0 > 128 * 1024 {
        //warn!(size=%ty.total_size,%ty, comment, "this is too much");
    }
}
