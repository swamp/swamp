/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
//! Layouts analyzed into Vm Types (`BasicType`)

use seq_map::SeqMap;
use std::cmp::max;
use std::rc::Rc;
use swamp_symbol::TopLevelSymbolId;
use swamp_types::prelude::{AnonymousStructType, EnumType, EnumVariantType, NamedStructType};
use swamp_types::{TypeId, TypeKind, TypeRef};
use swamp_vm_isa::{
    ANY_HEADER_ALIGNMENT, ANY_HEADER_SIZE, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE,
    MAP_HEADER_ALIGNMENT, MemoryAlignment, MemoryOffset, MemorySize, PTR_ALIGNMENT, PTR_SIZE,
    STRING_PTR_ALIGNMENT, STRING_PTR_SIZE, VEC_HEADER_ALIGNMENT, VEC_HEADER_SIZE,
};
use swamp_vm_types::types::{
    BasicType, BasicTypeId, BasicTypeKind, BasicTypeRef, OffsetMemoryItem, StructType, TaggedUnion,
    TaggedUnionVariant, TupleType,
};
use swamp_vm_types::{CountU16, adjust_size_to_alignment, align_to};

#[derive(Clone)]
pub struct LayoutCache {
    pub id_to_layout: SeqMap<TypeId, BasicTypeRef>,
    pub kind_to_layout: SeqMap<TypeKind, BasicTypeRef>,
    pub universal_id_to_layout: SeqMap<u64, BasicTypeRef>,
    pub universal_short_id_to_layout: SeqMap<u32, BasicTypeRef>,
}

impl LayoutCache {}

impl Default for LayoutCache {
    fn default() -> Self {
        Self::new()
    }
}

impl LayoutCache {
    #[must_use]
    pub fn new() -> Self {
        Self {
            id_to_layout: SeqMap::default(),
            kind_to_layout: SeqMap::default(),
            universal_id_to_layout: SeqMap::default(),
            universal_short_id_to_layout: SeqMap::default(),
        }
    }

    #[must_use]
    pub fn layout(&mut self, analyzed_type: &TypeRef) -> BasicTypeRef {
        // First check if we already have a layout for this type ID
        if let Some(x) = self.id_to_layout.get(&analyzed_type.id) {
            return x.clone();
        }

        // Check if we already have a layout for this kind of type
        if let Some(existing_layout) = self.kind_to_layout.get(&analyzed_type.kind).cloned() {
            // For deduplication, we reuse the existing layout directly
            // This ensures pointer equality for structurally identical types
            self.insert_layout(analyzed_type.id, existing_layout.clone());
            return existing_layout;
        }

        let basic_type = self.layout_type(analyzed_type);

        self.insert_layout(analyzed_type.id, basic_type.clone());

        // Also store in kind_to_layout for future deduplication
        let _ = self
            .kind_to_layout
            .insert((*analyzed_type.kind).clone(), basic_type.clone());

        basic_type
    }

    #[must_use]
    fn layout_tagged_union(variants: &[TaggedUnionVariant], name: &str) -> TaggedUnion {
        let num_variants = variants.len();
        let (tag_size, tag_alignment) = if num_variants <= 0xFF {
            (MemorySize(1), MemoryAlignment::U8)
        } else if num_variants <= 0xFFFF {
            (MemorySize(2), MemoryAlignment::U16)
        } else {
            (MemorySize(4), MemoryAlignment::U32)
        };

        // First pass: Find maximum payload size and alignment
        let max_payload_size = variants
            .iter()
            .map(|v| v.ty.total_size)
            .max()
            .unwrap_or(MemorySize(0));
        let max_payload_alignment = variants
            .iter()
            .map(|v| v.ty.max_alignment)
            .max()
            .unwrap_or(MemoryAlignment::U8);

        // Second pass: Layout using maximum alignment
        let max_alignment = max(tag_alignment, max_payload_alignment);
        let payload_offset = align_to(MemoryOffset(tag_size.0), max_payload_alignment);

        let complete_size_before_alignment = MemorySize(payload_offset.0 + max_payload_size.0);
        let total_size = adjust_size_to_alignment(complete_size_before_alignment, max_alignment);

        TaggedUnion {
            name: name.to_string(),
            tag_offset: MemoryOffset(0),
            tag_size,
            tag_alignment,
            payload_offset,
            payload_max_size: max_payload_size,
            max_payload_alignment,
            total_size,
            max_alignment,
            variants: variants.to_vec(),
        }
    }

    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn layout_enum_into_tagged_union(
        &mut self,
        name: &str,
        v: &[EnumVariantType],
    ) -> TaggedUnion {
        let variant_layouts: Vec<_> = v
            .iter()
            .map(|variant| {
                let gen_payload_type = self.layout_type(&variant.payload_type);

                TaggedUnionVariant {
                    name: variant.common.assigned_name.clone(),
                    ty: gen_payload_type,
                }
            })
            .collect();

        Self::layout_tagged_union(&variant_layouts, name)
    }

    #[must_use]
    pub fn layout_enum(
        &mut self,
        name: &str,
        variants: &[EnumVariantType],
        type_id: TypeId,
        symbol_id: TopLevelSymbolId,
    ) -> BasicTypeRef {
        // Check if we already have a layout for this kind
        let enum_kind = TypeKind::Enum(EnumType::new(
            source_map_node::Node::default(),
            name,
            symbol_id,
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

    #[must_use]
    fn layout_vec_like(
        &mut self,
        element_type: &TypeRef,
        capacity: usize,
    ) -> (BasicTypeRef, MemorySize, MemoryAlignment) {
        let element_type_basic = self.layout_type(element_type);
        let (mem_size, mem_alignment) =
            self.layout_vec_like_from_basic(&element_type_basic, capacity);

        (element_type_basic, mem_size, mem_alignment)
    }

    #[must_use]
    fn layout_vec_like_from_basic(
        &mut self,
        element_type_basic: &BasicTypeRef,
        capacity: usize,
    ) -> (MemorySize, MemoryAlignment) {
        let total_size =
            element_type_basic.total_size.0 as usize * capacity + VEC_HEADER_SIZE.0 as usize;
        let max_alignment = max(element_type_basic.max_alignment, MemoryAlignment::U32);

        (MemorySize(total_size as u32), max_alignment)
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
            // structural types, but we still have separate entries for each TypeId
            let new_basic_type = Rc::new(BasicType {
                id: BasicTypeId(ty.id.inner()),
                total_size: existing_layout.total_size,
                max_alignment: existing_layout.max_alignment,
                kind: existing_layout.kind.clone(),
            });

            // Store the mapping from this type ID to the new layout
            self.insert_layout(ty.id, new_basic_type.clone());

            // For nested types, we need to properly track all component types
            // This is essential for the deduplication to work correctly
            match &*ty.kind {
                TypeKind::AnonymousStruct(struct_type) => {
                    // Process each field type
                    for field in struct_type.field_name_sorted_fields.values() {
                        let field_type = &field.field_type;
                        let field_layout = self.layout(field_type);
                        let _ = self.id_to_layout.insert(field_type.id, field_layout);
                    }
                }

                TypeKind::NamedStruct(named_struct) => {
                    // Process each field type in the anonymous struct
                    if let TypeKind::AnonymousStruct(anon_struct) =
                        &*named_struct.anon_struct_type.kind
                    {
                        for field in anon_struct.field_name_sorted_fields.values() {
                            let field_type = &field.field_type;
                            let field_layout = self.layout(field_type);
                            self.insert_layout(field_type.id, field_layout);
                        }
                    }
                }

                TypeKind::Tuple(tuple_types) => {
                    // Process each tuple element type
                    for elem_type in tuple_types {
                        let elem_layout = self.layout(elem_type);
                        self.insert_layout(elem_type.id, elem_layout);
                    }
                }
                _ => {}
            }

            return new_basic_type;
        }

        let basic_type = match &*ty.kind {
            TypeKind::Never => {
                create_basic_type(ty.id, BasicTypeKind::U8, MemorySize(0), MemoryAlignment::U8)
            }
            TypeKind::Pointer(_) => create_basic_type(
                ty.id,
                BasicTypeKind::Pointer,
                MemorySize(4),
                MemoryAlignment::U32,
            ),
            TypeKind::Byte => {
                create_basic_type(ty.id, BasicTypeKind::U8, MemorySize(1), MemoryAlignment::U8)
            }
            TypeKind::Codepoint => create_basic_type(
                ty.id,
                BasicTypeKind::U32,
                MemorySize(4),
                MemoryAlignment::U32,
            ),
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

            TypeKind::StringView(byte, char) => create_basic_type(
                ty.id,
                BasicTypeKind::StringView {
                    byte: self.layout(byte),
                    char: self.layout(char),
                },
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
                self.layout_enum(
                    &enum_type.assigned_name,
                    &variants,
                    ty.id,
                    enum_type.symbol_id,
                )
            }

            TypeKind::FixedCapacityAndLengthArray(element_type, capacity) => {
                let (element_layout, total_size, max_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let array_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::FixedCapacityArray(element_layout.clone(), *capacity),
                    total_size,
                    max_alignment,
                });

                // Also store the element type in id_to_layout
                self.insert_layout(element_type.id, element_layout);

                array_type
            }

            TypeKind::StringStorage(byte_type, char_type, capacity) => {
                let (element_layout, total_size, max_alignment) =
                    self.layout_vec_like(byte_type, *capacity);

                let array_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::StringStorage {
                        element_type: element_layout.clone(),
                        char: self.layout(char_type),
                        capacity: *capacity,
                    },
                    total_size,
                    max_alignment,
                });

                // Also store the element type in id_to_layout
                self.insert_layout(byte_type.id, element_layout);

                array_type
            }

            TypeKind::Any => create_basic_type(
                ty.id,
                BasicTypeKind::Any,
                ANY_HEADER_SIZE,
                ANY_HEADER_ALIGNMENT,
            ),

            TypeKind::DynamicLengthVecView(element_type) => {
                let (element_layout, _, _) = self.layout_vec_like(element_type, 0);

                let vec_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthVecView(element_layout.clone()),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                });

                self.insert_layout(element_type.id, element_layout);

                vec_type
            }

            TypeKind::VecStorage(element_type, capacity) => {
                let (element_layout, total_size, element_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let storage_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::VecStorage(element_layout.clone(), *capacity),
                    total_size,
                    max_alignment: max(VEC_HEADER_ALIGNMENT, element_alignment),
                });

                self.insert_layout(element_type.id, element_layout);

                storage_type
            }

            TypeKind::DynamicLengthMapView(key_type, value_type) => {
                // Layout key type
                let key_layout = self.layout(key_type);
                self.insert_layout(key_type.id, key_layout.clone());

                // Layout value type
                let value_layout = self.layout(value_type);
                self.insert_layout(value_type.id, value_layout.clone());

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
                let key_layout = self.layout(key_type);
                self.insert_layout(key_type.id, key_layout.clone());

                // Layout value type
                let value_layout = self.layout(value_type);
                self.insert_layout(value_type.id, value_layout.clone());

                let logical_limit = *logical_limit;

                let (_bucket_layout, map_init) = hashmap_mem::layout(
                    key_layout.total_size.0,
                    key_layout.max_alignment.into(),
                    value_layout.total_size.0,
                    value_layout.max_alignment.into(),
                    logical_limit as u16,
                );
                let total_size = MemorySize(map_init.total_size);

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::MapStorage {
                        key_type: key_layout,
                        logical_limit,
                        capacity: CountU16(map_init.capacity),
                        value_type: value_layout,
                    },
                    total_size,
                    max_alignment: MAP_HEADER_ALIGNMENT,
                })
            }

            TypeKind::Range(_range_struct) => create_basic_type(
                ty.id,
                BasicTypeKind::InternalRangeHeader,
                MemorySize(12),
                MemoryAlignment::U32,
            ),

            TypeKind::GridView(element_type) => {
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::GridView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }

            TypeKind::GridStorage(element_type, width, height) => {
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

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
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SliceView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }

            TypeKind::SparseStorage(element_type, capacity) => {
                let element_layout = self.layout(element_type);
                let size = sparse_mem::layout_size(*capacity as u16, element_layout.total_size.0);

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SparseStorage(element_layout, *capacity),
                    total_size: MemorySize(size as u32),
                    max_alignment: MemoryAlignment::U64,
                })
            }

            TypeKind::SparseView(element_type) => {
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::SparseView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }

            TypeKind::StackStorage(element_type, capacity) => {
                let (element_layout, total_size, _max_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let storage_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::StackStorage(element_layout.clone(), *capacity),
                    total_size,
                    max_alignment: VEC_HEADER_ALIGNMENT,
                });

                self.insert_layout(element_type.id, element_layout);

                storage_type
            }

            TypeKind::StackView(element_type) => {
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

                Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::DynamicLengthVecView(element_layout),
                    total_size: PTR_SIZE,
                    max_alignment: PTR_ALIGNMENT,
                })
            }

            TypeKind::QueueStorage(element_type, capacity) => {
                let (element_layout, total_size, _max_alignment) =
                    self.layout_vec_like(element_type, *capacity);

                let storage_type = Rc::new(BasicType {
                    id: BasicTypeId(ty.id.inner()),
                    kind: BasicTypeKind::QueueStorage(element_layout.clone(), *capacity),
                    total_size,
                    max_alignment: VEC_HEADER_ALIGNMENT,
                });

                self.insert_layout(element_type.id, element_layout);

                storage_type
            }

            TypeKind::QueueView(element_type) => {
                let element_layout = self.layout(element_type);
                self.insert_layout(element_type.id, element_layout.clone());

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
        self.insert_layout(ty.id, basic_type.clone());
        let _ = self
            .kind_to_layout
            .insert((*ty.kind).clone(), basic_type.clone());

        basic_type
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

        // Extract AnonymousStructType from the TypeRef
        let anon_struct = match &*named_struct_type.anon_struct_type.kind {
            TypeKind::AnonymousStruct(anon_struct) => anon_struct,
            _ => panic!("Expected AnonymousStruct in NamedStructType"),
        };

        let inner_struct = self.layout_struct_type(anon_struct, &named_struct_type.assigned_name);

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
        // First pass: Layout all field types and determine maximum alignment requirement
        let mut field_layouts = Vec::with_capacity(struct_type.field_name_sorted_fields.len());
        let mut max_alignment = MemoryAlignment::U8;

        for (field_name, field_type) in &struct_type.field_name_sorted_fields {
            // Use layout instead of layout_type to ensure proper caching
            let field_layout = self.layout(&field_type.field_type);
            check_type_size(&field_layout, &format!("field {name}::{field_name}"));

            // Make sure the field type is in the id_to_layout map
            self.insert_layout(field_type.field_type.id, field_layout.clone());

            if field_layout.max_alignment > max_alignment {
                max_alignment = field_layout.max_alignment;
            }

            field_layouts.push((field_name.clone(), field_layout));
        }

        // Second pass: Layout fields using the maximum alignment
        let mut offset = MemoryOffset(0);
        let mut items = Vec::with_capacity(field_layouts.len());

        for (field_name, field_layout) in field_layouts {
            offset = align_to(offset, field_layout.max_alignment);

            items.push(OffsetMemoryItem {
                offset,
                size: field_layout.total_size,
                name: field_name,
                ty: field_layout.clone(),
            });

            offset = offset + field_layout.total_size;
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
            let _field_layout = self.layout(field_type);

            // The layout method already handles storing in both caches
        }

        // Check if we already have a layout for this kind
        let struct_kind = TypeKind::AnonymousStruct(struct_type.clone());
        if let Some(existing_layout) = self.kind_to_layout.get(&struct_kind).cloned() {
            // Store the mapping from this type ID to the existing layout
            self.insert_layout(type_id, existing_layout.clone());
            return existing_layout;
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
        self.insert_layout(struct_id, basic_type.clone());
        let _ = self.kind_to_layout.insert(struct_kind, basic_type.clone());

        basic_type
    }

    #[must_use]
    pub fn layout_optional_type(&mut self, inner_type: &TypeRef, type_id: TypeId) -> BasicTypeRef {
        // Check if we already have a layout for this kind
        let optional_kind = TypeKind::Optional(inner_type.clone());
        if let Some(existing_layout) = self.kind_to_layout.get(&optional_kind) {
            assert!(matches!(&existing_layout.kind, BasicTypeKind::Optional(_)));
            return existing_layout.clone();
        }

        // Layout the inner type first
        let inner_layout = self.layout(inner_type);

        // Store the inner type in both caches
        self.insert_layout(inner_type.id, inner_layout.clone());
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
        self.insert_layout(optional_id, basic_type.clone());
        let _ = self
            .kind_to_layout
            .insert(optional_kind, basic_type.clone());

        basic_type
    }

    #[must_use]
    pub fn layout_optional_type_items(&mut self, inner_type: &TypeRef) -> TaggedUnion {
        let gen_type = self.layout_type(inner_type);

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

        Self::layout_tagged_union(&[none_tagged_variant, payload_tagged_variant], "Maybe")
    }

    #[must_use]
    pub fn layout_tuple_items(&mut self, types: &[TypeRef]) -> TupleType {
        // First pass: Determine maximum alignment requirement
        let mut max_alignment = MemoryAlignment::U8;
        for ty in types {
            // Use layout instead of layout_type to ensure proper caching
            let elem_layout = self.layout(ty);

            // Make sure the element type is in the id_to_layout map
            self.insert_layout(ty.id, elem_layout.clone());

            if elem_layout.max_alignment > max_alignment {
                max_alignment = elem_layout.max_alignment;
            }
        }

        // Second pass: Layout fields using the maximum alignment
        let mut offset = MemoryOffset(0);
        let mut items = Vec::with_capacity(types.len());

        for (i, ty) in types.iter().enumerate() {
            // Reuse the layout from the cache
            let elem_layout = self.layout(ty);

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
            let inner_layout = self.layout(ty);
            self.insert_layout(ty.id, inner_layout.clone());
            let _ = self
                .kind_to_layout
                .insert((*ty.kind).clone(), inner_layout.clone());
        }

        // Check if we already have a layout for this kind
        let tuple_kind = TypeKind::Tuple(types.to_vec());
        if let Some(existing_layout) = self.kind_to_layout.get(&tuple_kind).cloned() {
            // Store the mapping from this type ID to the existing layout
            self.insert_layout(tuple_id, existing_layout.clone());
            return existing_layout;
        }

        let tuple_layout = self.layout_tuple_items(types);

        let basic_type = Rc::new(BasicType {
            id: BasicTypeId(tuple_id.inner()),
            total_size: tuple_layout.total_size,
            max_alignment: tuple_layout.max_alignment,
            kind: BasicTypeKind::Tuple(tuple_layout),
        });

        // Store in both caches
        self.insert_layout(tuple_id, basic_type.clone());
        let _ = self.kind_to_layout.insert(tuple_kind, basic_type.clone());

        basic_type
    }

    /// Helper method to insert a layout into all relevant maps
    fn insert_layout(&mut self, type_id: TypeId, layout: BasicTypeRef) {
        let _ = self.id_to_layout.insert(type_id, layout.clone());

        // Calculate universal hashes and store them
        let universal_hash = layout.universal_hash_u64();
        let _ = self
            .universal_id_to_layout
            .insert(universal_hash, layout.clone());
        let _ = self
            .universal_short_id_to_layout
            .insert(universal_hash as u32, layout);
    }

    #[must_use]
    pub fn universal_short_id(&self, short_id: u32) -> &BasicTypeRef {
        if let Some(x) = self.universal_short_id_to_layout.get(&short_id) {
            x
        } else {
            for (hash, ty) in &self.universal_short_id_to_layout {
                eprintln!("{hash:X} ({hash}) {}", ty.kind);
            }
            panic!("not found")
        }
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

pub const fn check_type_size(ty: &BasicType, _comment: &str) {
    if ty.total_size.0 > 1024 * 1024 {
        //eprintln!("suspicious allocation: {} for {ty}", ty.total_size);
    }
}
