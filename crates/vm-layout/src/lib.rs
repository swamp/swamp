/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

//! Layouts analyzed into Vm Types (`BasicType`)
use seq_map::SeqMap;
use std::cmp::max;
use std::rc::Rc;
use swamp_types::prelude::{AnonymousStructType, EnumVariantType, NamedStructType};
use swamp_types::{TypeId, TypeKind, TypeRef};
use swamp_vm_types::aligner::align;
use swamp_vm_types::types::{range_type, BasicType, BasicTypeId, BasicTypeKind, BasicTypeRef, OffsetMemoryItem, StructType, TaggedUnion, TaggedUnionVariant, TupleType};
use swamp_vm_types::{
    adjust_size_to_alignment, align_to, CountU16,
    MemoryAlignment, MemoryOffset, MemorySize, GRID_HEADER_ALIGNMENT, GRID_HEADER_SIZE,
    MAP_HEADER_ALIGNMENT, MAP_HEADER_SIZE, PTR_ALIGNMENT, PTR_SIZE, STRING_PTR_ALIGNMENT,
    STRING_PTR_SIZE, VEC_HEADER_SIZE,
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
}

impl LayoutCache {
    pub fn new() -> Self {
        Self {
            id_to_layout: SeqMap::default(),
        }
    }
    pub fn layout(&mut self, analyzed_type: TypeRef) -> BasicTypeRef {
        if let Some(x) = self.id_to_layout.get(&analyzed_type.id) {
            x.clone()
        } else {
            let basic_type = self.layout_type(&analyzed_type);

            self.id_to_layout.insert(analyzed_type.id, basic_type.clone()).expect("should work");

            basic_type
        }
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
    pub fn layout_enum_into_tagged_union(&mut self, name: &str, variants: &[EnumVariantType]) -> TaggedUnion {
        let variant_infos = variants.iter().map(|variant| match variant {
            EnumVariantType::Struct(s) => {
                let struct_type = self.layout_struct_type(&s.anon_struct, &s.common.assigned_name);
                (
                    VariantLayout {
                        size: struct_type.total_size,
                        alignment: struct_type.max_alignment,
                    },
                    TaggedUnionVariant {
                        name: s.common.assigned_name.clone(),
                        ty: BasicType {
                            total_size: struct_type.total_size,
                            max_alignment: struct_type.max_alignment,
                            kind: BasicTypeKind::Struct(struct_type),
                        },
                    },
                )
            }
            EnumVariantType::Tuple(t) => {
                let tuple_type = self.layout_tuple_items(&t.fields_in_order);
                (
                    VariantLayout {
                        size: tuple_type.total_size,
                        alignment: tuple_type.max_alignment,
                    },
                    TaggedUnionVariant {
                        name: t.common.assigned_name.clone(),
                        ty: BasicType {
                            total_size: tuple_type.total_size,
                            max_alignment: tuple_type.max_alignment,
                            kind: BasicTypeKind::Tuple(tuple_type),
                        },
                    },
                )
            }

            EnumVariantType::Nothing(n) => (
                VariantLayout {
                    size: MemorySize(0),
                    alignment: MemoryAlignment::U8,
                },
                TaggedUnionVariant {
                    name: n.common.assigned_name.clone(),
                    ty: BasicType {
                        total_size: MemorySize(0),
                        max_alignment: MemoryAlignment::U8,
                        kind: BasicTypeKind::Empty,
                    },
                },
            ),
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
    pub fn layout_enum(&mut self, name: &str, variants: &[EnumVariantType]) -> BasicTypeRef {
        let tagged_union = self.layout_enum_into_tagged_union(name, variants);

        Rc::new(BasicType {
            id: TypeId::new(TypeId::EMPTY),
            total_size: tagged_union.total_size,
            max_alignment: tagged_union.max_alignment,
            kind: BasicTypeKind::TaggedUnion(tagged_union),
        })
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
        match &*ty.kind {
            TypeKind::Int => create_basic_type(ty.id, BasicTypeKind::S32, MemorySize(4), MemoryAlignment::U32),
            TypeKind::Float => {
                create_basic_type(ty.id, BasicTypeKind::Fixed32, MemorySize(4), MemoryAlignment::U32)
            }
            TypeKind::Bool => create_basic_type(ty.id, BasicTypeKind::B8, MemorySize(1), MemoryAlignment::U8),
            TypeKind::Unit => create_basic_type(ty.id, BasicTypeKind::Empty, MemorySize(0), MemoryAlignment::U8),
            TypeKind::String => create_basic_type(ty.id,
                                                  BasicTypeKind::InternalStringPointer,
                                                  STRING_PTR_SIZE,
                                                  STRING_PTR_ALIGNMENT,
            ),
            TypeKind::Range(_) => range_type(),
            TypeKind::SliceView(element_type) => {
                let element_type_basic = self.layout_type(element_type);
                create_basic_type(
                    ty.id,
                    BasicTypeKind::SliceView(element_type_basic),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }

            // Fixed Capacity Array and Vec Storage are the same when it comes to layout
            TypeKind::FixedCapacityAndLengthArray(element_type, fixed_size_element_count) => {
                let element_type_basic = self.layout_type(element_type);
                let total_size = element_type_basic.total_size.0 as usize * fixed_size_element_count
                    + VEC_HEADER_SIZE.0 as usize;
                let max_alignment = max(element_type_basic.max_alignment, MemoryAlignment::U16);
                create_basic_type(
                    element_type.id,
                    BasicTypeKind::FixedCapacityArray(
                        element_type_basic,
                        *fixed_size_element_count,
                    ),
                    MemorySize(total_size as u32),
                    max_alignment,
                )
            }

            TypeKind::DynamicLengthMapView(key_type, element_type) => {
                let tuple_gen_type = self.layout_tuple_items(&[*key_type.clone(), *element_type.clone()]);
                create_basic_type(
                    ty.id,
                    BasicTypeKind::DynamicLengthMapView(
                        Box::from(tuple_gen_type.fields[0].clone()),
                        Box::from(tuple_gen_type.fields[1].clone()),
                    ),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }
            TypeKind::SparseStorage(value_type, capacity) => {
                let generator_value = self.layout_type(value_type);
                let total_size =
                    sparse_mem::layout_size(*capacity as u16, generator_value.total_size.0);

                create_basic_type(
                    ty.id,
                    BasicTypeKind::SparseStorage(generator_value, *capacity),
                    MemorySize(total_size as u32),
                    sparse_mem::alignment().try_into().unwrap(),
                )
            }

            TypeKind::GridStorage(element_type, width, height) => {
                let layout_element = self.layout_type(element_type);

                let total_capacity_byte_count = width * height * (layout_element.total_size.0 as usize);

                let max_alignment = max(layout_element.max_alignment, GRID_HEADER_ALIGNMENT);
                let total_size = MemorySize(GRID_HEADER_SIZE.0 + total_capacity_byte_count as u32);

                create_basic_type(
                    ty.id,
                    BasicTypeKind::GridStorage(layout_element, *width, *height),
                    total_size,
                    max_alignment,
                )
            }

            TypeKind::MapStorage(key_type, value_type, logical_size) => {
                let key_layout = self.layout_type(key_type);
                let value_layout = self.layout_type(value_type);

                let status_size: u32 = 1;
                let mut current_offset = status_size;

                let key_offset = align(current_offset as usize, key_layout.max_alignment.into());
                current_offset = key_offset as u32 + key_layout.total_size.0;

                let value_offset = align(current_offset as usize, value_layout.max_alignment.into());
                current_offset = value_offset as u32 + value_layout.total_size.0;
                let bucket_content_alignment =
                    max(key_layout.max_alignment, value_layout.max_alignment);

                let bucket_size = align(current_offset as usize, bucket_content_alignment.into());

                let max_alignment = max(bucket_content_alignment, MAP_HEADER_ALIGNMENT);

                let capacity = (*logical_size).max(1).next_power_of_two() as u16;
                let total_size = (bucket_size * capacity as usize) + MAP_HEADER_SIZE.0 as usize;

                let tuple_gen_type = self.layout_tuple_items(&[*key_type.clone(), *value_type.clone()]);

                create_basic_type(
                    ty.id,
                    BasicTypeKind::MapStorage {
                        element_type: Rc::from(BasicType {
                            kind: BasicTypeKind::Tuple(tuple_gen_type.clone()),
                            total_size: tuple_gen_type.total_size,
                            max_alignment: tuple_gen_type.max_alignment,
                        }),
                        tuple_type: Rc::from(tuple_gen_type),
                        logical_limit: *logical_size,
                        capacity: CountU16(capacity),
                        tuple_alignment: bucket_content_alignment,
                        bucket_size: MemorySize(bucket_size as u32),
                    },
                    MemorySize(total_size as u32),
                    max_alignment,
                )
            }

            TypeKind::QueueStorage(element_type, fixed_size_element_count) => {
                let (element_type_basic, total_size, max_alignment) =
                    self.layout_vec_like(element_type, *fixed_size_element_count);
                create_basic_type(
                    BasicTypeKind::QueueStorage(
                        element_type_basic,
                        *fixed_size_element_count,
                    ),
                    total_size,
                    max_alignment,
                )
            }

            TypeKind::StackStorage(element_type, fixed_size_element_count) => {
                let (element_type_basic, total_size, max_alignment) =
                    self.layout_vec_like(element_type, *fixed_size_element_count);
                create_basic_type(
                    BasicTypeKind::StackStorage(
                        element_type_basic,
                        *fixed_size_element_count,
                    ),
                    total_size,
                    max_alignment,
                )
            }
            TypeKind::VecStorage(element_type, fixed_size_element_count) => {
                let (element_type_basic, total_size, max_alignment) =
                    self.layout_vec_like(element_type, *fixed_size_element_count);
                create_basic_type(
                    ty.id,
                    BasicTypeKind::VecStorage(element_type_basic, *fixed_size_element_count),
                    total_size,
                    max_alignment,
                )
            }

            TypeKind::QueueView(inner_type)
            | TypeKind::StackView(inner_type)
            | TypeKind::DynamicLengthVecView(inner_type) => {
                let inner_gen_type = self.layout_type(inner_type);
                create_basic_type(
                    ty.id,
                    BasicTypeKind::DynamicLengthVecView(inner_gen_type),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }

            TypeKind::SparseView(inner_type) => {
                let inner_gen_type = self.layout_type(inner_type);
                create_basic_type(
                    inner_type.id,
                    BasicTypeKind::SparseView(inner_gen_type),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }

            TypeKind::GridView(inner_type) => {
                let inner_gen_type = self.layout_type(inner_type);
                create_basic_type(
                    inner_type.id,
                    BasicTypeKind::GridView(inner_gen_type),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }

            TypeKind::StackView(inner_type) => {
                let inner_gen_type = self.layout_type(inner_type);
                create_basic_type(
                    inner_type.id,
                    BasicTypeKind::DynamicLengthVecView(inner_gen_type),
                    PTR_SIZE,
                    PTR_ALIGNMENT,
                )
            }
            TypeKind::Tuple(types) => self.layout_tuple(types),
            TypeKind::NamedStruct(named_struct_type) => {
                self.layout_named_struct(named_struct_type) // NOTE: memory_offset removed
            }
            TypeKind::AnonymousStruct(anon_struct_type) => {
                self.layout_struct(anon_struct_type, "") // NOTE: memory_offset removed
            }
            TypeKind::Enum(a) => self.layout_enum(
                &a.assigned_name,
                &a.variants.values().cloned().collect::<Vec<_>>(),
            ),
            TypeKind::Optional(inner_type) => self.layout_optional_type(inner_type),
            TypeKind::MutableReference(inner_type) => self.layout_mutable_reference(inner_type),
            // ----------
            TypeKind::Function(_) => panic!("function types should not be a part of codegen"),
        }
    }

    fn layout_mutable_reference(&mut self, analyzed_type: &TypeRef) -> BasicTypeRef {
        if analyzed_type.is_primitive() {
            // For primitives, just use the primitive type directly
            // The ABI passes primitives inside the register directly and
            // have a copy back scheme that copies the register back afterward.
            return self.layout_type(analyzed_type);
        }

        let inner_type = self.layout_type(analyzed_type);
        BasicType {
            total_size: inner_type.total_size,
            max_alignment: inner_type.max_alignment,
            kind: BasicTypeKind::MutablePointer(inner_type),
        }
    }

    fn layout_named_struct(&mut self, named_struct_type: &NamedStructType) -> BasicType {
        self.layout_struct(
            &named_struct_type.anon_struct_type,
            //memory_offset,
            &named_struct_type.assigned_name,
        )
    }

    #[must_use]
    pub fn layout_struct_type(&mut self, struct_type: &AnonymousStructType, name: &str) -> StructType {
        let mut offset = MemoryOffset(0);
        let mut max_alignment = MemoryAlignment::U8;
        let mut items = Vec::with_capacity(struct_type.field_name_sorted_fields.len());

        for (field_name, field_type) in &struct_type.field_name_sorted_fields {
            let field_layout = self.layout_type(&field_type.field_type);
            check_type_size(&field_layout, &format!("field {name}::{field_name}"));

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

    #[must_use]
    pub fn layout_struct(&mut self, struct_type: &AnonymousStructType, name: &str) -> BasicTypeRef {
        let inner_struct = self.layout_struct_type(struct_type, name);
        Rc::new(BasicType {
            total_size: inner_struct.total_size,
            max_alignment: inner_struct.max_alignment,
            kind: BasicTypeKind::Struct(inner_struct),
        })
    }

    #[must_use]
    pub fn layout_optional_type(&mut self, inner_type: &TypeRef) -> BasicTypeRef {
        let tagged_union_type = self.layout_optional_type_items(inner_type);
        todo!();
        /*
        BasicType {
            total_size: tagged_union_type.total_size,
            max_alignment: tagged_union_type.max_alignment,
            kind: BasicTypeKind::Optional(tagged_union_type),
        }
        
         */
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
        let tagged = self.layout_tagged_union(&[none_variant, payload_variant]);

        let payload_tagged_variant = TaggedUnionVariant {
            name: "Some".to_string(),
            ty: self.layout_type(inner_type),
        };

        let none_tagged_variant = TaggedUnionVariant {
            name: "None".to_string(),
            ty: BasicType {
                kind: BasicTypeKind::Empty,
                total_size: MemorySize(0),
                max_alignment: MemoryAlignment::U8,
            }.into(),
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
            let elem_layout = self.layout_type(ty);
            if elem_layout.max_alignment > max_alignment {
                max_alignment = elem_layout.max_alignment;
            }
        }

        // Second pass: Layout fields using the maximum alignment
        let mut offset = MemoryOffset(0);
        let mut items = Vec::with_capacity(types.len());

        for (i, ty) in types.iter().enumerate() {
            let elem_layout = self.layout_type(ty);

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
    pub fn layout_tuple(&mut self, types: &[TypeRef]) -> BasicTypeRef {
        let tuple_type = self.layout_tuple_items(types);

        // TODO: Must insert into cache
        Rc::new(BasicType {
            id: BasicTypeId(0),
            total_size: tuple_type.total_size,
            max_alignment: tuple_type.max_alignment,
            kind: BasicTypeKind::Tuple(tuple_type),
        })
    }
}

fn create_basic_type(
    id: TypeId,
    basic_type_kind: BasicTypeKind,
    size: MemorySize,
    alignment: MemoryAlignment,
) -> BasicTypeRef {
    // TODO: Maybe should insert directly into cache
    Rc::new(BasicType {
        id: BasicTypeId(id.inner()),
        kind: basic_type_kind,
        total_size: size,
        max_alignment: alignment,
    })
}

const fn check_type_size(ty: &BasicType, comment: &str) {
    if ty.total_size.0 > 128 * 1024 {
        //warn!(size=%ty.total_size,%ty, comment, "this is too much");
    }
}
