/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Type;
use crate::cache::TypeCache;
use crate::type_kind::TypeKind;

impl Type {
    pub(crate) fn do_compatible_with(&self, other: &Self, type_cache: &mut TypeCache) -> bool {
        if self.id == other.id {
            return true;
        }

        // Since `Never` (!) types never return, then it is safe to be it compatible with anything
        if matches!(&*other.kind, TypeKind::Never) {
            return true;
        }

        // Since `Never` (!) types never return, then it is safe to be it compatible with anything
        if matches!(&*self.kind, TypeKind::Never) {
            return true;
        }

        match (&*self.kind, &*other.kind) {
            // Direct comparisons for primitive types
            (TypeKind::Int, TypeKind::Int) => true,
            (TypeKind::Float, TypeKind::Float) => true,
            (TypeKind::Bool, TypeKind::Bool) => true,
            (TypeKind::Codepoint, TypeKind::Codepoint) => true,
            (TypeKind::StringView(..), TypeKind::StringView(..)) => true,
            (TypeKind::Unit, TypeKind::Unit) => true,

            // For container types, we compare their structure but delegate inner compatibility
            // checks to TypeCache.compatible_with
            (TypeKind::Optional(a), TypeKind::Optional(b)) => type_cache.compatible_with(a, b),

            (TypeKind::VecStorage(a, _), TypeKind::VecStorage(b, _)) => {
                type_cache.compatible_with(a, b)
            }
            (TypeKind::SparseStorage(a, _), TypeKind::SparseStorage(b, _)) => {
                type_cache.compatible_with(a, b)
            }
            (TypeKind::QueueStorage(a, _), TypeKind::QueueStorage(b, _)) => {
                type_cache.compatible_with(a, b)
            }

            (TypeKind::StackStorage(a, _), TypeKind::StackStorage(b, _)) => {
                type_cache.compatible_with(a, b)
            }

            (TypeKind::MapStorage(a_key, a_value, _), TypeKind::MapStorage(b_key, b_value, _)) => {
                type_cache.compatible_with(a_key, b_key)
                    && type_cache.compatible_with(a_value, b_value)
            }

            (
                TypeKind::GridStorage(a, rows_a, cols_a),
                TypeKind::GridStorage(b, rows_b, cols_b),
            ) => rows_a >= rows_b && cols_a >= cols_b && type_cache.compatible_with(a, b),

            (TypeKind::SliceView(a), TypeKind::SliceView(b)) => type_cache.compatible_with(a, b),
            (TypeKind::SparseView(a), TypeKind::SparseView(b)) => type_cache.compatible_with(a, b),
            (TypeKind::QueueView(a), TypeKind::QueueView(b)) => type_cache.compatible_with(a, b),
            (TypeKind::StackView(a), TypeKind::StackView(b)) => type_cache.compatible_with(a, b),
            (TypeKind::DynamicLengthVecView(a), TypeKind::DynamicLengthVecView(b)) => {
                type_cache.compatible_with(a, b)
            }
            (
                TypeKind::DynamicLengthMapView(a_key, a_value),
                TypeKind::DynamicLengthMapView(b_key, b_value),
            ) => {
                type_cache.compatible_with(a_key, b_key)
                    && type_cache.compatible_with(a_value, b_value)
            }

            (
                TypeKind::FixedCapacityAndLengthArray(a, size_a),
                TypeKind::FixedCapacityAndLengthArray(b, size_b),
            ) => {
                size_a == size_b && type_cache.compatible_with(a, b) // Fixed arrays must have the same size
            }

            (TypeKind::Tuple(elems_a), TypeKind::Tuple(elems_b)) => {
                elems_a.len() == elems_b.len() // Tuples must have same length
            }

            (TypeKind::AnonymousStruct(anon_a), TypeKind::AnonymousStruct(anon_b)) => {
                anon_a.field_name_sorted_fields.len() == anon_b.field_name_sorted_fields.len()
                    && anon_a
                        .field_name_sorted_fields
                        .keys()
                        .all(|key| anon_b.field_name_sorted_fields.contains_key(key))
            }

            (TypeKind::NamedStruct(named_a), TypeKind::NamedStruct(named_b)) => {
                named_a.assigned_name == named_b.assigned_name
                    && type_cache
                        .compatible_with(&named_a.anon_struct_type, &named_b.anon_struct_type)
            }

            (TypeKind::Enum(enum_a), TypeKind::Enum(enum_b)) => {
                enum_a.assigned_name == enum_b.assigned_name
                    && enum_a.instantiated_type_parameters.len()
                        == enum_b.instantiated_type_parameters.len()
            }

            (TypeKind::Function(sig_a), TypeKind::Function(sig_b)) => {
                sig_a.parameters.len() == sig_b.parameters.len()
            }

            // Default case
            _ => self.compatible_with_storage_and_view(other, type_cache),
        }
    }

    #[must_use]
    pub fn compatible_with_storage_and_view(
        &self,
        other_view: &Self,
        type_cache: &mut TypeCache,
    ) -> bool {
        let left_kind = self.lowest_common_denominator_vec_like_view();
        let right_kind = other_view.lowest_common_denominator_vec_like_view();
        if left_kind.is_some() && right_kind.is_some() {
            let left = left_kind.unwrap();
            let right = right_kind.unwrap();
            match (left, right) {
                (TypeKind::SliceView(a), TypeKind::SliceView(b)) => {
                    type_cache.compatible_with(&a, &b)
                }
                (TypeKind::DynamicLengthVecView(a), TypeKind::DynamicLengthVecView(b)) => {
                    type_cache.compatible_with(&a, &b)
                }
                _ => false,
            }
        } else {
            match (&*self.kind, &*other_view.kind) {
                (
                    TypeKind::MapStorage(key_a, value_a, _),
                    TypeKind::DynamicLengthMapView(key_b, value_b),
                )
                | (
                    TypeKind::DynamicLengthMapView(key_a, value_a),
                    TypeKind::MapStorage(key_b, value_b, _),
                ) => {
                    type_cache.compatible_with(key_a, key_b)
                        && type_cache.compatible_with(value_a, value_b)
                }
                _ => false,
            }
        }
    }

    // Helper method for lowest_common_denominator_view
    #[must_use]
    pub fn lowest_common_denominator_vec_like_view(&self) -> Option<TypeKind> {
        match &*self.kind {
            TypeKind::FixedCapacityAndLengthArray(inner, _)
            | TypeKind::QueueStorage(inner, _)
            | TypeKind::StackStorage(inner, _)
            | TypeKind::SparseStorage(inner, _)
            | TypeKind::StringStorage(inner, _, _)
            | TypeKind::VecStorage(inner, _) => Some(TypeKind::SliceView(inner.clone())),

            TypeKind::SliceView(inner)
            | TypeKind::QueueView(inner)
            | TypeKind::StackView(inner)
            | TypeKind::SparseView(inner)
            | TypeKind::StringView(inner, _)
            | TypeKind::DynamicLengthVecView(inner) => Some(TypeKind::SliceView(inner.clone())),

            _ => None,
        }
    }

    #[must_use]
    pub fn strict_compatible_with_capacity(&self, other: &Self) -> bool {
        match (&*self.kind, &*other.kind) {
            (TypeKind::VecStorage(_, cap_a), TypeKind::VecStorage(_, cap_b)) => cap_a == cap_b,
            (TypeKind::SparseStorage(_, cap_a), TypeKind::SparseStorage(_, cap_b)) => {
                cap_a == cap_b
            }
            (TypeKind::QueueStorage(_, cap_a), TypeKind::QueueStorage(_, cap_b)) => cap_a == cap_b,
            (TypeKind::StackStorage(_, cap_a), TypeKind::StackStorage(_, cap_b)) => cap_a == cap_b,
            (TypeKind::MapStorage(_, _, cap_a), TypeKind::MapStorage(_, _, cap_b)) => {
                cap_a == cap_b
            }
            (
                TypeKind::GridStorage(_, rows_a, cols_a),
                TypeKind::GridStorage(_, rows_b, cols_b),
            ) => rows_a == rows_b && cols_a == cols_b,
            // For all other types, default to regular compatibility
            _ => self.id == other.id,
        }
    }
}
