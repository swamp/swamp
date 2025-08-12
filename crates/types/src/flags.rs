/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::type_kind::TypeKind;

/// Flags describing properties of a type
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TypeFlags(u32);

impl Default for TypeFlags {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeFlags {
    pub const NONE: Self = Self(0);
    /// Can be held on to for a short while
    pub const ALLOWED_AS_TRANSIENT: Self = Self(1 << 0);
    /// Is the type self-contained in a register
    pub const IS_SCALAR: Self = Self(1 << 1);
    /// Can it be stored in a sum or product type
    pub const IS_STORAGE: Self = Self(1 << 2);
    pub const IS_ALLOWED_RETURN: Self = Self(1 << 3);

    #[must_use]
    pub const fn new() -> Self {
        Self::NONE
    }

    #[must_use]
    pub const fn contains(self, flag: Self) -> bool {
        (self.0 & flag.0) != 0
    }

    #[must_use]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Compute type flags based on the `TypeKind`
    #[allow(clippy::too_many_lines)]
    #[must_use]
    pub fn compute_for_type_kind(kind: &TypeKind) -> Self {
        let mut flags = Self::NONE;

        match kind {
            TypeKind::Never => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::Any => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::Pointer(_) => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::Codepoint => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Byte => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Int => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }

            TypeKind::Float => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Bool => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Unit => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_ALLOWED_RETURN)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Function(_) => {}
            TypeKind::Enum(enum_type) => {
                if enum_type.are_all_variants_with_blittable_payload() {
                    flags = flags
                        .union(Self::ALLOWED_AS_TRANSIENT)
                        .union(Self::IS_ALLOWED_RETURN);
                } else {
                    panic!("enums should be blittable")
                }
                if enum_type.are_all_variants_with_storage_payload() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::Tuple(types) => {
                // A tuple is blittable if all its component types are blittable
                if types.iter().all(|t| t.flags.contains(Self::ALLOWED_AS_TRANSIENT)) {
                    flags = flags
                        .union(Self::ALLOWED_AS_TRANSIENT)
                        .union(Self::IS_ALLOWED_RETURN);
                }
                if types.iter().all(|t| t.flags.contains(Self::IS_STORAGE)) {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::Optional(inner) => {
                // An optional is blittable if its inner type is blittable
                if inner.flags.contains(Self::ALLOWED_AS_TRANSIENT) {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                // TODO: check this
                if inner.flags.contains(Self::IS_ALLOWED_RETURN) {
                    flags = flags.union(Self::IS_ALLOWED_RETURN);
                }
                if inner.flags.contains(Self::IS_STORAGE) {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::NamedStruct(named) => {
                if named.anon_struct_type.flags.contains(Self::ALLOWED_AS_TRANSIENT) {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if named.anon_struct_type.flags.contains(Self::IS_STORAGE) {
                    flags = flags.union(Self::IS_STORAGE);
                }
                if named
                    .anon_struct_type
                    .flags
                    .contains(Self::IS_ALLOWED_RETURN)
                {
                    flags = flags.union(Self::IS_ALLOWED_RETURN);
                }
            }
            TypeKind::AnonymousStruct(anon) => {
                if anon
                    .field_name_sorted_fields
                    .iter()
                    .all(|(_name, field)| field.field_type.flags.contains(Self::ALLOWED_AS_TRANSIENT))
                {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }

                anon.field_name_sorted_fields
                    .iter()
                    .all(|(_name, field)| field.field_type.flags.contains(Self::IS_ALLOWED_RETURN));
                flags = flags.union(Self::IS_ALLOWED_RETURN);

                if anon
                    .field_name_sorted_fields
                    .iter()
                    .all(|(_name, field)| field.field_type.flags.contains(Self::IS_STORAGE))
                {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::MapStorage(key, value, _) => {
                if key.is_blittable() && value.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }

                if key.is_storage() && value.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::DynamicLengthMapView(_, _) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::FixedCapacityAndLengthArray(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::SliceView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::VecStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::DynamicLengthVecView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }

            TypeKind::StringStorage(_, _, _) => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_STORAGE);
            }

            TypeKind::StringView(..) => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_ALLOWED_RETURN);
            }

            TypeKind::StackStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::StackView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }

            TypeKind::QueueStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::QueueView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::GridStorage(inner, _, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::GridView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::SparseStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::ALLOWED_AS_TRANSIENT);
                }
                if inner.is_storage() {
                    flags = flags.union(Self::IS_STORAGE);
                }
            }
            TypeKind::SparseView(_) => {
                flags = flags.union(Self::IS_ALLOWED_RETURN);
            }
            TypeKind::Range(_inner) => {
                flags = flags
                    .union(Self::ALLOWED_AS_TRANSIENT)
                    .union(Self::IS_ALLOWED_RETURN);
                flags = flags.union(Self::IS_STORAGE);
            }
        }

        flags
    }
}
