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
    /// Can it be copied without any extra logic
    pub const IS_BLITTABLE: Self = Self(1 << 0);
    /// Is the type self-contained in a register
    pub const IS_SCALAR: Self = Self(1 << 1);
    /// Can it be stored in a sum or product type
    pub const IS_STORAGE: Self = Self(1 << 2);

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
            TypeKind::Int => {
                flags = flags
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Float => {
                flags = flags
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::String => {
                flags = flags.union(Self::IS_BLITTABLE).union(Self::IS_SCALAR); // Strings are "sort of" self-contained in a register
            }
            TypeKind::Bool => {
                flags = flags
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Unit => {
                flags = flags
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_STORAGE)
                    .union(Self::IS_SCALAR);
            }
            TypeKind::Function(_) => {}
            TypeKind::Enum(enum_type) => {
                if enum_type.are_all_variants_with_blittable_payload() {
                    flags = flags.union(Self::IS_BLITTABLE);
                } else {
                    panic!("enums should be blittable")
                }
                if enum_type.are_all_variants_with_storage_payload() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::Tuple(types) => {
                // A tuple is blittable if all its component types are blittable
                if types.iter().all(|t| t.flags.contains(Self::IS_BLITTABLE)) {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::Optional(inner) => {
                // An optional is blittable if its inner type is blittable
                if inner.flags.contains(Self::IS_BLITTABLE) {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::NamedStruct(named) => {
                if named.anon_struct_type.flags.contains(Self::IS_BLITTABLE) {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::AnonymousStruct(anon) => {
                if anon.field_name_sorted_fields.iter().all(|(_name, field)| {
                    if !field.field_type.is_blittable() {
                        panic!("what is wrong with field: {field}");
                    }
                    field.field_type.flags.contains(Self::IS_BLITTABLE)
                }) {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::MapStorage(key, value, _) => {
                if key.is_blittable() && value.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::FixedCapacityAndLengthArray(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::VecStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::StackStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::QueueStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::GridStorage(inner, _, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::SparseStorage(inner, _) => {
                if inner.is_blittable() {
                    flags = flags.union(Self::IS_BLITTABLE);
                }
            }
            TypeKind::Range(inner) => {
                flags = flags.union(Self::IS_BLITTABLE);
            }

            _ => {}
        }

        flags
    }
}
