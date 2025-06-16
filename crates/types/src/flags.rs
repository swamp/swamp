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
    pub const IS_PRIMITIVE: Self = Self(1 << 0);
    pub const IS_INT: Self = Self(1 << 1);
    pub const IS_FLOAT: Self = Self(1 << 2);
    pub const IS_STRING: Self = Self(1 << 3);
    pub const IS_BOOL: Self = Self(1 << 4);
    pub const IS_UNIT: Self = Self(1 << 5);
    pub const IS_FUNCTION_TYPE: Self = Self(1 << 6);
    pub const IS_BLITTABLE: Self = Self(1 << 7);
    pub const IS_DIRECT: Self = Self(1 << 8);

    #[must_use]
    pub const fn new() -> Self {
        Self::NONE
    }

    #[must_use]
    pub const fn contains(self, flag: Self) -> bool {
        (self.0 & flag.0) == flag.0
    }

    #[must_use]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// Compute type flags based on the `TypeKind`
    #[must_use]
    pub fn compute_for_type_kind(kind: &TypeKind) -> Self {
        let mut flags = Self::NONE;

        match kind {
            TypeKind::Int => {
                flags = flags
                    .union(Self::IS_PRIMITIVE)
                    .union(Self::IS_INT)
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_DIRECT);
            }
            TypeKind::Float => {
                flags = flags
                    .union(Self::IS_PRIMITIVE)
                    .union(Self::IS_FLOAT)
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_DIRECT);
            }
            TypeKind::String => {
                flags = flags.union(Self::IS_PRIMITIVE).union(Self::IS_STRING);
            }
            TypeKind::Bool => {
                flags = flags
                    .union(Self::IS_PRIMITIVE)
                    .union(Self::IS_BOOL)
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_DIRECT);
            }
            TypeKind::Unit => {
                flags = flags
                    .union(Self::IS_PRIMITIVE)
                    .union(Self::IS_UNIT)
                    .union(Self::IS_BLITTABLE)
                    .union(Self::IS_DIRECT);
            }
            TypeKind::Function(_) => {
                flags = flags.union(Self::IS_FUNCTION_TYPE);
            }
            TypeKind::MutableReference(_) => {
                flags = flags.union(Self::IS_BLITTABLE).union(Self::IS_DIRECT);
            }
            TypeKind::Enum(enum_type) => {
                if enum_type.are_all_variants_without_payload() {
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
            _ => {}
        }

        flags
    }
}
