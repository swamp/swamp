mod cache;
mod calc_compat;
mod flag_helper;
mod flags;
pub mod prelude;
mod supporting_types;
mod type_kind;

use crate::flags::TypeFlags;
use crate::type_kind::TypeKind;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(PartialEq, Clone, Eq, Hash, Copy, Debug)]
pub struct TypeId(u32);

impl TypeId {
    #[must_use]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub id: TypeId,
    pub flags: TypeFlags,
    pub kind: Rc<TypeKind>,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}>{}", self.id, self.kind)
    }
}

impl Type {
    #[inline]
    #[must_use]
    pub const fn is_bool(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BOOL)
    }

    #[inline]
    #[must_use]
    pub const fn is_unit(&self) -> bool {
        self.flags.contains(TypeFlags::IS_UNIT)
    }

    #[inline]
    #[must_use]
    pub const fn is_primitive(&self) -> bool {
        self.flags.contains(TypeFlags::IS_PRIMITIVE)
    }

    #[inline]
    #[must_use]
    pub const fn is_concrete(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    #[inline]
    #[must_use]
    pub const fn is_concrete_or_unit(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    #[inline]
    #[must_use]
    pub const fn is_function_type(&self) -> bool {
        self.flags.contains(TypeFlags::IS_FUNCTION_TYPE)
    }

    #[inline]
    #[must_use]
    pub const fn can_be_stored_in_field(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    #[inline]
    #[must_use]
    pub const fn is_blittable(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }
}
