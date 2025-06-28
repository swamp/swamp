mod cache;
mod calc_compat;
mod flag_helper;
mod flags;
pub mod prelude;
mod pretty_print;
mod supporting_types;
mod type_kind;

use crate::flags::TypeFlags;
pub use crate::type_kind::TypeKind;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(PartialEq, Clone, Eq, Hash, Copy, Debug)]
pub struct TypeId(u32);

impl TypeId {
    pub const EMPTY: u32 = 0xffffffff;

    #[must_use]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn inner(&self) -> u32 {
        self.0
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

pub type TypeRef = Rc<Type>;

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}|{}", self.id, self.kind)
    }
}

impl Type {
    #[inline]
    #[must_use]
    pub const fn is_scalar(&self) -> bool {
        self.flags.contains(TypeFlags::IS_SCALAR)
    }

    #[inline]
    #[must_use]
    pub const fn can_be_stored_in_field(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    #[inline]
    #[must_use]
    pub const fn can_be_stored_in_variable(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    #[inline]
    #[must_use]
    pub const fn is_storage(&self) -> bool {
        self.flags.contains(TypeFlags::IS_STORAGE)
    }

    #[inline]
    #[must_use]
    pub const fn allowed_as_return_type(&self) -> bool {
        self.flags.contains(TypeFlags::IS_ALLOWED_RETURN)
    }

    #[inline]
    #[must_use]
    pub const fn allowed_as_parameter_type(&self) -> bool {
        self.flags.contains(TypeFlags::IS_ALLOWED_RETURN)
    }

    #[inline]
    #[must_use]
    pub const fn is_blittable(&self) -> bool {
        self.flags.contains(TypeFlags::IS_BLITTABLE)
    }

    /// Check if this type requires explicit storage allocation from caller
    /// Only dynamically-sized collections need this, not fixed-size aggregates
    #[must_use]
    pub fn needs_explicit_storage(&self) -> bool {
        match &*self.kind {
            // Dynamic collections that need explicit storage
            TypeKind::DynamicLengthVecView(_) 
            | TypeKind::DynamicLengthMapView(_, _)
            | TypeKind::StackView(_)
            | TypeKind::QueueView(_)
            | TypeKind::SparseView(_)
            | TypeKind::GridView(_)
            | TypeKind::SliceView(_) => true,

            // Fixed-size aggregates can be allocated automatically  
            TypeKind::Optional(_)
            | TypeKind::Tuple(_)
            | TypeKind::NamedStruct(_)
            | TypeKind::AnonymousStruct(_)
            | TypeKind::Enum(_)
            | TypeKind::Range(_)
            | TypeKind::VecStorage(_, _)
            | TypeKind::StackStorage(_, _) 
            | TypeKind::QueueStorage(_, _)
            | TypeKind::MapStorage(_, _, _)
            | TypeKind::SparseStorage(_, _)
            | TypeKind::GridStorage(_, _, _) 
            | TypeKind::FixedCapacityAndLengthArray(_, _)
            | TypeKind::StringStorage(_, _) => false,

            // Primitives don't need storage
            _ => false,
        }
    }
}
