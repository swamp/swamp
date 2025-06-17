use crate::type_kind::TypeKind;

impl TypeKind {
    /// Check if this is a primitive type
    #[must_use]
    pub(crate) const fn is_primitive(&self) -> bool {
        matches!(self, Self::Int | Self::Float | Self::Bool | Self::Unit)
    }

    /// TODO: Is this used anywhere?
    #[must_use]
    pub(crate) const fn is_scalar(&self) -> bool {
        matches!(
            self,
            Self::Int | Self::String | Self::Float | Self::Bool | Self::Unit
        )
    }

    /// Check if this is a blittable type (can be copied byte-by-byte)
    /// TODO: Add rest of types, storage, struct, etc
    #[must_use]
    pub(crate) fn is_blittable(&self) -> bool {
        match self {
            Self::Int | Self::Float | Self::Bool | Self::Unit => true,
            Self::Enum(enum_type) => enum_type.are_all_variants_without_payload(),
            Self::Tuple(types) => types.iter().all(|t| t.kind.is_blittable()),
            Self::Optional(inner) => inner.kind.is_blittable(),
            _ => false,
        }
    }

    // TODO: This is not correct anymore, remove or fix it
    #[must_use]
    pub(crate) fn is_concrete_helper(&self) -> bool {
        match self {
            Self::Optional(inner) => inner.kind.is_concrete_helper(),
            Self::Tuple(elements) => elements.iter().all(|t| t.kind.is_concrete_helper()),
            Self::VecStorage(element, _)
            | Self::SparseStorage(element, _)
            | Self::QueueStorage(element, _)
            | Self::StackStorage(element, _)
            | Self::SliceView(element)
            | Self::SparseView(element)
            | Self::DynamicLengthVecView(element)
            | Self::QueueView(element)
            | Self::StackView(element)
            | Self::FixedCapacityAndLengthArray(element, _)
            | Self::Optional(element) => element.kind.is_concrete_helper(),

            Self::MapStorage(key, value, _) | Self::DynamicLengthMapView(key, value) => {
                key.kind.is_concrete_helper() && value.kind.is_concrete_helper()
            }

            Self::GridStorage(element, _, _) => element.kind.is_concrete_helper(),

            Self::NamedStruct(named) => {
                named.instantiated_type_parameters.is_empty()
                    || named
                        .instantiated_type_parameters
                        .iter()
                        .all(|t| t.kind.is_concrete_helper())
            }

            Self::Enum(enum_type) => {
                enum_type.instantiated_type_parameters.is_empty()
                    || enum_type
                        .instantiated_type_parameters
                        .iter()
                        .all(|t| t.kind.is_concrete_helper())
            }

            Self::Function(signature) => {
                signature.return_type.kind.is_concrete_helper()
                    && signature
                        .parameters
                        .iter()
                        .all(|p| p.resolved_type.kind.is_concrete_helper())
            }

            _ => true,
        }
    }

    /// Check if this is a direct type (can be stored directly in a variable)
    #[must_use]
    pub(crate) const fn is_direct(&self) -> bool {
        matches!(self, Self::Int | Self::Float | Self::Bool | Self::Unit)
    }

    /// Check if this is a function type
    #[must_use]
    pub(crate) const fn is_function_type(&self) -> bool {
        matches!(self, Self::Function(_))
    }

    /// Check if this type can be stored in a struct field
    #[must_use]
    pub(crate) fn helper_can_be_stored_in_field(&self) -> bool {
        match self {
            Self::Function(_) => false,
            Self::Tuple(types) => types.iter().all(|t| t.kind.helper_can_be_stored_in_field()),
            Self::Optional(inner) => inner.kind.helper_can_be_stored_in_field(),
            Self::NamedStruct(_) | Self::AnonymousStruct(_) | Self::Enum(_) => true,
            _ => self.is_primitive() || self.is_blittable(),
        }
    }
}
