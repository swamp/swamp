/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_types::{TypeKind, TypeRef};

/// `TypeRef` checking context
#[derive(Debug, Clone)]
pub struct TypeContext<'a> {
    /// Expected type for the current expression
    pub expected_type: Option<&'a TypeRef>,
    pub has_lvalue_target: bool,
    pub ephemeral_is_allowed: bool,
}

impl<'a> TypeContext<'a> {
    #[must_use]
    pub const fn new(expected_type: Option<&'a TypeRef>, has_lvalue_target: bool) -> Self {
        Self {
            expected_type,
            has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    pub(crate) const fn with_lvalue(&self) -> Self {
        Self {
            expected_type: self.expected_type,
            has_lvalue_target: true,
            ephemeral_is_allowed: false,
        }
    }

    pub(crate) const fn with_ephemeral(&self) -> TypeContext {
        Self {
            expected_type: self.expected_type,
            has_lvalue_target: self.has_lvalue_target,
            ephemeral_is_allowed: true,
        }
    }

    pub(crate) const fn argument(&self, expected_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(expected_type),
            has_lvalue_target: self.has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    pub(crate) const fn with_argument_anything(&self) -> Self {
        Self {
            expected_type: None,
            has_lvalue_target: self.has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    #[must_use]
    pub const fn new_argument(required_type: &'a TypeRef, has_lvalue_target: bool) -> Self {
        Self {
            expected_type: Some(required_type),
            has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    pub(crate) const fn new_argument_ephemeral(
        required_type: &'a TypeRef,
        has_lvalue_target: bool,
    ) -> Self {
        Self {
            expected_type: Some(required_type),
            has_lvalue_target,
            ephemeral_is_allowed: true,
        }
    }

    #[must_use]
    pub const fn new_unsure_argument(
        expected_type: Option<&'a TypeRef>,
        has_lvalue_target: bool,
    ) -> Self {
        Self {
            expected_type,
            has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    #[must_use]
    pub const fn new_anything_argument(has_lvalue_target: bool) -> Self {
        Self {
            expected_type: None,
            has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    #[must_use]
    pub fn new_function(required_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(required_type),
            has_lvalue_target: required_type.collection_view_that_needs_explicit_storage(),
            ephemeral_is_allowed: false,
        }
    }

    #[must_use]
    pub const fn with_expected_type(
        &self,
        expected_type: Option<&'a TypeRef>,
        has_lvalue_target: bool,
    ) -> Self {
        Self {
            expected_type,
            has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }

    pub(crate) const fn we_know_expected_type(
        &self,
        found_type: &'a TypeRef,
        has_lvalue_target: bool,
    ) -> Self {
        self.with_expected_type(Some(found_type), has_lvalue_target)
    }

    /// If the expected type is `Optional<T>`, returns `T`; otherwise returns the
    /// original expected type.
    #[must_use]
    pub fn expected_type_or_optional_inner(&self) -> Self {
        #[allow(clippy::bind_instead_of_map)]
        let new_expected = self
            .expected_type
            .and_then(|expected| match expected.kind.as_ref() {
                TypeKind::Optional(inner) => Some(inner),
                _ => Some(expected),
            });

        Self {
            expected_type: new_expected,
            has_lvalue_target: self.has_lvalue_target,
            ephemeral_is_allowed: false,
        }
    }
}
