use swamp_types::TypeRef;

/// `TypeRef` checking context
#[derive(Debug, Clone)]
pub struct TypeContext<'a> {
    /// Expected type for the current expression
    pub expected_type: Option<&'a TypeRef>,
    pub has_lvalue_target: bool,
}

impl<'a> TypeContext<'a> {
    #[must_use]
    pub const fn new(expected_type: Option<&'a TypeRef>, has_lvalue_target: bool) -> Self {
        Self {
            expected_type,
            has_lvalue_target,
        }
    }

    pub(crate) const fn with_lvalue(&self) -> Self {
        Self {
            expected_type: self.expected_type,
            has_lvalue_target: true,
        }
    }

    pub(crate) const fn argument(&self, expected_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(expected_type),
            has_lvalue_target: self.has_lvalue_target,
        }
    }

    #[must_use]
    pub const fn new_argument(required_type: &'a TypeRef, has_lvalue_target: bool) -> Self {
        Self {
            expected_type: Some(required_type),
            has_lvalue_target,
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
        }
    }

    #[must_use]
    pub const fn new_anything_argument(has_lvalue_target: bool) -> Self {
        Self {
            expected_type: None,
            has_lvalue_target,
        }
    }

    #[must_use]
    pub fn new_function(required_type: &'a TypeRef) -> Self {
        Self {
            expected_type: Some(required_type),
            has_lvalue_target: required_type.collection_view_that_needs_explicit_storage(),
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
        }
    }

    pub(crate) const fn we_know_expected_type(
        &self,
        found_type: &'a TypeRef,
        has_lvalue_target: bool,
    ) -> Self {
        self.with_expected_type(Some(found_type), has_lvalue_target)
    }
}
