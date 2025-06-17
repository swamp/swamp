use crate::Type;
use crate::supporting_types::{AnonymousStructType, NamedStructType};
use crate::type_kind::TypeKind;

impl Type {
    pub(crate) fn do_compatible_with(&self, other: &Self) -> bool {
        if self.id == other.id {
            return true;
        }

        match (&*self.kind, &*other.kind) {
            // Direct comparisons for primitive types
            (TypeKind::Int, TypeKind::Int) => true,
            (TypeKind::Float, TypeKind::Float) => true,
            (TypeKind::Bool, TypeKind::Bool) => true,
            (TypeKind::String, TypeKind::String) => true,
            (TypeKind::Unit, TypeKind::Unit) => true,

            // For container types, we compare their structure but delegate inner compatibility
            // checks to TypeCache.compatible_with
            (TypeKind::Optional(_), TypeKind::Optional(_)) => true,

            (TypeKind::VecStorage(_, cap_a), TypeKind::VecStorage(_, cap_b)) => {
                cap_a >= cap_b // Vec with larger capacity is compatible with smaller requirement
            }

            (TypeKind::SparseStorage(_, cap_a), TypeKind::SparseStorage(_, cap_b)) => {
                cap_a >= cap_b
            }

            (TypeKind::QueueStorage(_, cap_a), TypeKind::QueueStorage(_, cap_b)) => cap_a >= cap_b,

            (TypeKind::StackStorage(_, cap_a), TypeKind::StackStorage(_, cap_b)) => cap_a >= cap_b,

            (TypeKind::MapStorage(_, _, cap_a), TypeKind::MapStorage(_, _, cap_b)) => {
                cap_a >= cap_b
            }

            (
                TypeKind::GridStorage(_, rows_a, cols_a),
                TypeKind::GridStorage(_, rows_b, cols_b),
            ) => rows_a >= rows_b && cols_a >= cols_b,

            (TypeKind::SliceView(_), TypeKind::SliceView(_)) => true,
            (TypeKind::SparseView(_), TypeKind::SparseView(_)) => true,
            (TypeKind::QueueView(_), TypeKind::QueueView(_)) => true,
            (TypeKind::StackView(_), TypeKind::StackView(_)) => true,
            (TypeKind::DynamicLengthVecView(_), TypeKind::DynamicLengthVecView(_)) => true,
            (TypeKind::DynamicLengthMapView(_, _), TypeKind::DynamicLengthMapView(_, _)) => true,

            (
                TypeKind::FixedCapacityAndLengthArray(_, size_a),
                TypeKind::FixedCapacityAndLengthArray(_, size_b),
            ) => {
                size_a == size_b // Fixed arrays must have the same size
            }

            (TypeKind::MutableReference(_), TypeKind::MutableReference(_)) => true,

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
                    && named_a.instantiated_type_parameters.len()
                        == named_b.instantiated_type_parameters.len()
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
            _ => false,
        }
    }

    // Helper method for lowest_common_denominator_view
    fn lowest_common_denominator_view(&self) -> Option<TypeKind> {
        match &*self.kind {
            TypeKind::FixedCapacityAndLengthArray(inner, _size)
            | TypeKind::QueueStorage(inner, _size)
            | TypeKind::StackStorage(inner, _size)
            | TypeKind::VecStorage(inner, _size) => Some(TypeKind::SliceView(inner.clone())),

            TypeKind::SliceView(inner)
            | TypeKind::QueueView(inner)
            | TypeKind::StackView(inner)
            | TypeKind::DynamicLengthVecView(inner) => Some(TypeKind::SliceView(inner.clone())),

            _ => None,
        }
    }

    #[must_use] pub fn strict_compatible_with_capacity(&self, other: &Self) -> bool {
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

/// Performs a strict comparison of named struct types, requiring exact matches of all fields and types
#[must_use]
pub fn same_named_struct_ref_strict(a: &NamedStructType, b: &NamedStructType) -> bool {
    if a.assigned_name != b.assigned_name || a.module_path != b.module_path {
        return false;
    }

    compare_anonymous_struct_types_strict(&a.anon_struct_type, &b.anon_struct_type)
}

/// Performs a strict comparison of anonymous struct types, requiring exact matches of all fields and types
#[must_use]
pub fn same_anon_struct_ref_strict(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    compare_anonymous_struct_types_strict(a, b)
}

/// Helper function for strict comparison of anonymous struct types
#[must_use]
pub fn compare_anonymous_struct_types_strict(
    a: &AnonymousStructType,
    b: &AnonymousStructType,
) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for ((a_name, a_type), (b_name, b_type)) in a
        .field_name_sorted_fields
        .iter()
        .zip(b.field_name_sorted_fields.iter())
    {
        if a_name != b_name {
            return false;
        }

        if !a_type
            .field_type
            .strict_compatible_with_capacity(&b_type.field_type)
        {
            return false;
        }
    }

    true
}

pub fn same_named_struct_ref(a: &NamedStructType, b: &NamedStructType) -> bool {
    if a.assigned_name != b.assigned_name {
        return false;
    }

    if a.instantiated_type_parameters.len() != b.instantiated_type_parameters.len() {
        return false;
    }

    for (a_param, b_param) in a
        .instantiated_type_parameters
        .iter()
        .zip(b.instantiated_type_parameters.iter())
    {
        if !a_param.do_compatible_with(b_param) {
            return false;
        }
    }

    compare_anonymous_struct_types(&a.anon_struct_type, &b.anon_struct_type)
}

/// Performs a regular comparison of anonymous struct types, allowing compatible field types
#[must_use]
pub fn same_anon_struct_ref(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    compare_anonymous_struct_types(a, b)
}

/// Helper function for regular comparison of anonymous struct types
#[must_use]
pub fn compare_anonymous_struct_types(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for ((a_name, a_type), (b_name, b_type)) in a
        .field_name_sorted_fields
        .iter()
        .zip(b.field_name_sorted_fields.iter())
    {
        if a_name != b_name {
            return false;
        }

        if !a_type.field_type.do_compatible_with(&b_type.field_type) {
            return false;
        }
    }

    true
}
