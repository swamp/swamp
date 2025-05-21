/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

/*
pub fn overwrite_value(target: &ValueRef, source: Value) {
    if let Value::AnonymousStruct(ref mut target_struct_type_ref, ref mut target_fields) =
        *target.borrow_mut()
    {
        if let Value::AnonymousStruct(source_struct_type_ref, source_fields) = source {
            overwrite_struct(
                target_struct_type_ref,
                target_fields,
                &source_struct_type_ref,
                &source_fields,
            );
        }
    }
}

/// # Panics
///
pub fn overwrite_struct(
    target_struct_type_ref: &NamedStructType,
    target_values: &mut Vec<ValueRef>,
    source_struct: &NamedStructType,
    source_values: &[ValueRef],
) {
    let borrowed_source_struct_type = source_struct;
    let source_anon_type = &borrowed_source_struct_type.anon_struct_type;

    for ((field_name, target_field_type), target_field_value) in target_struct_type_ref
        .anon_struct_type
        .field_name_sorted_fields
        .iter()
        .zip(target_values)
    {
        if let Some(source_field_type) = source_anon_type.field_name_sorted_fields.get(field_name) {
            if source_field_type
                .field_type
                .compatible_with(&target_field_type.field_type)
            {
                let index = source_anon_type
                    .field_name_sorted_fields
                    .get_index(field_name)
                    .expect("should work, we checked get()");
                *target_field_value.borrow_mut() = source_values[index].borrow().clone();
            } else {
                panic!("overwrite_struct, wrong type");
            }
        }
    }
}
*/
