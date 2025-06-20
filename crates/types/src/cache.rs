use crate::flags::TypeFlags;
use crate::supporting_types::{AnonymousStructType, EnumType, NamedStructType, Signature};
use crate::type_kind::{TypeKind, TypeRef};
use crate::{Type, TypeId};
use seq_map::SeqMap;
use std::rc::Rc;

/// Type cache for interning and deduplicating types in the system
#[derive(Debug, Clone)]
pub struct TypeCache {
    pub(crate) type_id_to_type: SeqMap<TypeId, Rc<Type>>,
    pub(crate) kind_to_type_id: SeqMap<TypeKind, TypeId>,
    pub(crate) compatible_cache: SeqMap<(TypeId, TypeId), bool>,
    pub(crate) next_id: u32,
}

impl Default for TypeCache {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeCache {
    /// Create a new empty type cache
    #[must_use]
    pub fn new() -> Self {
        Self {
            type_id_to_type: SeqMap::new(),
            kind_to_type_id: SeqMap::new(),
            compatible_cache: SeqMap::new(),
            next_id: 0,
        }
    }

    #[must_use]
    pub const fn type_id_to_type(&self) -> &SeqMap<TypeId, Rc<Type>> {
        &self.type_id_to_type
    }

    #[must_use]
    pub const fn compatible_cache(&self) -> &SeqMap<(TypeId, TypeId), bool> {
        &self.compatible_cache
    }

    const fn next_type_id(&mut self) -> TypeId {
        let id = TypeId(self.next_id);
        self.next_id += 1;
        id
    }

    /// Create a new type instance with the given kind
    fn create_type(&mut self, kind: TypeKind) -> Rc<Type> {
        let id = self.next_type_id();

        let flags = TypeFlags::compute_for_type_kind(&kind);

        let type_instance = Type {
            id,
            flags,
            is_mutable: false,
            kind: Rc::new(kind),
        };

        let rc_type = Rc::new(type_instance);
        self.type_id_to_type.insert(id, Rc::clone(&rc_type));

        rc_type
    }

    fn create_type_mutable(&mut self, kind: TypeKind, is_mutable: bool) -> Rc<Type> {
        let id = self.next_type_id();

        let flags = TypeFlags::compute_for_type_kind(&kind);

        let type_instance = Type {
            id,
            flags,
            is_mutable,
            kind: Rc::new(kind),
        };

        let rc_type = Rc::new(type_instance);
        self.type_id_to_type.insert(id, Rc::clone(&rc_type));

        rc_type
    }

    /// Find an existing type in the cache by its kind
    #[inline]
    fn find_type(&self, kind: &TypeKind) -> Option<Rc<Type>> {
        self.kind_to_type_id
            .get(kind)
            .map(|id| self.type_id_to_type[id].clone())
    }

    /// Add a type to the cache for future lookups
    #[inline]
    fn add_type_to_cache(&mut self, type_: &Rc<Type>) {
        self.kind_to_type_id.insert((*type_.kind).clone(), type_.id);
    }

    /// Get a type by its ID
    #[inline]
    #[must_use]
    pub fn get_by_id(&self, id: TypeId) -> Option<Rc<Type>> {
        self.type_id_to_type.get(&id).cloned()
    }

    /// Check if two types are compatible
    #[allow(clippy::too_many_lines)]
    pub fn compatible_with(&mut self, a: &Type, b: &Type) -> bool {
        if a.id == b.id {
            return true;
        }

        let key = (a.id, b.id);

        if let Some(&result) = self.compatible_cache.get(&key) {
            return result;
        }

        // HACK: Mark as being processed (optimistically), this is to avoid recursion
        self.compatible_cache.insert(key, true);

        // Make the slow compatible check
        let base_compatible = a.do_compatible_with(b);

        // If not base compatible, we can return early
        if !base_compatible {
            self.compatible_cache.insert(key, false);
            return false;
        }

        // Now check inner types if needed
        let result = match (&*a.kind, &*b.kind) {
            (TypeKind::Optional(inner_a), TypeKind::Optional(inner_b)) => {
                self.compatible_with(inner_a, inner_b)
            }

            (TypeKind::VecStorage(elem_a, _), TypeKind::VecStorage(elem_b, _)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::SparseStorage(elem_a, _), TypeKind::SparseStorage(elem_b, _)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::QueueStorage(elem_a, _), TypeKind::QueueStorage(elem_b, _)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::StackStorage(elem_a, _), TypeKind::StackStorage(elem_b, _)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::SliceView(elem_a), TypeKind::SliceView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::SparseView(elem_a), TypeKind::SparseView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::QueueView(elem_a), TypeKind::QueueView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::StackView(elem_a), TypeKind::StackView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::DynamicLengthVecView(elem_a), TypeKind::DynamicLengthVecView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::MapStorage(key_a, val_a, _), TypeKind::MapStorage(key_b, val_b, _)) => {
                self.compatible_with(key_a, key_b) && self.compatible_with(val_a, val_b)
            }

            (
                TypeKind::DynamicLengthMapView(key_a, val_a),
                TypeKind::DynamicLengthMapView(key_b, val_b),
            ) => self.compatible_with(key_a, key_b) && self.compatible_with(val_a, val_b),

            (TypeKind::GridStorage(elem_a, _, _), TypeKind::GridStorage(elem_b, _, _)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::GridView(elem_a), TypeKind::GridView(elem_b)) => {
                self.compatible_with(elem_a, elem_b)
            }

            (TypeKind::Tuple(elems_a), TypeKind::Tuple(elems_b)) => {
                if elems_a.len() == elems_b.len() {
                    elems_a
                        .iter()
                        .zip(elems_b.iter())
                        .all(|(a, b)| self.compatible_with(a, b))
                } else {
                    false
                }
            }

            (
                TypeKind::FixedCapacityAndLengthArray(elem_a, size_a),
                TypeKind::FixedCapacityAndLengthArray(elem_b, size_b),
            ) => size_a == size_b && self.compatible_with(elem_a, elem_b),

            (TypeKind::AnonymousStruct(anon_a), TypeKind::AnonymousStruct(anon_b)) => {
                // Check if fields match
                anon_a.field_name_sorted_fields.len() == anon_b.field_name_sorted_fields.len()
                    && anon_a.field_name_sorted_fields.keys().all(|key| {
                        anon_b.field_name_sorted_fields.contains_key(key)
                            && self.compatible_with(
                                &anon_a.field_name_sorted_fields[key].field_type,
                                &anon_b.field_name_sorted_fields[key].field_type,
                            )
                    })
            }

            (TypeKind::Range(range_a), TypeKind::Range(range_b)) => {
                // Extract NamedStructType from TypeRef, then AnonymousStructType from that
                let named_a = match &*range_a.kind {
                    TypeKind::NamedStruct(named_struct) => named_struct,
                    _ => return false,
                };
                let named_b = match &*range_b.kind {
                    TypeKind::NamedStruct(named_struct) => named_struct,
                    _ => return false,
                };

                let anon_a = match &*named_a.anon_struct_type.kind {
                    TypeKind::AnonymousStruct(anon_struct) => anon_struct,
                    _ => return false,
                };
                let anon_b = match &*named_b.anon_struct_type.kind {
                    TypeKind::AnonymousStruct(anon_struct) => anon_struct,
                    _ => return false,
                };

                // Compare range types
                anon_a.field_name_sorted_fields.len() == anon_b.field_name_sorted_fields.len()
                    && anon_a.field_name_sorted_fields.keys().all(|key| {
                        anon_b.field_name_sorted_fields.contains_key(key)
                            && self.compatible_with(
                                &anon_a.field_name_sorted_fields[key].field_type,
                                &anon_b.field_name_sorted_fields[key].field_type,
                            )
                    })
            }

            (TypeKind::NamedStruct(named_a), TypeKind::NamedStruct(named_b)) => {
                // Check named struct compatibility
                if named_a.assigned_name != named_b.assigned_name
                    || named_a.instantiated_type_parameters.len()
                        != named_b.instantiated_type_parameters.len()
                {
                    false
                } else {
                    // Check type parameters compatibility
                    named_a
                        .instantiated_type_parameters
                        .iter()
                        .zip(named_b.instantiated_type_parameters.iter())
                        .all(|(a, b)| self.compatible_with(a, b))
                }
            }

            (TypeKind::Enum(enum_a), TypeKind::Enum(enum_b)) => {
                // Check enum compatibility
                if enum_a.assigned_name != enum_b.assigned_name
                    || enum_a.instantiated_type_parameters.len()
                        != enum_b.instantiated_type_parameters.len()
                {
                    false
                } else {
                    // Check type parameters compatibility
                    enum_a
                        .instantiated_type_parameters
                        .iter()
                        .zip(enum_b.instantiated_type_parameters.iter())
                        .all(|(a, b)| self.compatible_with(a, b))
                }
            }

            (TypeKind::Function(sig_a), TypeKind::Function(sig_b)) => {
                // Compare function signatures
                if sig_a.parameters.len() == sig_b.parameters.len() {
                    // Check parameters and return type
                    let params_match = sig_a
                        .parameters
                        .iter()
                        .zip(sig_b.parameters.iter())
                        .all(|(a, b)| self.compatible_with(&a.resolved_type, &b.resolved_type));

                    params_match && self.compatible_with(&sig_a.return_type, &sig_b.return_type)
                } else {
                    false
                }
            }

            _ => true,
        };

        self.compatible_cache.insert(key, result);

        result
    }

    #[must_use]
    pub fn can_be_stored_in_variable(&self, ty: &TypeRef) -> bool {
        todo!()
    }

    /// Clear the compatibility cache
    pub fn clear_compatibility_cache(&mut self) {
        self.compatible_cache.clear();
    }

    //
    // Primitive helpers
    // TODO: Maybe just add the primitives at creation instead
    //

    pub fn int(&mut self) -> Rc<Type> {
        let int_kind = TypeKind::Int;

        if let Some(existing) = self.find_type(&int_kind) {
            return existing;
        }

        let int_type = self.create_type(int_kind);
        self.add_type_to_cache(&int_type);
        int_type
    }

    pub fn float(&mut self) -> Rc<Type> {
        let float_kind = TypeKind::Float;

        if let Some(existing) = self.find_type(&float_kind) {
            return existing;
        }

        let float_type = self.create_type(float_kind);
        self.add_type_to_cache(&float_type);
        float_type
    }

    pub fn string(&mut self) -> Rc<Type> {
        let string_kind = TypeKind::String;

        if let Some(existing) = self.find_type(&string_kind) {
            return existing;
        }

        let string_type = self.create_type(string_kind);
        self.add_type_to_cache(&string_type);
        string_type
    }

    pub fn bool(&mut self) -> Rc<Type> {
        let bool_kind = TypeKind::Bool;

        if let Some(existing) = self.find_type(&bool_kind) {
            return existing;
        }

        let bool_type = self.create_type(bool_kind);
        self.add_type_to_cache(&bool_type);
        bool_type
    }

    pub fn unit(&mut self) -> Rc<Type> {
        let unit_kind = TypeKind::Unit;

        if let Some(existing) = self.find_type(&unit_kind) {
            return existing;
        }

        let unit_type = self.create_type(unit_kind);
        self.add_type_to_cache(&unit_type);
        unit_type
    }

    //
    // Container type helpers
    //

    pub fn optional(&mut self, inner_type: &Rc<Type>) -> Rc<Type> {
        let optional_kind = TypeKind::Optional(Rc::clone(inner_type));

        if let Some(existing) = self.find_type(&optional_kind) {
            return existing;
        }

        let optional_type = self.create_type(optional_kind);
        self.add_type_to_cache(&optional_type);
        optional_type
    }

    pub fn tuple(&mut self, element_types: Vec<Rc<Type>>) -> Rc<Type> {
        let tuple_kind = TypeKind::Tuple(element_types);

        if let Some(existing) = self.find_type(&tuple_kind) {
            return existing;
        }

        let tuple_type = self.create_type(tuple_kind);
        self.add_type_to_cache(&tuple_type);
        tuple_type
    }

    // TODO: Maybe use a shared function for types with one element type
    pub fn vec_storage(&mut self, element_type: &Rc<Type>, capacity: usize) -> Rc<Type> {
        let vec_kind = TypeKind::VecStorage(Rc::clone(element_type), capacity);

        if let Some(existing) = self.find_type(&vec_kind) {
            return existing;
        }

        let vec_type = self.create_type(vec_kind);
        self.add_type_to_cache(&vec_type);
        vec_type
    }

    pub fn sparse_storage(&mut self, element_type: &Rc<Type>, capacity: usize) -> Rc<Type> {
        let sparse_kind = TypeKind::SparseStorage(Rc::clone(element_type), capacity);

        if let Some(existing) = self.find_type(&sparse_kind) {
            return existing;
        }

        let sparse_type = self.create_type(sparse_kind);
        self.add_type_to_cache(&sparse_type);
        sparse_type
    }

    pub fn queue_storage(&mut self, element_type: &Rc<Type>, capacity: usize) -> Rc<Type> {
        let queue_kind = TypeKind::QueueStorage(Rc::clone(element_type), capacity);

        if let Some(existing) = self.find_type(&queue_kind) {
            return existing;
        }

        let queue_type = self.create_type(queue_kind);
        self.add_type_to_cache(&queue_type);
        queue_type
    }

    pub fn stack_storage(&mut self, element_type: &Rc<Type>, capacity: usize) -> Rc<Type> {
        let stack_kind = TypeKind::StackStorage(Rc::clone(element_type), capacity);

        if let Some(existing) = self.find_type(&stack_kind) {
            return existing;
        }

        let stack_type = self.create_type(stack_kind);
        self.add_type_to_cache(&stack_type);
        stack_type
    }

    pub fn map_storage(
        &mut self,
        key_type: &Rc<Type>,
        value_type: &Rc<Type>,
        capacity: usize,
    ) -> Rc<Type> {
        let map_kind = TypeKind::MapStorage(Rc::clone(key_type), Rc::clone(value_type), capacity);

        if let Some(existing) = self.find_type(&map_kind) {
            return existing;
        }

        let map_type = self.create_type(map_kind);
        self.add_type_to_cache(&map_type);
        map_type
    }

    pub fn grid_storage(&mut self, element_type: &Rc<Type>, rows: usize, cols: usize) -> Rc<Type> {
        let grid_kind = TypeKind::GridStorage(Rc::clone(element_type), rows, cols);

        if let Some(existing) = self.find_type(&grid_kind) {
            return existing;
        }

        let grid_type = self.create_type(grid_kind);
        self.add_type_to_cache(&grid_type);
        grid_type
    }

    pub fn fixed_array(&mut self, element_type: &Rc<Type>, size: usize) -> Rc<Type> {
        let array_kind = TypeKind::FixedCapacityAndLengthArray(Rc::clone(element_type), size);

        if let Some(existing) = self.find_type(&array_kind) {
            return existing;
        }

        let array_type = self.create_type(array_kind);
        self.add_type_to_cache(&array_type);
        array_type
    }

    pub fn slice_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let slice_kind = TypeKind::SliceView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&slice_kind) {
            return existing;
        }

        let slice_type = self.create_type(slice_kind);
        self.add_type_to_cache(&slice_type);
        slice_type
    }

    pub fn sparse_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let sparse_kind = TypeKind::SparseView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&sparse_kind) {
            return existing;
        }

        let sparse_type = self.create_type(sparse_kind);
        self.add_type_to_cache(&sparse_type);
        sparse_type
    }

    pub fn queue_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let queue_kind = TypeKind::QueueView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&queue_kind) {
            return existing;
        }

        let queue_type = self.create_type(queue_kind);
        self.add_type_to_cache(&queue_type);
        queue_type
    }

    pub fn stack_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let stack_kind = TypeKind::StackView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&stack_kind) {
            return existing;
        }

        let stack_type = self.create_type(stack_kind);
        self.add_type_to_cache(&stack_type);
        stack_type
    }

    pub fn dynamic_vec_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let vec_view_kind = TypeKind::DynamicLengthVecView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&vec_view_kind) {
            return existing;
        }

        let vec_view_type = self.create_type(vec_view_kind);
        self.add_type_to_cache(&vec_view_type);
        vec_view_type
    }

    pub fn dynamic_map_view(&mut self, key_type: &Rc<Type>, value_type: &Rc<Type>) -> Rc<Type> {
        let map_view_kind =
            TypeKind::DynamicLengthMapView(Rc::clone(key_type), Rc::clone(value_type));

        if let Some(existing) = self.find_type(&map_view_kind) {
            return existing;
        }

        let map_view_type = self.create_type(map_view_kind);
        self.add_type_to_cache(&map_view_type);
        map_view_type
    }

    pub fn grid_view(&mut self, element_type: &Rc<Type>) -> Rc<Type> {
        let grid_view_kind = TypeKind::GridView(Rc::clone(element_type));

        if let Some(existing) = self.find_type(&grid_view_kind) {
            return existing;
        }

        let grid_view_type = self.create_type(grid_view_kind);
        self.add_type_to_cache(&grid_view_type);
        grid_view_type
    }

    //
    // Complex type helpers
    //

    /// Create an anonymous struct type
    pub fn anonymous_struct(&mut self, anon_struct: AnonymousStructType) -> Rc<Type> {
        let struct_kind = TypeKind::AnonymousStruct(anon_struct);

        if let Some(existing) = self.find_type(&struct_kind) {
            return existing;
        }

        let struct_type = self.create_type(struct_kind);
        self.add_type_to_cache(&struct_type);
        struct_type
    }

    pub fn range(&mut self, range_struct_ref: TypeRef) -> Rc<Type> {
        let range_kind = TypeKind::Range(range_struct_ref);

        if let Some(existing) = self.find_type(&range_kind) {
            return existing;
        }

        let range_type = self.create_type(range_kind);
        self.add_type_to_cache(&range_type);
        range_type
    }

    pub fn named_struct(&mut self, named_struct: NamedStructType) -> Rc<Type> {
        let struct_kind = TypeKind::NamedStruct(named_struct);

        if let Some(existing) = self.find_type(&struct_kind) {
            return existing;
        }

        let struct_type = self.create_type(struct_kind);
        self.add_type_to_cache(&struct_type);
        struct_type
    }

    pub fn enum_type(&mut self, enum_type: EnumType) -> Rc<Type> {
        let enum_kind = TypeKind::Enum(enum_type);

        if let Some(existing) = self.find_type(&enum_kind) {
            return existing;
        }

        let enum_type = self.create_type(enum_kind);
        self.add_type_to_cache(&enum_type);
        enum_type
    }

    pub fn function(&mut self, signature: Signature) -> Rc<Type> {
        let function_kind = TypeKind::Function(signature);

        if let Some(existing) = self.find_type(&function_kind) {
            return existing;
        }

        let function_type = self.create_type(function_kind);
        self.add_type_to_cache(&function_type);
        function_type
    }
}
