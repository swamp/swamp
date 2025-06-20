use crate::{Type, TypeRef};
use seq_map::SeqMap;
use source_map_node::Node;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NamedStructType {
    pub name: Node,
    pub module_path: Vec<String>,
    pub assigned_name: String,
    pub anon_struct_type: TypeRef,
    pub instantiated_type_parameters: Vec<Type>,
}

impl NamedStructType {
    #[must_use]
    pub fn new(
        name: Node,
        assigned_name: &str,
        anon_struct_type: TypeRef,
        module_path: &[String],
    ) -> Self {
        Self {
            anon_struct_type,
            name,
            module_path: module_path.to_vec(),
            assigned_name: assigned_name.to_string(),
            instantiated_type_parameters: Vec::default(),
        }
    }

    #[must_use]
    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        if let crate::TypeKind::AnonymousStruct(anon_struct) = &*self.anon_struct_type.kind {
            anon_struct
                .field_name_sorted_fields
                .get_index(&field_name.to_string())
        } else {
            None
        }
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct AnonymousStructType {
    pub field_name_sorted_fields: SeqMap<String, StructTypeField>,
}

impl AnonymousStructType {
    #[must_use]
    pub fn new_and_sort_fields(source_ordered_fields: &SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: sort_struct_fields2(source_ordered_fields),
        }
    }

    #[must_use]
    pub const fn new(defined_order: SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: defined_order,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct StructTypeField {
    pub identifier: Option<Node>,
    pub field_type: TypeRef,
}

impl Display for StructTypeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.field_type)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: TypeRef,
}

impl Signature {
    #[must_use]
    pub fn same_type(&self, other: &Self) -> bool {
        if !self.return_type.do_compatible_with(&other.return_type) {
            return false;
        }

        if self.parameters.len() != other.parameters.len() {
            return false;
        }

        for (i, self_param) in self.parameters.iter().enumerate() {
            let other_param = &other.parameters[i];

            if !self_param
                .resolved_type
                .do_compatible_with(&other_param.resolved_type)
            {
                return false;
            }

            if self_param.is_mutable != other_param.is_mutable {
                return false;
            }
        }

        true
    }

    #[must_use]
    pub fn is_self_mutable(&self) -> bool {
        self.parameters
            .first()
            .is_some_and(|x| x.name == "self" && x.is_mutable)
    }
}

#[derive(Debug, Eq, Clone)]
pub struct TypeForParameter {
    pub name: String,
    pub resolved_type: TypeRef,
    pub is_mutable: bool,
    pub node: Option<ParameterNode>,
}

impl PartialEq for TypeForParameter {
    fn eq(&self, other: &Self) -> bool {
        self.resolved_type == other.resolved_type && self.is_mutable == other.is_mutable
    }
}

impl std::hash::Hash for TypeForParameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.resolved_type.hash(state);
        self.is_mutable.hash(state);
    }
}

impl Display for TypeForParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}: {:?}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type
        )
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ParameterNode {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Debug for ParameterNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "Parameter")
    }
}

impl ParameterNode {
    #[inline]
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable.is_some()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumType {
    pub name: Node,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub variants: SeqMap<String, EnumVariantType>,
    pub instantiated_type_parameters: Vec<Type>,
}

impl EnumType {
    #[must_use]
    pub fn new(name: Node, assigned_name: &str, module_path: Vec<String>) -> Self {
        Self {
            name,
            assigned_name: assigned_name.to_string(),
            module_path,
            variants: SeqMap::new(),
            instantiated_type_parameters: Vec::default(),
        }
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }

    #[must_use]
    pub fn get_variant(&self, name: &str) -> Option<&EnumVariantType> {
        self.variants.get(&name.to_string())
    }

    #[must_use]
    pub fn are_all_variants_without_payload(&self) -> bool {
        self.variants
            .iter()
            .all(|(_name, variant)| variant.payload_type.is_unit())
    }

    #[must_use]
    pub fn get_variant_from_index(&self, index: usize) -> Option<&EnumVariantType> {
        Some(self.variants.values().collect::<Vec<_>>()[index])
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct EnumVariantType {
    pub common: EnumVariantCommon,
    pub payload_type: TypeRef, // AnonymousStruct, Unit, or Tuple
}

impl EnumVariantType {
    #[must_use]
    pub const fn common(&self) -> &EnumVariantCommon {
        &self.common
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct EnumVariantCommon {
    pub name: Node,
    pub assigned_name: String,
    pub container_index: u8,
}

impl EnumVariantCommon {
    #[must_use]
    pub const fn index(&self) -> u8 {
        self.container_index
    }
}

#[must_use]
pub fn sort_struct_fields2(
    unordered_seq_map: &SeqMap<String, StructTypeField>,
) -> SeqMap<String, StructTypeField> {
    let mut sorted_pairs: Vec<(&String, &StructTypeField)> = unordered_seq_map.iter().collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(b.0));

    sorted_pairs
        .into_iter()
        .map(|(name, field)| (name.clone(), field.clone()))
        .collect()
}

impl Display for NamedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.instantiated_type_parameters.is_empty() {
            write!(f, "{}", self.assigned_name)
        } else {
            write!(
                f,
                "{}<{}>",
                self.assigned_name,
                seq_fmt::comma(&self.instantiated_type_parameters)
            )
        }
    }
}

impl Display for AnonymousStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{ {} }}",
            seq_fmt::comma(
                &self
                    .field_name_sorted_fields
                    .iter()
                    .map(|(name, field)| format!("{}: {}", name, field.field_type))
                    .collect::<Vec<_>>()
            )
        )
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> {}",
            seq_fmt::comma(&self.parameters),
            self.return_type
        )
    }
}

impl Display for EnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.instantiated_type_parameters.is_empty() {
            write!(f, "{}", self.assigned_name)
        } else {
            write!(
                f,
                "{}<{}>",
                self.assigned_name,
                seq_fmt::comma(&self.instantiated_type_parameters)
            )
        }
    }
}

impl Display for EnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.payload_type)
    }
}
