/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_node::Node;

#[derive(Debug, Clone, Default)]
pub struct Attributes(pub Vec<Attribute>);

impl Attributes {
    #[must_use]
    pub fn get_attributes(&self, name: &str) -> Self {
        let vec: Vec<_> = self
            .0
            .iter()
            .filter(|attr| attr.path.name == name)
            .cloned()
            .collect();

        Self(vec)
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[must_use]
    pub fn has_attribute(&self, name: &str) -> bool {
        self.0.iter().any(|attr| attr.path.name == name)
    }

    #[must_use]
    pub fn get_attribute(&self, name: &str) -> Option<&Attribute> {
        self.0.iter().find(|attr| attr.path.name == name)
    }

    #[must_use]
    pub fn get_fn_arg_by_name(&self, attr_name: &str, arg_fn_name: &str) -> Option<&AttributeArg> {
        self.get_attribute(attr_name).and_then(|attr| {
            attr.args.iter().find(|arg| {
                if let AttributeArg::Function(id, _) = arg {
                    id.name == arg_fn_name
                } else {
                    false
                }
            })
        })
    }

    #[must_use]
    pub fn get_fn_arg_sub_args(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
    ) -> Option<&Vec<AttributeArg>> {
        self.get_fn_arg_by_name(attr_name, arg_fn_name)
            .and_then(|arg| {
                if let AttributeArg::Function(_, sub_args) = arg {
                    Some(sub_args)
                } else {
                    None // This should ideally not happen if `get_fn_arg_by_name` already filtered for Function
                }
            })
    }

    #[must_use]
    pub fn get_arg(&self, name: &str, arg_index: usize) -> Option<AttributeArg> {
        self.get_args(name)
            .and_then(|a| Option::from(a[arg_index].clone()))
    }

    #[must_use]
    pub fn get_value(&self, name: &str, arg_index: usize) -> Option<AttributeValue> {
        self.get_arg(name, arg_index).and_then(|arg| match arg {
            AttributeArg::Literal(x) => Some(x),
            _ => None,
        })
    }

    #[must_use]
    pub fn get_int(&self, name: &str, arg_index: usize) -> Option<i32> {
        self.get_value(name, arg_index).and_then(|x| match x {
            AttributeValue::Int(v) => Some(v),
            _ => None,
        })
    }
    #[must_use]
    pub fn get_int_from_fn_arg(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
        sub_arg_index: usize,
    ) -> Option<i32> {
        self.get_fn_arg_sub_args(attr_name, arg_fn_name)
            .and_then(|sub_args| {
                sub_args
                    .get(sub_arg_index)
                    .and_then(|sub_arg| match sub_arg {
                        AttributeArg::Literal(attr_value) => match attr_value {
                            AttributeValue::Int(v) => Some(*v),
                            _ => None,
                        },
                        _ => None,
                    })
            })
    }

    #[must_use]
    pub fn get_string_from_fn_arg(
        &self,
        attr_name: &str,
        arg_fn_name: &str,
        sub_arg_index: usize,
    ) -> Option<&String> {
        self.get_fn_arg_sub_args(attr_name, arg_fn_name)
            .and_then(|sub_args| {
                sub_args
                    .get(sub_arg_index)
                    .and_then(|sub_arg| match sub_arg {
                        AttributeArg::Literal(attr_value) => match attr_value {
                            AttributeValue::String(s) => Some(s),
                            _ => None,
                        },
                        _ => None,
                    })
            })
    }

    #[must_use]
    pub fn get_string(&self, name: &str, arg_index: usize) -> Option<String> {
        self.get_value(name, arg_index).and_then(|x| match x {
            AttributeValue::String(v) => Some(v),
            _ => None,
        })
    }
    #[must_use]
    pub fn get_args(&self, name: &str) -> Option<Vec<AttributeArg>> {
        self.get_attribute(name)
            .and_then(|a| Option::from(a.args.clone()))
    }
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub path: AttributeIdentifier,
    pub args: Vec<AttributeArg>,
    pub node: Node,
}

#[derive(Debug, Clone)]
pub struct AttributeIdentifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    Path(AttributeIdentifier), // e.g., Debug, unix, clippy::something
    Literal(AttributeValue),   // e.g., "foo", 42, true
    Function(AttributeIdentifier, Vec<AttributeArg>), // e.g., any(unix, windows)
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    String(String),
    Int(i32),
    Bool(bool),
}
