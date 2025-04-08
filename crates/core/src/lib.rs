/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_node::Node;
use swamp_modules::modules::Module;
use swamp_modules::symtbl::{SymbolTable, TypeGenerator, TypeGeneratorKind};
use swamp_semantic::prelude::{IntrinsicFunction, IntrinsicFunctionDefinition};
use swamp_types::{AliasType, Signature, Type, TypeForParameter};
use tiny_ver::TinyVersion;

pub const PACKAGE_NAME: &str = "core";
fn add_intrinsic_types(core_ns: &mut SymbolTable) {
    let int_alias = AliasType {
        name: Node::default(),
        assigned_name: "Int".to_string(),
        referenced_type: Type::Int,
    };
    core_ns.add_alias(int_alias).unwrap();

    let float_alias = AliasType {
        name: Node::default(),
        assigned_name: "Float".to_string(),
        referenced_type: Type::Float,
    };
    core_ns.add_alias(float_alias).unwrap();

    let string_alias = AliasType {
        name: Node::default(),
        assigned_name: "String".to_string(),
        referenced_type: Type::String,
    };
    core_ns.add_alias(string_alias).unwrap();

    let bool_alias = AliasType {
        name: Node::default(),
        assigned_name: "Bool".to_string(),
        referenced_type: Type::Bool,
    };
    core_ns.add_alias(bool_alias).unwrap();
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_functions(core_ns: &mut SymbolTable) {
    add_intrinsic_bool_functions(core_ns);
    add_intrinsic_float_functions(core_ns);
    add_intrinsic_int_functions(core_ns);
    add_intrinsic_string_functions(core_ns);
    add_intrinsic_grid_functions(core_ns);
    add_intrinsic_vec_functions(core_ns);
    add_intrinsic_map_functions(core_ns);
    add_intrinsic_map2_functions(core_ns);
    add_intrinsic_sparse_functions(core_ns);
    add_intrinsic_debug_functions(core_ns);
}

fn add_intrinsic_debug_functions(core_ns: &mut SymbolTable) {
    let string_unit = Signature {
        parameters: [TypeForParameter {
            name: "v".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let string_unit_functions = [IntrinsicFunction::RuntimePanic];
    for intrinsic_fn in string_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: string_unit.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_sparse_functions(core_ns: &mut SymbolTable) {
    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let slice_to_self_functions = [IntrinsicFunction::SparseFromSlice];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }

    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(Type::Never),
    };

    let unit_to_value_functions = [IntrinsicFunction::SparseCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_to_value_functions = [
        IntrinsicFunction::SparseIter,
        IntrinsicFunction::SparseIterMut,
    ];

    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_value_to_value_functions = [
        IntrinsicFunction::SparseSubscript,
        IntrinsicFunction::SparseAdd,
    ];

    for intrinsic_fn in self_value_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "value".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_value_value_mut_to_unit_functions = [IntrinsicFunction::SparseSubscriptMut];

    for intrinsic_fn in self_value_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };

    let self_value_to_bool_functions = [IntrinsicFunction::SparseHas];

    for intrinsic_fn in self_value_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_bool.clone(),
            })
            .unwrap();
    }

    let self_value_to_option_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Optional(Box::new(Type::Never))),
    };

    let self_value_to_option_value_functions = [IntrinsicFunction::SparseRemove];

    for intrinsic_fn in self_value_to_option_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_option_value.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_map2_functions(core_ns: &mut SymbolTable) {
    let self_value_value_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };
    let self_value_to_bool_functions = [IntrinsicFunction::Map2Has];
    for intrinsic_fn in self_value_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_to_bool.clone(),
            })
            .unwrap();
    }

    let self_value_to_vec = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_value_to_vec_functions = [IntrinsicFunction::Map2GetColumn];
    for intrinsic_fn in self_value_to_vec_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_vec.clone(),
            })
            .unwrap();
    }

    let self_value_value_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_value_value_to_value_functions = [IntrinsicFunction::Map2Get];
    for intrinsic_fn in self_value_value_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_y_to_vec = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_value_y_to_vec_functions = [IntrinsicFunction::Map2GetRow];
    for intrinsic_fn in self_value_y_to_vec_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_y_to_vec.clone(),
            })
            .unwrap();
    }

    let self_value_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let self_value_value_to_unit_functions = [IntrinsicFunction::Map2Remove];
    for intrinsic_fn in self_value_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_value_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let self_value_value_value_to_unit_functions = [IntrinsicFunction::Map2Insert];
    for intrinsic_fn in self_value_value_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_value_to_unit.clone(),
            })
            .unwrap();
    }

    let to_self = Signature {
        parameters: [].into(),
        return_type: Box::new(Type::Never),
    };
    let to_self_functions = [IntrinsicFunction::Map2Create];
    for intrinsic_fn in to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: to_self.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_map_functions(core_ns: &mut SymbolTable) {
    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice_pair".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let slice_to_self_functions = [IntrinsicFunction::MapFromSlicePair];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }

    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(Type::Never),
    };

    let unit_to_value_functions = [IntrinsicFunction::MapCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_to_value_functions = [IntrinsicFunction::MapIter, IntrinsicFunction::MapIterMut];

    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_value_to_value_functions = [IntrinsicFunction::MapSubscript];

    for intrinsic_fn in self_value_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_value_value_mut_to_unit_functions = [
        IntrinsicFunction::MapSubscriptMut,
        IntrinsicFunction::MapSubscriptMutCreateIfNeeded,
    ];

    for intrinsic_fn in self_value_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };

    let self_value_to_bool_functions = [IntrinsicFunction::MapHas];

    for intrinsic_fn in self_value_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_bool.clone(),
            })
            .unwrap();
    }

    let self_value_to_unit_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };

    let self_value_to_option_value_functions = [IntrinsicFunction::MapRemove];

    for intrinsic_fn in self_value_to_option_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_unit_value.clone(),
            })
            .unwrap();
    }

    let self_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let self_to_int_functions = [IntrinsicFunction::MapLen];

    for intrinsic_fn in self_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_int.clone(),
            })
            .unwrap();
    }

    let self_to_bool = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Bool),
    };

    let self_to_bool_functions = [IntrinsicFunction::MapIsEmpty];

    for intrinsic_fn in self_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_bool.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_grid_functions(core_ns: &mut SymbolTable) {
    let int_int_value_to_self = Signature {
        parameters: [
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "initial_value".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let int_int_value_to_self_functions = [IntrinsicFunction::GridCreate];
    for intrinsic_fn in int_int_value_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_int_value_to_self.clone(),
            })
            .unwrap();
    }

    let self_value_int_int_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let self_value_int_int_value_to_unit_functions = [IntrinsicFunction::GridSet];
    for intrinsic_fn in self_value_int_int_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_int_int_value_to_unit.clone(),
            })
            .unwrap();
    }

    let self_int_int_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_int_int_to_value_functions = [IntrinsicFunction::GridGet];
    for intrinsic_fn in self_int_int_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_int_to_value.clone(),
            })
            .unwrap();
    }

    let self_int_to_array_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_int_to_array_value_functions = [IntrinsicFunction::GridGetColumn];
    for intrinsic_fn in self_int_to_array_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_array_value.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_vec_functions(core_ns: &mut SymbolTable) {
    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(Type::Never),
    };

    let unit_to_value_functions = [IntrinsicFunction::VecCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };

    let self_value_to_unit_functions = [IntrinsicFunction::VecPush];

    for intrinsic_fn in self_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: true,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_value_to_option_functions = [IntrinsicFunction::VecPop];
    for intrinsic_fn in self_value_to_option_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_int_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_int_to_value_functions = [IntrinsicFunction::VecSubscript];
    for intrinsic_fn in self_int_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_value.clone(),
            })
            .unwrap();
    }

    let self_range_to_self = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "range".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_range_to_self_functions = [IntrinsicFunction::VecSubscriptRange];
    for intrinsic_fn in self_range_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_range_to_self.clone(),
            })
            .unwrap();
    }

    let self_int_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };

    let self_int_value_mut_to_unit_functions = [IntrinsicFunction::VecSubscriptMut];

    for intrinsic_fn in self_int_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let mut_self_int_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let mut_self_int_to_value_functions = [IntrinsicFunction::VecRemoveIndexGetValue];
    for intrinsic_fn in mut_self_int_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: mut_self_int_to_value.clone(),
            })
            .unwrap();
    }

    let self_int_to_unit_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let self_int_to_unit_value_functions = [IntrinsicFunction::VecRemoveIndex];
    for intrinsic_fn in self_int_to_unit_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_unit_value.clone(),
            })
            .unwrap();
    }

    let self_int_to_optional_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Optional(Box::new(Type::Never))),
    };
    let self_int_to_optional_value_functions = [IntrinsicFunction::VecGet];
    for intrinsic_fn in self_int_to_optional_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_optional_value.clone(),
            })
            .unwrap();
    }

    let mut_self_to_unit = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: true,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let mut_self_to_unit_functions = [IntrinsicFunction::VecClear];
    for intrinsic_fn in mut_self_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: mut_self_to_unit.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_to_value_functions = [
        IntrinsicFunction::VecIter,
        IntrinsicFunction::VecIterMut,
        IntrinsicFunction::VecFirst,
        IntrinsicFunction::VecLast,
    ];
    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let self_block_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "block".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let self_block_to_unit_functions = [IntrinsicFunction::VecFor, IntrinsicFunction::VecWhile];
    for intrinsic_fn in self_block_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_block_to_unit.clone(),
            })
            .unwrap();
    }

    let self_block_to_generic = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "block".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_block_to_generic_functions = [
        IntrinsicFunction::VecMap,
        IntrinsicFunction::VecFindMap,
        IntrinsicFunction::VecFilter,
        IntrinsicFunction::VecFilterMap,
        IntrinsicFunction::VecFind,
    ];
    for intrinsic_fn in self_block_to_generic_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_block_to_generic.clone(),
            })
            .unwrap();
    }

    let self_element_block_to_generic = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "element".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "block".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let self_element_block_to_generic_functions = [IntrinsicFunction::VecFold];
    for intrinsic_fn in self_element_block_to_generic_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_element_block_to_generic.clone(),
            })
            .unwrap();
    }

    let self_block_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "block".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };
    let self_block_to_bool_functions = [IntrinsicFunction::VecAny, IntrinsicFunction::VecAll];
    for intrinsic_fn in self_block_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_block_to_bool.clone(),
            })
            .unwrap();
    }

    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice".to_string(),
            resolved_type: Type::Never, // TODO: Should have proper slice generic
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Never),
    };
    let slice_to_self_functions = [IntrinsicFunction::VecFromSlice];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }

    let self_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let self_to_int_functions = [IntrinsicFunction::VecLen];

    for intrinsic_fn in self_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_int.clone(),
            })
            .unwrap();
    }

    let self_to_bool = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: Type::Never,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Bool),
    };
    let self_to_bool_functions = [IntrinsicFunction::VecIsEmpty];
    for intrinsic_fn in self_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_bool.clone(),
            })
            .unwrap();
    }

    let mut_self_int_int_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "a".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let mut_self_int_int_to_unit_functions = [IntrinsicFunction::VecSwap];
    for intrinsic_fn in mut_self_int_int_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: mut_self_int_int_to_unit.clone(),
            })
            .unwrap();
    }

    let mut_self_int_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: Type::Never,
                is_mutable: true,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: Type::Never,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };
    let mut_self_int_value_to_unit_functions = [IntrinsicFunction::VecInsert];
    for intrinsic_fn in mut_self_int_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: mut_self_int_value_to_unit.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_string_functions(core_ns: &mut SymbolTable) {
    let string_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::String,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let string_to_int_functions = [IntrinsicFunction::StringLen];

    for intrinsic_fn in string_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: string_to_int.clone(),
            })
            .unwrap();
    }
}

fn add_intrinsic_bool_functions(core_ns: &mut SymbolTable) {
    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Bool,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::String),
    };
    let self_to_string_functions = [IntrinsicFunction::BoolToString];
    for intrinsic_fn in self_to_string_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_string.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_int_functions(core_ns: &mut SymbolTable) {
    let int_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Int,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };
    let int_to_int_functions = [IntrinsicFunction::IntAbs, IntrinsicFunction::IntRnd];
    for intrinsic_fn in int_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_to_int.clone(),
            })
            .unwrap();
    }

    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Int,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::String),
    };
    let self_to_string_functions = [IntrinsicFunction::IntToString];
    for intrinsic_fn in self_to_string_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_string.clone(),
            })
            .unwrap();
    }

    let int_int_to_int = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Int),
    };
    let int_int_to_int_functions = [IntrinsicFunction::IntMax, IntrinsicFunction::IntMin];

    for intrinsic_fn in int_int_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_int_to_int.clone(),
            })
            .unwrap();
    }

    let int_int_int_to_int = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "a".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Int),
    };
    let int_int_int_to_int_functions = [IntrinsicFunction::IntClamp];
    for intrinsic_fn in int_int_int_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_int_int_to_int.clone(),
            })
            .unwrap();
    }

    let int_to_float = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Int,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Float),
    };

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: IntrinsicFunction::IntToFloat.to_string(),
            intrinsic: IntrinsicFunction::IntToFloat,
            signature: int_to_float,
        })
        .unwrap();
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_float_functions(core_ns: &mut SymbolTable) {
    let float_to_float = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Float,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Float),
    };

    let float_to_float_functions = [
        IntrinsicFunction::FloatSqrt,
        IntrinsicFunction::FloatSign,
        IntrinsicFunction::FloatAbs,
        IntrinsicFunction::FloatRnd,
        IntrinsicFunction::FloatCos,
        IntrinsicFunction::FloatSin,
        IntrinsicFunction::FloatAcos,
        IntrinsicFunction::FloatAsin,
    ];
    for intrinsic_fn in float_to_float_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_to_float.clone(),
            })
            .unwrap();
    }

    let float_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Float,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };
    let float_to_int_functions = [IntrinsicFunction::FloatRound, IntrinsicFunction::FloatFloor];
    for intrinsic_fn in float_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_to_int.clone(),
            })
            .unwrap();
    }

    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Float,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::String),
    };
    let self_to_string_functions = [IntrinsicFunction::FloatToString];
    for intrinsic_fn in self_to_string_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_string.clone(),
            })
            .unwrap();
    }

    let float_float_to_float = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "other".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Float),
    };

    let float_float_to_float_functions = [
        IntrinsicFunction::FloatAtan2,
        IntrinsicFunction::FloatMin,
        IntrinsicFunction::FloatMax,
        IntrinsicFunction::Float2Magnitude,
    ];
    for intrinsic_fn in float_float_to_float_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_float_to_float.clone(),
            })
            .unwrap();
    }

    let float_float_float_to_float = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "a".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Float),
    };

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: IntrinsicFunction::FloatClamp.to_string(),
            intrinsic: IntrinsicFunction::FloatClamp,
            signature: float_float_float_to_float,
        })
        .unwrap();
}

/// # Panics
/// if `versioned_name` is wrong
#[must_use]
pub fn create_module(tiny_version: &TinyVersion) -> Module {
    let canonical_core_path = [tiny_version.versioned_name(PACKAGE_NAME).unwrap()];
    let mut intrinsic_types_symbol_table = SymbolTable::new(&canonical_core_path);
    add_intrinsic_types(&mut intrinsic_types_symbol_table);
    add_intrinsic_type_generator(&mut intrinsic_types_symbol_table);
    add_intrinsic_functions(&mut intrinsic_types_symbol_table);

    Module::new(intrinsic_types_symbol_table, None)
}

fn add_intrinsic_type_generator(core_ns: &mut SymbolTable) {
    core_ns
        .add_type_generator(
            "Slice",
            TypeGenerator {
                arity: 1,
                kind: TypeGeneratorKind::Slice,
            },
        )
        .unwrap();

    core_ns
        .add_type_generator(
            "SlicePair",
            TypeGenerator {
                arity: 2,
                kind: TypeGeneratorKind::SlicePair,
            },
        )
        .unwrap();
}
