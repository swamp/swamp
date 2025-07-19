/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod text;

use swamp_modules::prelude::{Module, SymbolTable};
use swamp_modules::symtbl::AliasType;
use swamp_semantic::prelude::{IntrinsicFunction, IntrinsicFunctionDefinition};
use swamp_types::prelude::{Signature, TypeCache, TypeForParameter};
use tiny_ver::TinyVersion;

pub const PACKAGE_NAME: &str = "core";
fn add_intrinsic_types(core_ns: &mut SymbolTable, cache: &mut TypeCache) {
    let any_alias = AliasType {
        name: None,
        assigned_name: "Any".to_string(),
        ty: cache.any(),
    };
    core_ns.add_alias(any_alias).unwrap();

    let byte_alias = AliasType {
        name: None,
        assigned_name: "Byte".to_string(),
        ty: cache.byte(),
    };
    core_ns.add_alias(byte_alias).unwrap();

    let char_alias = AliasType {
        name: None,
        assigned_name: "Char".to_string(),
        ty: cache.codepoint(),
    };
    core_ns.add_alias(char_alias).unwrap();

    let int_alias = AliasType {
        name: None,
        assigned_name: "Int".to_string(),
        ty: cache.int(),
    };
    core_ns.add_alias(int_alias).unwrap();

    let float_alias = AliasType {
        name: None,
        assigned_name: "Float".to_string(),
        ty: cache.float(),
    };
    core_ns.add_alias(float_alias).unwrap();

    let string_alias = AliasType {
        name: None,
        assigned_name: "String".to_string(),
        ty: cache.string(),
    };
    core_ns.add_alias(string_alias).unwrap();

    let bool_alias = AliasType {
        name: None,
        assigned_name: "Bool".to_string(),
        ty: cache.bool(),
    };
    core_ns.add_alias(bool_alias).unwrap();
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    add_intrinsic_byte_functions(core_ns, type_cache);
    add_intrinsic_codepoint_functions(core_ns, type_cache);
    add_intrinsic_bool_functions(core_ns, type_cache);
    add_intrinsic_float_functions(core_ns, type_cache);
    add_intrinsic_int_functions(core_ns, type_cache);
    add_intrinsic_string_functions(core_ns, type_cache);

    add_intrinsic_debug_functions(core_ns, type_cache);
}

fn add_intrinsic_debug_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let unit_type = type_cache.unit();

    let string_unit = Signature {
        parameters: [TypeForParameter {
            name: "v".to_string(),
            resolved_type: string_type,
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: unit_type.clone(),
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

    let nothing_unit = Signature {
        parameters: [].into(),
        return_type: unit_type,
    };
    let nothing_unit_functions = [
        IntrinsicFunction::RuntimeHalt,
        IntrinsicFunction::RuntimeStep,
    ];
    for intrinsic_fn in nothing_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: nothing_unit.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_string_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let int_type = type_cache.int();

    let string_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: string_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: int_type,
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

    let string_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: string_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
    };

    let string_to_string_functions = [IntrinsicFunction::StringToString];

    for intrinsic_fn in string_to_string_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: string_to_string.clone(),
            })
            .unwrap();
    }
}

fn add_intrinsic_bool_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let bool_type = type_cache.bool();

    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: bool_type,
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
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

fn add_intrinsic_byte_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let byte_type = type_cache.byte();

    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: byte_type,
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
    };
    let self_to_string_functions = [IntrinsicFunction::ByteToString];
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

fn add_intrinsic_codepoint_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let char_type = type_cache.codepoint();
    let int_type = type_cache.int();

    let self_to_string = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: char_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
    };
    let self_to_string_functions = [IntrinsicFunction::CodepointToString];
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

    let self_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: char_type,
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: int_type,
    };
    let self_to_int_functions = [IntrinsicFunction::CodepointToInt];
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
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_int_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let int_type = type_cache.int();
    let float_type = type_cache.float();
    let int_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: int_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: int_type.clone(),
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
            resolved_type: int_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
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
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
            .into(),
        return_type: int_type.clone(),
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
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "a".into(),
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
            .into(),
        return_type: int_type.clone(),
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
            resolved_type: int_type,
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: float_type,
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
fn add_intrinsic_float_functions(core_ns: &mut SymbolTable, type_cache: &mut TypeCache) {
    let string_type = type_cache.string();
    let int_type = type_cache.int();
    let float_type = type_cache.float();

    let float_to_float = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: float_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: float_type.clone(),
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
            resolved_type: float_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: int_type,
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
            resolved_type: float_type.clone(),
            is_mutable: false,
            node: None,
        }]
            .into(),
        return_type: string_type,
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
                resolved_type: float_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "other".into(),
                resolved_type: float_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
            .into(),
        return_type: float_type.clone(),
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
                resolved_type: float_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "a".into(),
                resolved_type: float_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: float_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
            .into(),
        return_type: float_type,
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
pub fn create_module(tiny_version: &TinyVersion, type_cache: &mut TypeCache) -> Module {
    let canonical_core_path = [tiny_version.versioned_name(PACKAGE_NAME).unwrap()];
    let mut intrinsic_types_symbol_table = SymbolTable::new(&canonical_core_path);
    add_intrinsic_types(&mut intrinsic_types_symbol_table, type_cache);
    add_intrinsic_functions(&mut intrinsic_types_symbol_table, type_cache);

    Module::new(intrinsic_types_symbol_table, Vec::new(), None)
}

/// # Panics
/// if `versioned_name` is wrong
#[must_use]
pub fn create_module_with_name(path: &[String]) -> Module {
    let intrinsic_types_symbol_table = SymbolTable::new(path);

    Module::new(intrinsic_types_symbol_table, Vec::new(), None)
}
