/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use swamp_types::prelude::{Signature, TypeCache, TypeForParameter};

#[test]
fn test_signature_deduplication_with_typeref() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create two identical function signatures: (int, bool) -> int
    let signature1 = Signature {
        parameters: vec![
            TypeForParameter {
                name: "x".to_string(),
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "y".to_string(),
                resolved_type: bool_type.clone(),
                is_mutable: false,
                node: None,
            },
        ],
        return_type: int_type.clone(),
    };

    let signature2 = Signature {
        parameters: vec![
            TypeForParameter {
                name: "a".to_string(), // Different parameter name
                resolved_type: int_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".to_string(), // Different parameter name
                resolved_type: bool_type,
                is_mutable: false,
                node: None,
            },
        ],
        return_type: int_type,
    };

    // Create function types using the signatures
    let func_type1 = type_cache.function(signature1);
    let func_type2 = type_cache.function(signature2);

    // The function TypeRefs should be the same due to deduplication
    // even though parameter names differ (structural equality)
    assert!(std::rc::Rc::ptr_eq(&func_type1, &func_type2));
}

#[test]
fn test_signature_different_mutability_no_deduplication() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();

    // Create two function signatures with different mutability: (int) -> int vs (mut int) -> int
    let signature1 = Signature {
        parameters: vec![TypeForParameter {
            name: "x".to_string(),
            resolved_type: int_type.clone(),
            is_mutable: false,
            node: None,
        }],
        return_type: int_type.clone(),
    };

    let signature2 = Signature {
        parameters: vec![TypeForParameter {
            name: "x".to_string(),
            resolved_type: int_type.clone(),
            is_mutable: true, // Different mutability
            node: None,
        }],
        return_type: int_type,
    };

    // Create function types using the signatures
    let func_type1 = type_cache.function(signature1);
    let func_type2 = type_cache.function(signature2);

    // The function TypeRefs should NOT be the same due to different mutability
    assert!(!std::rc::Rc::ptr_eq(&func_type1, &func_type2));
}

#[test]
fn test_signature_return_type_sharing() {
    let mut type_cache = TypeCache::new();

    let int_type = type_cache.int();
    let bool_type = type_cache.bool();

    // Create two different function signatures that share return types
    let signature1 = Signature {
        parameters: vec![TypeForParameter {
            name: "x".to_string(),
            resolved_type: int_type,
            is_mutable: false,
            node: None,
        }],
        return_type: bool_type.clone(),
    };

    let signature2 = Signature {
        parameters: vec![TypeForParameter {
            name: "y".to_string(),
            resolved_type: bool_type.clone(),
            is_mutable: false,
            node: None,
        }],
        return_type: bool_type, // Same return type
    };

    let func_type1 = type_cache.function(signature1);
    let func_type2 = type_cache.function(signature2);

    // Functions should be different (different parameter types)
    assert!(!std::rc::Rc::ptr_eq(&func_type1, &func_type2));

    // But both should share the same return type TypeRef
    if let swamp_types::TypeKind::Function(sig1) = &*func_type1.kind {
        if let swamp_types::TypeKind::Function(sig2) = &*func_type2.kind {
            assert!(std::rc::Rc::ptr_eq(&sig1.return_type, &sig2.return_type));
        } else {
            panic!("Expected Function TypeKind");
        }
    } else {
        panic!("Expected Function TypeKind");
    }
}
