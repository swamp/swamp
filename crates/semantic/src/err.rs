/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::SemanticError;
use source_map_node::Node;
use std::fmt;
use std::num::{ParseFloatError, ParseIntError};
use swamp_types::prelude::*;

#[derive(Clone, Debug)]
pub struct Error {
    pub node: Node,
    pub kind: ErrorKind,
}
#[derive(Clone, Debug)]
pub enum ErrorKind {
    NoAssociatedFunction(TypeRef, String),
    MissingSubscriptMember,
    UnusedVariablesCanNotBeMut,
    VariableTypeMustBeBlittable(TypeRef),
    GuardCanNotHaveMultipleWildcards,
    WildcardMustBeLastInGuard,
    GuardMustHaveWildcard,
    GuardHasNoType,
    TooManyDestructureVariables,
    CanNotDestructure,
    UnknownStructTypeReference,
    DuplicateFieldName,
    MissingFieldInStructInstantiation(Vec<String>, AnonymousStructType),
    UnknownVariable,
    ArrayIndexMustBeInt(TypeRef),
    OverwriteVariableWithAnotherType,
    NoneNeedsExpectedTypeHint,
    ExpectedMutableLocation,
    WrongNumberOfArguments(usize, usize),
    CanOnlyOverwriteVariableWithMut,
    OverwriteVariableNotAllowedHere,
    UnknownEnumVariantType,
    UnknownStructField,
    UnknownEnumVariantTypeInPattern,
    ExpectedEnumInPattern,
    WrongEnumVariantContainer(EnumVariantType),
    VariableIsNotMutable,
    ArgumentIsNotMutable,
    UnknownTypeReference,
    SemanticError(SemanticError),
    ExpectedOptional,
    MapKeyTypeMismatch {
        expected: TypeRef,
        found: TypeRef,
    },
    MapValueTypeMismatch {
        expected: TypeRef,
        found: TypeRef,
    },
    IncompatibleTypes {
        expected: TypeRef,
        found: TypeRef,
    },
    UnknownMemberFunction(TypeRef),
    ExpressionsNotAllowedInLetPattern,
    UnknownField,
    EnumVariantHasNoFields,
    ExpectedTupleType,
    TooManyTupleFields {
        max: usize,
        got: usize,
    },
    ExpectedBooleanExpression,
    NotAnIterator,
    IntConversionError(ParseIntError),
    FloatConversionError(ParseFloatError),
    BoolConversionError,
    DuplicateFieldInStructInstantiation(String),
    UnknownIdentifier(String),
    NoDefaultImplemented(TypeRef),
    UnknownConstant,
    NotValidLocationStartingPoint,
    CallsCanNotBePartOfChain,
    UnwrapCanNotBePartOfChain,
    NoneCoalesceCanNotBePartOfChain,
    InvalidOperatorAfterOptionalChaining,
    SelfNotCorrectType,
    CanNotNoneCoalesce,
    UnknownSymbol,
    UnknownEnumType,
    UnknownModule,
    BreakOutsideLoop,
    ReturnOutsideCompare,
    EmptyMatch,
    MatchArmsMustHaveTypes,
    ContinueOutsideLoop,
    ParameterIsNotMutable,
    CouldNotCoerceTo(TypeRef),
    UnexpectedType,
    CanNotAttachFunctionsToType,
    MissingMemberFunction(String, TypeRef),
    ExpectedLambda,
    ExpectedSlice,
    MissingToString(TypeRef),
    IncompatibleTypesForAssignment {
        expected: TypeRef,
        found: TypeRef,
    },
    CapacityNotEnough {
        size_requested: usize,
        capacity: usize,
    },
    ExpectedInitializerTarget {
        destination_type: TypeRef,
    },
    NoInferredTypeForEmptyInitializer,
    TooManyInitializerListElementsForStorage {
        capacity: usize,
    },
    KeyVariableNotAllowedToBeMutable,
    SelfNotCorrectMutableState,
    NotAllowedAsReturnType(TypeRef),
    ParameterTypeCanNotBeStorage(TypeRef),
    OperatorProblem,
    MatchMustHaveAtLeastOneArm,
    NeedStorage,
    TooManyParameters {
        encountered: usize,
        allowed: usize,
    },
    CanOnlyHaveFunctionCallAtStartOfPostfixChain,
    CanNotSubscriptWithThatType,
    EnumTypeWasntExpectedHere,
    CanNotInferEnumType,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error_message = match self {
            Self::EnumTypeWasntExpectedHere => "enum type was not expected here",
            Self::CanNotInferEnumType => "can not infer enum type",
            Self::ExpectedTupleType => "expected tuple type",
            Self::CanOnlyHaveFunctionCallAtStartOfPostfixChain => {
                "function calls only allowed at start of chain"
            }
            Self::CanNotSubscriptWithThatType => {
                "subscript not possible with that type"
            }
            // Function and Method Errors
            Self::NoAssociatedFunction(_, _) => "no associated function",
            Self::UnknownMemberFunction(_) => "unknown member function",
            Self::MissingMemberFunction(_, _) => "missing member function",
            Self::WrongNumberOfArguments(_, _) => "wrong number of arguments",
            Self::TooManyParameters { .. } => "too many parameters",
            Self::CallsCanNotBePartOfChain => "calls cannot be part of a chain",
            Self::ExpectedLambda => "expected a lambda",

            // Variable and Type Errors
            Self::UnknownVariable => "unknown variable",
            Self::UnknownIdentifier(_) => "unknown identifier",
            Self::UnknownTypeReference => "unknown type",
            Self::UnusedVariablesCanNotBeMut => "unused variable cannot be mutable",
            Self::VariableTypeMustBeBlittable(_) => "variable type must be blittable",
            Self::OverwriteVariableWithAnotherType => {
                "cannot overwrite variable with a different type"
            }
            Self::IncompatibleTypes { .. } => "incompatible types",
            Self::IncompatibleTypesForAssignment { .. } => "incompatible types for assignment",
            Self::CouldNotCoerceTo(_) => "could not coerce to type",
            Self::UnexpectedType => "unexpected type",
            Self::SelfNotCorrectType => "'self' is not the correct type",
            Self::SelfNotCorrectMutableState => "'self' has incorrect mutable state",
            Self::NotAllowedAsReturnType(_) => "not an allowed return type",
            Self::ParameterTypeCanNotBeStorage(_) => "parameter cannot be of storage type",

            // Mutability Errors
            Self::ExpectedMutableLocation => "expected a mutable location",
            Self::CanOnlyOverwriteVariableWithMut => "can only overwrite mutable variables",
            Self::OverwriteVariableNotAllowedHere => "overwriting variables is not allowed here",
            Self::VariableIsNotMutable => "variable is not mutable",
            Self::ArgumentIsNotMutable => "argument is not mutable",
            Self::ParameterIsNotMutable => "parameter is not mutable",
            Self::KeyVariableNotAllowedToBeMutable => "key variable cannot be mutable",

            // Struct and Enum Errors
            Self::UnknownStructTypeReference => "unknown struct type",
            Self::DuplicateFieldName => "duplicate field name",
            Self::MissingFieldInStructInstantiation(_, _) => {
                "missing field in struct instantiation"
            }
            Self::UnknownStructField => "unknown struct field",
            Self::UnknownField => "unknown field",
            Self::DuplicateFieldInStructInstantiation(_) => {
                "duplicate field in struct instantiation"
            }
            Self::UnknownEnumVariantType => "unknown enum variant",
            Self::UnknownEnumVariantTypeInPattern => "unknown enum variant in pattern",
            Self::ExpectedEnumInPattern => "expected an enum in pattern",
            Self::WrongEnumVariantContainer(_) => "wrong enum variant container",
            Self::EnumVariantHasNoFields => "enum variant has no fields",
            Self::UnknownEnumType => "unknown enum type",

            // Optional and Chaining Errors
            Self::ExpectedOptional => "expected optional",
            Self::NoneNeedsExpectedTypeHint => "none requires a type hint",
            Self::UnwrapCanNotBePartOfChain => "unwrap cannot be part of a chain",
            Self::NoneCoalesceCanNotBePartOfChain => "none coalesce cannot be part of a chain",
            Self::InvalidOperatorAfterOptionalChaining => {
                "invalid operator after optional chaining (?)"
            }
            Self::CanNotNoneCoalesce => "cannot none-coalesce",

            // Pattern Matching and Destructuring Errors
            Self::GuardCanNotHaveMultipleWildcards => "guard cannot have multiple wildcards",
            Self::WildcardMustBeLastInGuard => "wildcard must be last in guard",
            Self::GuardMustHaveWildcard => "guard must have a wildcard",
            Self::GuardHasNoType => "guard has no type",
            Self::TooManyDestructureVariables => "too many destructure variables",
            Self::CanNotDestructure => "cannot destructure",
            Self::ExpressionsNotAllowedInLetPattern => "expressions not allowed in let patterns",
            Self::EmptyMatch => "match statement is empty",
            Self::MatchArmsMustHaveTypes => "match arms must have types",
            Self::MatchMustHaveAtLeastOneArm => "match must have at least one arm",

            // Collection and Data Structure Errors
            Self::MissingSubscriptMember => "missing subscript member",
            Self::ArrayIndexMustBeInt(_) => "array index must be an integer",
            Self::MapKeyTypeMismatch { .. } => "map key type mismatch",
            Self::MapValueTypeMismatch { .. } => "map value type mismatch",
            Self::TooManyTupleFields { .. } => "too many tuple fields",
            Self::ExpectedSlice => "expected a slice",
            Self::CapacityNotEnough { .. } => "capacity not enough",
            Self::ExpectedInitializerTarget { .. } => "expected initializer target",
            Self::NoInferredTypeForEmptyInitializer => "cannot infer type for empty initializer",
            Self::TooManyInitializerListElementsForStorage { .. } => {
                "too many elements for storage"
            }

            // Control Flow Errors
            Self::BreakOutsideLoop => "break outside of a loop",
            Self::ContinueOutsideLoop => "continue outside of a loop",
            Self::ReturnOutsideCompare => "return outside of a compare",

            // Conversion and Operator Errors
            Self::IntConversionError(_) => "integer conversion error",
            Self::FloatConversionError(_) => "float conversion error",
            Self::BoolConversionError => "boolean conversion error",
            Self::ExpectedBooleanExpression => "expected a boolean expression",
            Self::OperatorProblem => "operator problem",
            Self::MissingToString(_) => "missing to_string implementation",

            // Miscellaneous Errors
            Self::SemanticError(e) => return write!(f, "{e:?}"),
            Self::NotAnIterator => "not an iterator",
            Self::NoDefaultImplemented(_) => "no default implementation",
            Self::UnknownConstant => "unknown constant",
            Self::NotValidLocationStartingPoint => "not a valid location starting point",
            Self::UnknownSymbol => "unknown symbol",
            Self::UnknownModule => "unknown module",
            Self::CanNotAttachFunctionsToType => "cannot attach functions to this type",
            Self::NeedStorage => "storage needed",
        };
        f.write_str(error_message)
    }
}


impl ErrorKind {
    #[must_use]
    pub const fn code(&self) -> usize {
        match self {
            Self::NoAssociatedFunction(_, _) => 1,
            Self::MissingSubscriptMember => 2,
            Self::UnusedVariablesCanNotBeMut => 3,
            Self::VariableTypeMustBeBlittable(_) => 4,
            Self::GuardCanNotHaveMultipleWildcards => 5,
            Self::WildcardMustBeLastInGuard => 6,
            Self::GuardMustHaveWildcard => 7,
            Self::GuardHasNoType => 8,
            Self::TooManyDestructureVariables => 9,
            Self::CanNotDestructure => 10,
            Self::UnknownStructTypeReference => 11,
            Self::DuplicateFieldName => 12,
            Self::MissingFieldInStructInstantiation(_, _) => 13,
            Self::UnknownVariable => 14,
            Self::ArrayIndexMustBeInt(_) => 15,
            Self::OverwriteVariableWithAnotherType => 16,
            Self::NoneNeedsExpectedTypeHint => 17,
            Self::ExpectedMutableLocation => 18,
            Self::WrongNumberOfArguments(_, _) => 19,
            Self::CanOnlyOverwriteVariableWithMut => 20,
            Self::OverwriteVariableNotAllowedHere => 21,
            Self::UnknownEnumVariantType => 22,
            Self::UnknownStructField => 23,
            Self::UnknownEnumVariantTypeInPattern => 24,
            Self::ExpectedEnumInPattern => 25,
            Self::WrongEnumVariantContainer(_) => 26,
            Self::VariableIsNotMutable => 27,
            Self::ArgumentIsNotMutable => 28,
            Self::UnknownTypeReference => 29,
            Self::SemanticError(_) => 30,
            Self::ExpectedOptional => 31,
            Self::MapKeyTypeMismatch { .. } => 32,
            Self::MapValueTypeMismatch { .. } => 33,
            Self::IncompatibleTypes { .. } => 34,
            Self::UnknownMemberFunction(_) => 35,
            Self::ExpressionsNotAllowedInLetPattern => 36,
            Self::UnknownField => 37,
            Self::EnumVariantHasNoFields => 38,
            Self::ExpectedTupleType => 39,
            Self::TooManyTupleFields { .. } => 40,
            Self::ExpectedBooleanExpression => 41,
            Self::NotAnIterator => 42,
            Self::IntConversionError(_) => 43,
            Self::FloatConversionError(_) => 44,
            Self::BoolConversionError => 45,
            Self::DuplicateFieldInStructInstantiation(_) => 46,
            Self::UnknownIdentifier(_) => 47,
            Self::NoDefaultImplemented(_) => 48,
            Self::UnknownConstant => 49,
            Self::NotValidLocationStartingPoint => 50,
            Self::CallsCanNotBePartOfChain => 51,
            Self::UnwrapCanNotBePartOfChain => 52,
            Self::NoneCoalesceCanNotBePartOfChain => 53,
            Self::InvalidOperatorAfterOptionalChaining => 54,
            Self::SelfNotCorrectType => 55,
            Self::CanNotNoneCoalesce => 56,
            Self::UnknownSymbol => 57,
            Self::UnknownEnumType => 58,
            Self::UnknownModule => 59,
            Self::BreakOutsideLoop => 60,
            Self::ReturnOutsideCompare => 61,
            Self::EmptyMatch => 62,
            Self::MatchArmsMustHaveTypes => 63,
            Self::ContinueOutsideLoop => 64,
            Self::ParameterIsNotMutable => 65,
            Self::CouldNotCoerceTo(_) => 66,
            Self::UnexpectedType => 67,
            Self::CanNotAttachFunctionsToType => 68,
            Self::MissingMemberFunction(_, _) => 69,
            Self::ExpectedLambda => 70,
            Self::ExpectedSlice => 71,
            Self::MissingToString(_) => 72,
            Self::IncompatibleTypesForAssignment { .. } => 73,
            Self::CapacityNotEnough { .. } => 74,
            Self::ExpectedInitializerTarget { .. } => 75,
            Self::NoInferredTypeForEmptyInitializer => 76,
            Self::TooManyInitializerListElementsForStorage { .. } => 77,
            Self::KeyVariableNotAllowedToBeMutable => 78,
            Self::SelfNotCorrectMutableState => 79,
            Self::NotAllowedAsReturnType(_) => 80,
            Self::ParameterTypeCanNotBeStorage(_) => 81,
            Self::OperatorProblem => 82,
            Self::MatchMustHaveAtLeastOneArm => 83,
            Self::NeedStorage => 84,
            Self::TooManyParameters { .. } => 85,
            Self::CanOnlyHaveFunctionCallAtStartOfPostfixChain => 86,
            Self::CanNotSubscriptWithThatType => 87,
            Self::EnumTypeWasntExpectedHere => 88,
            Self::CanNotInferEnumType => 89,
        }
    }
}


impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self {
            node: Node::default(),
            kind: ErrorKind::SemanticError(value),
        }
    }
}
