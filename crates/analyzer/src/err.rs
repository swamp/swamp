/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_node::Node;
use std::num::{ParseFloatError, ParseIntError};
use swamp_semantic::SemanticError;
use swamp_types::prelude::*;

#[derive(Debug)]
pub struct Error {
    pub node: Node,
    pub kind: ErrorKind,
}
#[derive(Debug)]
pub enum ErrorKind {
    NoAssociatedFunction(Type, String),
    MissingSubscriptMember,
    UnusedVariablesCanNotBeMut,
    VariableTypeMustBeConcrete,
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
    ArrayIndexMustBeInt(Type),
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
        expected: Type,
        found: Type,
    },
    MapValueTypeMismatch {
        expected: Type,
        found: Type,
    },
    IncompatibleTypes {
        expected: Type,
        found: Type,
    },
    UnknownMemberFunction(Type),
    ExpressionsNotAllowedInLetPattern,
    UnknownField,
    EnumVariantHasNoFields,
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
    NoDefaultImplemented(Type),
    UnknownConstant,
    NotValidLocationStartingPoint,
    CallsCanNotBePartOfChain,
    UnwrapCanNotBePartOfChain,
    NoneCoalesceCanNotBePartOfChain,
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
    CouldNotCoerceTo(Type),
    UnexpectedType,
    CanNotAttachFunctionsToType,
    MissingMemberFunction(String, Type),
    ExpectedLambda,
    ExpectedSlice,
    MissingToString(Type),
    IncompatibleTypesForAssignment {
        expected: Type,
        found: Type,
    },
    CapacityNotEnough {
        size_requested: usize,
        capacity: usize,
    },
    ExpectedInitializerTarget {
        destination_type: Type,
    },
}

impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self {
            node: Node::default(),
            kind: ErrorKind::SemanticError(value),
        }
    }
}
