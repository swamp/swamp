/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::SemanticError;
use source_map_node::Node;
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
}

impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self {
            node: Node::default(),
            kind: ErrorKind::SemanticError(value),
        }
    }
}
