/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use source_map_node::Node;
use swamp_core_extra::value::ValueError;
use swamp_semantic::ExternalFunctionId;

#[derive(Debug, PartialEq, Eq)]
pub enum ConversionError {
    TypeError(String),
    ValueError(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct RuntimeError {
    pub node: Node,
    pub kind: RuntimeErrorKind,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    StackCouldNotBePopped,
    VecIndexOutOfBoundsError { tried: i32, size: usize },
    MapKeyNonExisting,
    MustHaveGuardArmThatMatches,
    ValueError(ValueError),
    ArgumentIsNotMutable,
    WrongNumberOfArguments(usize, usize),
    ExpectedOptional,
    NonUniqueKeysInMapLiteralDetected,
    NotAnArray,
    NotSparseValue,
    CoerceOptionToBoolFailed,
    VariableWasNotMutable,
    ContinueNotAllowedHere,
    BreakNotAllowedHere,
    NotAMap,
    NotAMap2,
    MissingExternalFunction(ExternalFunctionId),
    ExpectedInt,
    ExpectedString,
    RangeItemMustBeInt,
    OperationRequiresArray,
    ExpectedFloat,
    ExpectedTwoFloatTuple,
    ExpectedFunction,
    NotSparseId,
    ReturnNotAllowedHere,
    ExpectedStruct,
    ExpectedArray,
    ExpectedMap,
    PostfixChainError,
    IndexOutOfBounds,
    DivideByZero,
    MapKeyAlreadyExists,
    CouldNotConvertFromSignal,
    UnknownMutIntrinsic,
    UnknownGenericIntrinsic,
    Panic(String),
    ExpectedBool,
}
