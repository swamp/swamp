/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMapError;
use std::num::{ParseFloatError, ParseIntError};
use swamp_script_node::{Node, Span};
use swamp_script_semantic::SemanticError;
use swamp_script_types::prelude::*;

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
    UnknownIdentifier,
    VariableTypeMustBeConcrete,
    GuardCanNotHaveMultipleWildcards,
    WildcardMustBeLastInGuard,
    GuardMustHaveWildcard,
    GuardHasNoType,
    TooManyDestructureVariables,
    EmptyBlockWrongType,
    CanNotDestructure,
    EmptySliceCanOnlyBeMapOrArray,
    //NamespaceError(NamespaceError),
    CanNotFindModule(Vec<String>),
    UnknownStructTypeReference,
    UnknownLocalStructTypeReference(swamp_script_ast::LocalTypeIdentifier),
    DuplicateFieldName,
    Unknown(String),
    UnknownImplTargetTypeReference(swamp_script_ast::LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(NamedStructType, usize),
    MissingFieldInStructInstantiation(Vec<String>, AnonymousStructType),
    ExpectedFunctionExpression,
    CouldNotFindMember(Node, Node),
    UnknownVariable,
    NotAnArray,
    ArrayIndexMustBeInt(Type),
    OverwriteVariableWithAnotherType,
    NoneNeedsExpectedTypeHint,
    ExpectedMutableLocation,
    WrongNumberOfArguments(usize, usize),
    IncompatibleArguments(Type, Type),
    CanOnlyOverwriteVariableWithMut,
    OverwriteVariableNotAllowedHere,
    NotNamedStruct(Type),
    UnknownEnumVariantType,
    WasNotStructType,
    UnknownStructField,
    MustBeEnumType(swamp_script_ast::Pattern),
    UnknownEnumVariantTypeInPattern,
    ExpectedEnumInPattern,
    WrongEnumVariantContainer(EnumVariantType),
    VariableIsNotMutable,
    ArgumentIsNotMutable,
    WrongNumberOfTupleDeconstructVariables,
    UnknownTypeReference,
    SemanticError(SemanticError),
    SeqMapError(SeqMapError),
    ExpectedMemberCall,
    CouldNotFindStaticMember(Node, Node),
    TypeAliasNotAStruct,
    ModuleNotUnique,
    ExpressionIsOfWrongFieldType(Span, Type, Type),
    ExpectedOptional,
    ExpectedVariable,
    EmptyMapLiteral,
    MapKeyTypeMismatch { expected: Type, found: Type },
    MapValueTypeMismatch { expected: Type, found: Type },
    TypeIsNotAnIndexCollection(Type),
    NotSameKeyTypeForMapIndex(Type, Type),
    UnknownIndexAwareCollection,
    InvalidOperatorForArray,
    IncompatibleTypes(Type, Type),
    ExpectedArray(Type),
    UnknownMemberFunction,
    WrongNumberOfTypeArguments(usize, usize),
    OnlyVariablesAllowedInEnumPattern,
    ExpressionsNotAllowedInLetPattern,
    UnknownField,
    EnumVariantHasNoFields,
    TooManyTupleFields { max: usize, got: usize },
    NotInFunction,
    ExpectedBooleanExpression,
    NotAnIterator,
    UnsupportedIteratorPairs,
    NeedStructForFieldLookup,
    IntConversionError(ParseIntError),
    FloatConversionError(ParseFloatError),
    BoolConversionError,
    DuplicateFieldInStructInstantiation(String),
    InternalError(&'static str),
    WasNotFieldMutRef,
    UnknownFunction,
    NoDefaultImplemented(Type),
    NoDefaultImplementedForStruct(NamedStructType),
    UnknownConstant,
    ExpectedFunctionTypeForFunctionCall,
    TypeDoNotSupportIndexAccess,
    NoneCoalesceNeedsOptionalType,
    TypeDoNotSupportRangeAccess,
    ArgumentMustBeImmutable,
    NotValidLocationStartingPoint,
    NotValidLocationItem,
    ExpectedImmutableExpression,
    CallsCanNotBePartOfChain,
    UnwrapCanNotBePartOfChain,
    NoneCoalesceCanNotBePartOfChain,
    SelfNotCorrectType,
    IllegalIndexInChain,
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
    NoDefaultImplementedForType(Type),
    UnknownTypeVariable,
    WrongParameterCount(usize, usize),
    UnexpectedType,
    CanNotAttachFunctionsToType,
}

impl From<SemanticError> for Error {
    fn from(value: SemanticError) -> Self {
        Self {
            node: Default::default(),
            kind: ErrorKind::SemanticError(value),
        }
    }
}
