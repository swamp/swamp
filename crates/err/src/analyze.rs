/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::semantic::build_semantic_error;
use crate::{build_and_print, Builder, Report};
use eira::Kind;
use source_map_cache::SourceMap;
use std::path::Path;
use swamp_semantic::prelude::{Error, ErrorKind};

#[must_use]
#[allow(clippy::too_many_lines)]
pub fn build_analyzer_error(err: &Error) -> Builder<usize> {
    let span = &err.node.span;
    let mut b = match &err.kind {
        ErrorKind::OutOfVirtualRegisters | ErrorKind::ByteConversionError(_) | ErrorKind::CanNotHaveSeparateMemberFuncRef | ErrorKind::CanNotSubscriptWithThatType | ErrorKind::EnumTypeWasntExpectedHere | ErrorKind::CanNotInferEnumType => {
            Report::build(Kind::Error, err.kind.code(), &err.kind.to_string(), span)
        }
        ErrorKind::CanOnlyHaveFunctionCallAtStartOfPostfixChain => {
            Report::build(Kind::Error, 23, &err.kind.to_string(), span)
        }
        ErrorKind::ExpectedTupleType => Report::build(Kind::Error, 23, "expected tuple type", span),
        ErrorKind::TooManyParameters {
            encountered,
            allowed,
        } => Report::build(
            Kind::Error,
            23,
            "function definition has too many parameters",
            span,
        )
            .with_note(&format!("encountered {encountered} allowed {allowed}")),
        ErrorKind::NeedStorage => {
            Report::build(Kind::Error, 23, "expression needs storage (lvalue)", span).with_note("")
        }
        ErrorKind::ParameterTypeCanNotBeStorage(ty) => {
            Report::build(Kind::Error, 5, "parameter type can not be storage", span)
                .with_note(&format!("type: {ty}"))
        }
        ErrorKind::NotAllowedAsReturnType(ty) => {
            Report::build(Kind::Error, 5, "not allowed as return type", span)
                .with_note(&format!("type: {ty}"))
        }
        ErrorKind::KeyVariableNotAllowedToBeMutable => {
            Report::build(Kind::Error, 5, "key variables can not be mut", span)
        }
        ErrorKind::TooManyInitializerListElementsForStorage { capacity } => {
            Report::build(Kind::Error, 5, "too many initializer elements", span)
                .with_note(&format!("capacity: {capacity}"))
        }
        ErrorKind::NoInferredTypeForEmptyInitializer => {
            Report::build(Kind::Error, 5, "empty initializer without known type", span)
        }
        ErrorKind::ExpectedInitializerTarget { destination_type } => {
            Report::build(Kind::Error, 5, "incompatible initializer list target", span)
                .with_note(&format!("target_type: {destination_type}"))
        }
        ErrorKind::CapacityNotEnough {
            size_requested,
            capacity,
        } => Report::build(Kind::Error, 5, "storage capacity not enough", span).with_note(
            &format!("size_requested: {size_requested} capacity {capacity}"),
        ),
        ErrorKind::MissingToString(ty) => {
            Report::build(Kind::Error, 5, "missing to_string()", span)
                .with_note(&format!("type: {ty}"))
        }
        ErrorKind::ExpectedLambda => Report::build(Kind::Error, 2, "expected lambda", span),
        ErrorKind::UnknownEnumType => Report::build(Kind::Error, 1, "unknown enum type", span),
        ErrorKind::TooManyDestructureVariables => {
            Report::build(Kind::Error, 2, "too many destructure variables", span)
        }
        ErrorKind::CanNotDestructure => Report::build(Kind::Error, 3, "can not destructure", span),
        ErrorKind::UnknownConstant => Report::build(Kind::Error, 4, "unknown constant", span),
        ErrorKind::UnknownStructTypeReference => {
            Report::build(Kind::Error, 5, "unknown struct type reference", span)
        }
        ErrorKind::DuplicateFieldName => {
            Report::build(Kind::Error, 6, "duplicate field name", span)
        }
        ErrorKind::MissingFieldInStructInstantiation(fields, _struct_type) => {
            Report::build(Kind::Error, 7, "missing fields in instantiation", span)
                .with_note(&format!("fields: {fields:?}"))
        }
        ErrorKind::UnknownVariable => Report::build(Kind::Error, 8, "unknown variable", span),
        ErrorKind::OverwriteVariableWithAnotherType => {
            Report::build(Kind::Error, 9, "overwrite variable with another type", span)
        }
        ErrorKind::WrongNumberOfArguments(_expected, _encountered) => {
            Report::build(Kind::Error, 10, "wrong number of arguments", span)
        }
        ErrorKind::CanOnlyOverwriteVariableWithMut => Report::build(
            Kind::Error,
            11,
            "overwrite requires variable to be mut",
            span,
        ),
        ErrorKind::UnknownEnumVariantType => {
            Report::build(Kind::Error, 12, "unknown enum variant type", span)
        }
        ErrorKind::UnknownStructField => {
            Report::build(Kind::Error, 13, "unknown struct field reference", span)
        }
        ErrorKind::UnknownEnumVariantTypeInPattern => Report::build(
            Kind::Error,
            14,
            "unknown enum variant type in pattern",
            span,
        ),
        ErrorKind::ExpectedEnumInPattern => {
            Report::build(Kind::Error, 15, "expected enum in pattern", span)
        }
        ErrorKind::WrongEnumVariantContainer(_) => {
            Report::build(Kind::Error, 16, "wrong enum variant", span)
        }
        ErrorKind::VariableIsNotMutable => {
            Report::build(Kind::Error, 17, "variable is not mutable", span)
        }
        ErrorKind::ArgumentIsNotMutable => {
            Report::build(Kind::Error, 18, "argument is not mutable", span)
        }
        ErrorKind::UnknownTypeReference => {
            Report::build(Kind::Error, 19, "unknown type reference", span)
        }
        ErrorKind::SemanticError(semantic_error) => build_semantic_error(semantic_error, span),
        ErrorKind::ExpectedOptional => {
            Report::build(Kind::Error, 20, "expected optional type", span)
        }
        ErrorKind::MapKeyTypeMismatch { .. } => {
            Report::build(Kind::Error, 21, "map key type mismatch", span)
        }
        ErrorKind::MapValueTypeMismatch { .. } => {
            Report::build(Kind::Error, 22, "map value type mismatch", span)
        }
        ErrorKind::IncompatibleTypes { expected, found } => {
            Report::build(Kind::Error, 23, "incompatible types", span)
                //.with_label("first_type", a.clone())
                .with_note(&format!(
                    "required_type: {expected}, encountered type: {found}"
                ))
        }
        ErrorKind::IncompatibleTypesForAssignment { expected, found } => {
            Report::build(Kind::Error, 23, "incompatible types for assignment", span)
                //.with_label("first_type", a.clone())
                .with_note(&format!(
                    "required_type: {expected}, encountered type: {found}"
                ))
        }
        ErrorKind::UnknownMemberFunction(checked_type) => {
            Report::build(Kind::Error, 24, "unknown member function", span)
                .with_note(&format!("searched type {checked_type}"))
        }
        ErrorKind::ExpressionsNotAllowedInLetPattern => Report::build(
            Kind::Error,
            25,
            "expressions not allowed in let pattern",
            span,
        ),
        ErrorKind::UnknownField => Report::build(Kind::Error, 26, "unknown field", span),
        ErrorKind::EnumVariantHasNoFields => {
            Report::build(Kind::Error, 27, "enum variant has no fields", span)
        }
        ErrorKind::TooManyTupleFields { .. } => {
            Report::build(Kind::Error, 28, "too many tuple fields", span)
        }
        ErrorKind::ExpectedBooleanExpression => {
            Report::build(Kind::Error, 29, "expected boolean expression", span)
        }
        ErrorKind::NotAnIterator => Report::build(Kind::Error, 30, "not an iterator", span),
        ErrorKind::IntConversionError(_) => {
            Report::build(Kind::Error, 31, "int conversion error", span)
        }
        ErrorKind::FloatConversionError(_) => {
            Report::build(Kind::Error, 32, "float conversion error", span)
        }
        ErrorKind::BoolConversionError => {
            Report::build(Kind::Error, 33, "bool conversion error", span)
        }
        ErrorKind::DuplicateFieldInStructInstantiation(_) => {
            Report::build(Kind::Error, 34, "duplicate field in struct literal", span)
        }
        ErrorKind::NoDefaultImplemented(_resolved_type) => {
            Report::build(Kind::Error, 35, "no default() function", span)
        }
        ErrorKind::GuardHasNoType => Report::build(Kind::Error, 36, "guard has no type", span),
        ErrorKind::NoneNeedsExpectedTypeHint => {
            Report::build(Kind::Error, 37, "none needs expected type hint", span)
        }
        ErrorKind::NotValidLocationStartingPoint => {
            Report::build(Kind::Error, 38, "not valid location starting point", span)
        }
        ErrorKind::CallsCanNotBePartOfChain
        | ErrorKind::UnwrapCanNotBePartOfChain
        | ErrorKind::NoneCoalesceCanNotBePartOfChain => {
            Report::build(Kind::Error, 9904, "chain error", span)
        }
        ErrorKind::InvalidOperatorAfterOptionalChaining => Report::build(
            Kind::Error,
            9905,
            "invalid operator after optional chaining (?)",
            span,
        )
            .with_note("only field access, method calls, or subscripts are allowed after ?"),
        ErrorKind::SelfNotCorrectType => {
            Report::build(Kind::Error, 9901, "self is of wrong type", span)
        }
        ErrorKind::SelfNotCorrectMutableState => {
            Report::build(Kind::Error, 9901, "self is of wrong mutable status", span)
        }
        ErrorKind::CanNotNoneCoalesce => {
            Report::build(Kind::Error, 39, "can not coalesce to none", span)
        }
        &ErrorKind::GuardCanNotHaveMultipleWildcards
        | &ErrorKind::WildcardMustBeLastInGuard
        | &ErrorKind::GuardMustHaveWildcard => {
            Report::build(Kind::Error, 9902, "guard error", span)
        }
        ErrorKind::UnknownModule => Report::build(Kind::Error, 40, "unknown module", span),
        ErrorKind::UnknownSymbol => Report::build(Kind::Error, 41, "unknown symbol", span),
        ErrorKind::NoAssociatedFunction(_, _) => {
            Report::build(Kind::Error, 42, "no associated function", span)
        }
        ErrorKind::MissingSubscriptMember => {
            Report::build(Kind::Error, 43, "missing subscript member", span)
        }
        ErrorKind::UnusedVariablesCanNotBeMut => {
            Report::build(Kind::Error, 44, "unused variables must not be mut", span)
        }
        ErrorKind::UnknownIdentifier(x) => {
            Report::build(Kind::Error, 45, "unknown identifier", span)
                .with_note(&format!("identifier: {x}"))
        }
        ErrorKind::VariableTypeMustBeBlittable(encountered_type) => {
            Report::build(Kind::Error, 46, "variable type must be blittable", span)
                .with_note(&format!("encountered_type: {encountered_type}"))
        }
        ErrorKind::ArrayIndexMustBeInt(_) => {
            Report::build(Kind::Error, 47, "array index must be int", span)
        }
        ErrorKind::ExpectedMutableLocation => {
            Report::build(Kind::Error, 48, "expected mutable location", span)
        }
        ErrorKind::OverwriteVariableNotAllowedHere => {
            Report::build(Kind::Error, 49, "overwrite variable not allowed here", span)
        }
        ErrorKind::BreakOutsideLoop => Report::build(Kind::Error, 50, "break outside loop", span),
        ErrorKind::ReturnOutsideCompare => {
            Report::build(Kind::Error, 51, "return outside compare", span)
        }
        ErrorKind::EmptyMatch => Report::build(Kind::Error, 52, "empty match", span),
        ErrorKind::MatchArmsMustHaveTypes => {
            Report::build(Kind::Error, 53, "match arms must have types", span)
        }
        ErrorKind::ContinueOutsideLoop => {
            Report::build(Kind::Error, 54, "continue outside loop", span)
        }
        ErrorKind::ParameterIsNotMutable => {
            Report::build(Kind::Error, 55, "parameter is not mutable", span)
        }
        ErrorKind::CouldNotCoerceTo(_) => {
            Report::build(Kind::Error, 56, "could not coerce to", span)
        }
        ErrorKind::UnexpectedType => Report::build(Kind::Error, 57, "unexpected type", span),
        ErrorKind::CanNotAttachFunctionsToType => {
            Report::build(Kind::Error, 58, "can not attach functions to type", span)
        }
        ErrorKind::MissingMemberFunction(missing_func_name, ty) => Report::build(
            Kind::Error,
            59,
            &format!("missing member function {missing_func_name} on type {ty}"),
            span,
        ),
        ErrorKind::ExpectedSlice => Report::build(Kind::Error, 59, "expected slice", span),
        ErrorKind::OperatorProblem => Report::build(Kind::Error, 59, "operator problem", span),
        ErrorKind::MatchMustHaveAtLeastOneArm => {
            Report::build(Kind::Error, 59, "must have at least one match arm", span)
        }
    };
    b.error_module = "A".to_string();
    b
}

/// # Panics
///
pub fn show_analyzer_error(
    err: &Error,
    report_kind: Kind,
    source_map: &SourceMap,
    current_dir: &Path,
) {
    let mut builder = build_analyzer_error(err);
    builder.kind = report_kind;
    build_and_print(builder, source_map, current_dir);
}
