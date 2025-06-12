/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
mod util;
use crate::util::check;
/*
#[test_log::test]
fn init_var() {
    check(
        "a = 3",
        r"

Unit,VariableDefinition(Variable { name: <0:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<4:1>Int,Literal(IntLiteral(3))), is_mutable: None })

",
    );
}

#[test_log::test]
fn fail_compound_unknown_variable() {
    check_fail(
        "a += 3",
        r"
UnknownVariable(<0:1>)",
    );
}

#[test_log::test]
fn fail_overwrite_variable() {
    check_fail(
        r"
        a = 3
        a = 4
        ",
        r"
CanOnlyOverwriteVariableWithMut(<23:1>)",
    );
}

#[test_log::test]
fn fail_reassign_different_types() {
    check_fail(
        r"
        mut a = 3
        a = 'test'
        ",
        r"
OverwriteVariableWithAnotherType(<27:1>)",
    );
}

#[test_log::test]
fn array_push() {
    check(
        r"
        mut a = [1]
        a += 3
        ",
        r"
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(1, <18:1>, ResolvedIntType))])) })
ArrayPush(ResolvedVariable { name: <13:1>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, Literal(IntLiteral(3, <34:1>, ResolvedIntType)))

",
    );
}

#[test_log::test]
fn array_extend() {
    check(
        r"
        mut a = [1]
        a += [3, 4, 5]
        ",
        r"

..[Int],VariableDefinition(ResolvedVariable { name: <13:1>, resolved_type: [Int], mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<17:3>[Int],Literal(Array(ResolvedArrayType { item_type: Int }, [<18:1>Int,Literal(IntLiteral(1))]))), is_mutable: None })
..(),CompoundAssignment(ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression { kind: MutVariableRef, node: <29:1>, ty: [Int], starting_variable: ResolvedVariable { name: <13:1>, resolved_type: [Int], mutable_node: Some(<9:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), Add, <34:9>[Int],Literal(Array(ResolvedArrayType { item_type: Int }, [<35:1>Int,Literal(IntLiteral(3)), <38:1>Int,Literal(IntLiteral(4)), <41:1>Int,Literal(IntLiteral(5))])))

",
    );
}

#[test_log::test]
fn struct_def() {
    check(
        r"
        struct Something {
            a: Int,
            b: Int,
        }

        Something { b: 3, a: 4 }
        ",
        r#"

StructType(RefCell { value: ResolvedStructType { name: <16:9>, assigned_name: "Something", anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: Some(<40:1>), field_type: Int(ResolvedIntType) }, "b": ResolvedAnonymousStructFieldType { identifier: Some(<60:1>), field_type: Int(ResolvedIntType) }) }, functions: SeqMap() } })
---
StructInstantiation(ResolvedStructInstantiation { source_order_expressions: [(1, Literal(IntLiteral(3, <102:1>, ResolvedIntType))), (0, Literal(IntLiteral(4, <108:1>, ResolvedIntType)))], struct_type_ref: RefCell { value: ResolvedStructType { name: <16:9>, assigned_name: "Something", anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: Some(<40:1>), field_type: Int(ResolvedIntType) }, "b": ResolvedAnonymousStructFieldType { identifier: Some(<60:1>), field_type: Int(ResolvedIntType) }) }, functions: SeqMap() } } })

"#,
    );
}

#[test_log::test]
fn add_fn() {
    check(
        r"
fn add(a: Int, b: Int) -> Int {
    a+b
}
add(2, 3)
        ",
        r#"

FunctionDef(Internal(ResolvedInternalFunctionDefinition { body: Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <8:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: VariableAccess(ResolvedVariable { name: <16:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 1 }), kind: Add, node: <38:1>, resolved_type: Int(ResolvedIntType) })]), name: ResolvedLocalIdentifier(<4:3>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }, ResolvedTypeForParameter { name: "b", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Int(ResolvedIntType) }, constants: [] }))
---
FunctionCall(FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }, ResolvedTypeForParameter { name: "b", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Int(ResolvedIntType) }, InternalFunctionAccess(ResolvedInternalFunctionDefinition { body: Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <8:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: VariableAccess(ResolvedVariable { name: <16:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 1 }), kind: Add, node: <38:1>, resolved_type: Int(ResolvedIntType) })]), name: ResolvedLocalIdentifier(<4:3>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }, ResolvedTypeForParameter { name: "b", resolved_type: Int(ResolvedIntType), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Int(ResolvedIntType) }, constants: [] }), [Literal(IntLiteral(2, <47:1>, ResolvedIntType)), Literal(IntLiteral(3, <50:1>, ResolvedIntType))])

"#,
    );
}

#[test_log::test]
fn add_enum() {
    check(
        r"

enum Test {
    Simple,
    Tuple(Int, Int)
}

a = Test::Simple

        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Simple", number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<30:5>), assigned_name: "Tuple", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<30:5>), assigned_name: "Tuple", number: 2 }])
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <49:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Simple", number: 0 }, Nothing)) })

"#,
    );
}

#[test_log::test]
fn add_enum_struct() {
    check(
        r"

enum Test {
    Struct { a: Int, b: Float },
    Simple,
        Tuple(Int, Int)

}

a = Test::Struct { a: 10, b: 2.3 }

        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Struct", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: Some(<27:1>), field_type: Int(ResolvedIntType) }, "b": ResolvedAnonymousStructFieldType { identifier: Some(<35:1>), field_type: Float(ResolvedFloatType) }) } }), name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Struct", number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<51:6>), assigned_name: "Simple", number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 3, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<67:5>), assigned_name: "Tuple", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 } }, fields_in_order: [Int(ResolvedIntType), Int(ResolvedIntType)] }), name: ResolvedLocalTypeIdentifier(<67:5>), assigned_name: "Tuple", number: 3 }])
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <87:1>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Struct", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<7:4>), assigned_name: "Test", module_path: ["test"], number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("a": ResolvedAnonymousStructFieldType { identifier: Some(<27:1>), field_type: Int(ResolvedIntType) }, "b": ResolvedAnonymousStructFieldType { identifier: Some(<35:1>), field_type: Float(ResolvedFloatType) }) } }), name: ResolvedLocalTypeIdentifier(<18:6>), assigned_name: "Struct", number: 2 }, Struct([(0, Literal(IntLiteral(10, <109:2>, ResolvedIntType))), (1, Literal(FloatLiteral(fp:2.300 (150732), <116:3>, ResolvedFloatType)))]))) })

"#,
    );
}

#[test_log::test]
fn fail_enum_struct() {
    check_fail(
        r"

enum Test {
    Struct { a: Int, b: Float },
    Simple,
        Tuple(Int, Int)

}

a = Test::Struct { a: 10, b: 2 }

        ",
        r"

ExpressionIsOfWrongFieldType(<113:1 (65535)>, Float(ResolvedFloatType), Int(ResolvedIntType))
",
    );
}

#[test_log::test]
fn integer() {
    check(
        r"
a = 3
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(3, <5:1>, ResolvedIntType)) })

",
    );
}

#[test_log::test]
fn float() {
    check(
        r"
a = -44.4
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Float(ResolvedFloatType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: UnaryOp(ResolvedUnaryOperator { left: Literal(FloatLiteral(fp:44.400 (2909798), <6:4>, ResolvedFloatType)), kind: Negate, resolved_type: Float(ResolvedFloatType), node: <5:1> }) })

",
    );
}

#[test_log::test]
fn string() {
    check(
        r"
b = 'world'
a = 'hello {b}'
        ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: InterpolatedString(ResolvedStringType, [Literal(<6:5>, "world")]) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <13:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: InterpolatedString(ResolvedStringType, [Literal(<18:6>, "hello "), Interpolation(VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: String(ResolvedStringType), mutable_node: None, scope_index: 0, variable_index: 0 }), None)]) })

"#,
    );
}

#[test_log::test]
fn map() {
    check(
        r#"
b = [2: "something", 3: "other"]
b[3]
        "#,
        r#"

..[Int:String],VariableDefinition(ResolvedVariable { name: <1:1>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<5:28>[Int:String],Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(<6:1>Int,Literal(IntLiteral(2)), <9:11>String,Literal(StringLiteral("something"))), (<22:1>Int,Literal(IntLiteral(3)), <25:7>String,Literal(StringLiteral("other")))]))), is_mutable: None })
..String?,PostfixChain(Variable(ResolvedVariable { name: <1:1>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }), [ResolvedPostfix { node: <36:1>, ty: String?, kind: MapIndex(ResolvedMapType { key_type: Int, value_type: String }, <36:1>Int,Literal(IntLiteral(3))) }])

"#,
    );
}

#[test_log::test]
fn map_set() {
    check(
        r#"
mut b = [2: "something", 3: "other"]
b[3] = "hello"
        "#,
        r#"

..[Int:String],VariableDefinition(ResolvedVariable { name: <5:1>, resolved_type: [Int:String], mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<9:28>[Int:String],Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(<10:1>Int,Literal(IntLiteral(2)), <13:11>String,Literal(StringLiteral("something"))), (<26:1>Int,Literal(IntLiteral(3)), <29:7>String,Literal(StringLiteral("other")))]))), is_mutable: None })
..(),MapAssignment(ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression { kind: MutVariableRef, node: <38:1>, ty: [Int:String], starting_variable: ResolvedVariable { name: <5:1>, resolved_type: [Int:String], mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), <40:1>Int,Literal(IntLiteral(3)), <45:7>String,Literal(StringLiteral("hello")))

"#,
    );
}

#[test_log::test]
fn tuple() {
    check(
        r#"
mut b = (2.4, "hello", 4)
        "#,
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Tuple(ResolvedTupleType([Float(ResolvedFloatType), String(ResolvedStringType), Int(ResolvedIntType)])), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(TupleLiteral(ResolvedTupleType([Float(ResolvedFloatType), String(ResolvedStringType), Int(ResolvedIntType)]), [Literal(FloatLiteral(fp:2.400 (157286), <10:3>, ResolvedFloatType)), Literal(StringLiteral("hello", <15:7>, ResolvedStringType)), Literal(IntLiteral(4, <24:1>, ResolvedIntType))])) })

"#,
    );
}

#[test_log::test]
fn fail_if_condition() {
    check_fail(
        r"
if 2 {
 5
} else {
 6
}
        ",
        r"
ExpectedBooleanExpression(<4:1 (65535)>)
",
    );
}

#[test_log::test]
fn if_condition() {
    check(
        r"
if true {
 5
} else {
 6
}
        ",
        r"
Int,If(ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <4:4>, kind: Literal(BoolLiteral(true)) } }, ResolvedExpression { ty: Int, node: <9:6>, kind: Block([ResolvedExpression { ty: Int, node: <12:1>, kind: Literal(IntLiteral(5)) }]) }, Some(ResolvedExpression { ty: Int, node: <21:6>, kind: Block([ResolvedExpression { ty: Int, node: <24:1>, kind: Literal(IntLiteral(6)) }]) }))

",
    );
}

#[test_log::test]
fn for_loop() {
    check(
        r"
mut c = 0
for a in 0..3 {
    c += a
}
        ",
        r"

..Int,VariableDefinition(ResolvedVariable { name: <5:1>, resolved_type: Int, mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<9:1>Int,Literal(IntLiteral(0))), is_mutable: None })
..(),ForLoop(Single(ResolvedVariable { name: <15:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterable { key_type: None, value_type: Int, resolved_expression: ResolvedMutOrImmutableExpression { expression_or_location: Expression(<20:5>Iterable<Int>,ExclusiveRange(<20:1>Int,Literal(IntLiteral(0)), <23:1>Int,Literal(IntLiteral(3)))), is_mutable: None } }, <25:14>(),Block([<31:1>(),CompoundAssignment(ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression { kind: MutVariableRef, node: <31:1>, ty: Int, starting_variable: ResolvedVariable { name: <5:1>, resolved_type: Int, mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), Add, <36:1>Int,VariableAccess(ResolvedVariable { name: <15:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }))]))

",
    );
}

#[test_log::test]
fn for_loop_inclusive() {
    check(
        r"
mut c = 0
for a in 0..=3 {
    c += a
}
        ",
        r"

..Int,VariableDefinition(ResolvedVariable { name: <5:1>, resolved_type: Int, mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<9:1>Int,Literal(IntLiteral(0))), is_mutable: None })
..(),ForLoop(Single(ResolvedVariable { name: <15:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterable { key_type: None, value_type: Int, resolved_expression: ResolvedMutOrImmutableExpression { expression_or_location: Expression(<20:6>Iterable<Int>,InclusiveRange(<20:1>Int,Literal(IntLiteral(0)), <24:1>Int,Literal(IntLiteral(3)))), is_mutable: None } }, <26:14>(),Block([<32:1>(),CompoundAssignment(ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression { kind: MutVariableRef, node: <32:1>, ty: Int, starting_variable: ResolvedVariable { name: <5:1>, resolved_type: Int, mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), Add, <37:1>Int,VariableAccess(ResolvedVariable { name: <15:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }))]))

",
    );
}

#[test_log::test]
fn for_loop_arr() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for a in arr {
    c += a
}
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) })
ForLoop(Single(ResolvedVariable { name: <34:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }) }, Block([VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <34:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), compound_operator: ResolvedCompoundOperator { node: <51:2>, kind: Add } })]))

",
    );
}

#[test_log::test]
fn for_loop_arr_index() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for index, a in arr {
    c += a
}
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) })
ForLoop(Pair(ResolvedVariable { name: <34:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }) }, Block([VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), compound_operator: ResolvedCompoundOperator { node: <58:2>, kind: Add } })]))

",
    );
}

#[test_log::test]
fn for_loop_mut_arr_index() {
    check(
        r"
arr = [40, 50, 60]
mut c = 0
for index, a in arr {
    c += a
}
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }, [Literal(IntLiteral(40, <8:2>, ResolvedIntType)), Literal(IntLiteral(50, <12:2>, ResolvedIntType)), Literal(IntLiteral(60, <16:2>, ResolvedIntType))])) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(0, <28:1>, ResolvedIntType)) })
ForLoop(Pair(ResolvedVariable { name: <34:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), ResolvedIterator { key_type: Some(Int(ResolvedIntType)), value_type: Int(ResolvedIntType), resolved_expression: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: Array(ResolvedArrayType { item_type: Int(ResolvedIntType) }), mutable_node: None, scope_index: 0, variable_index: 0 }) }, Block([VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <24:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, expression: VariableAccess(ResolvedVariable { name: <41:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 1 }), compound_operator: ResolvedCompoundOperator { node: <58:2>, kind: Add } })]))

",
    );
}

#[test_log::test]
fn for_loop_break() {
    check(
        r"
mut c = 0
for a in 0..3 {
    c += a
    break
}
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(0, <9:1>, ResolvedIntType)) })
ForLoop(Single(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), ResolvedIterator { key_type: None, value_type: Int(ResolvedIntType), resolved_expression: ExclusiveRange(ResolvedExclusiveRangeType, Literal(IntLiteral(0, <20:1>, ResolvedIntType)), Literal(IntLiteral(3, <23:1>, ResolvedIntType))) }, Block([VariableCompoundAssignment(ResolvedVariableCompoundAssignment { variable_ref: ResolvedVariable { name: <5:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<1:3>), scope_index: 0, variable_index: 0 }, expression: VariableAccess(ResolvedVariable { name: <15:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }), compound_operator: ResolvedCompoundOperator { node: <33:2>, kind: Add } }), Break(<42:5>)]))

",
    );
}

#[test_log::test]
fn match_enum() {
    check(
        r"
enum GameState {
    Playing,
    Paused,
    GameOver,
}

game_state = GameState::Paused

match game_state {
    Playing => 1,
    Paused => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 3 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<47:8>), assigned_name: "GameOver", number: 4 }])
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <60:10>, resolved_type: "GameState", mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 3 }, Nothing)) })
Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: Normal(EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 2 }, None), None), expression: Literal(IntLiteral(1, <126:1>)), expression_type: Int }, ResolvedMatchArm { pattern: Normal(EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 3 }, None), None), expression: Literal(IntLiteral(2, <143:1>)), expression_type: Int }, ResolvedMatchArm { pattern: Normal(EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<47:8>), assigned_name: "GameOver", number: 4 }, None), None), expression: Literal(IntLiteral(3, <162:1>)), expression_type: Int }, ResolvedMatchArm { pattern: Wildcard(<169:1>), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <175:1>)), kind: Negate, resolved_type: Int, node: <174:1> }), expression_type: Int }], expression: VariableAccess(ResolvedVariable { name: <60:10>, resolved_type: "GameState", mutable_node: None, scope_index: 0, variable_index: 0 }) })

"#,
    );
}

#[test_log::test]
fn match_enum_2() {
    check(
        r"
enum GameState {
    Playing,
    Paused { time: Int },
    GameOver,
}

game_state = GameState::Paused

match game_state {
    Playing => 1,
    Paused time => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: Some(<44:4>), field_type: Int(ResolvedIntType) }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<61:8>), assigned_name: "GameOver", number: 0 }])
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <74:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: Some(<44:4>), field_type: Int(ResolvedIntType) }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, Nothing)) })
Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 0 }, None), expression: Literal(IntLiteral(1, <140:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Struct(ResolvedEnumVariantStructType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, anon_struct: ResolvedAnonymousStructType { defined_fields: SeqMap("time": ResolvedAnonymousStructFieldType { identifier: Some(<44:4>), field_type: Int(ResolvedIntType) }) } }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, Some([VariableWithFieldIndex(ResolvedVariable { name: <154:4>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 1, variable_index: 0 }, 0)])), expression: Literal(IntLiteral(2, <162:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<61:8>), assigned_name: "GameOver", number: 0 }, None), expression: Literal(IntLiteral(3, <181:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<188:1>)]), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <194:1>, ResolvedIntType)), kind: Negate, resolved_type: Int(ResolvedIntType), node: <193:1> }), expression_type: Int(ResolvedIntType) }], expression: VariableAccess(ResolvedVariable { name: <74:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }) })

"#,
    );
}

#[test_log::test]
fn match_enum_3() {
    check(
        r"
enum GameState {
    Playing,
    Paused (Float, Float),
    GameOver,
}

game_state = GameState::Paused

match game_state {
    Playing => 1,
    Paused time, _ => 2,
    GameOver => 3,
    _ => -1
}
        ",
        r#"

EnumType(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, [ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 0 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<62:8>), assigned_name: "GameOver", number: 0 }])
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <75:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(EnumVariantLiteral(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, Nothing)) })
Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<22:7>), assigned_name: "Playing", number: 0 }, None), expression: Literal(IntLiteral(1, <141:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Tuple(ResolvedEnumVariantTupleType { common: CommonEnumVariantType { number: 2, module_path: ResolvedModulePath([]), variant_name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", enum_ref: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 } }, fields_in_order: [Float(ResolvedFloatType), Float(ResolvedFloatType)] }), name: ResolvedLocalTypeIdentifier(<35:6>), assigned_name: "Paused", number: 2 }, Some([Variable(ResolvedVariable { name: <155:4>, resolved_type: Float(ResolvedFloatType), mutable_node: None, scope_index: 1, variable_index: 0 }), Wildcard(<161:1>)])), expression: Literal(IntLiteral(2, <166:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: EnumPattern(ResolvedEnumVariantType { owner: ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }, data: Nothing, name: ResolvedLocalTypeIdentifier(<62:8>), assigned_name: "GameOver", number: 0 }, None), expression: Literal(IntLiteral(3, <185:1>, ResolvedIntType)), expression_type: Int(ResolvedIntType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<192:1>)]), expression: UnaryOp(ResolvedUnaryOperator { left: Literal(IntLiteral(1, <198:1>, ResolvedIntType)), kind: Negate, resolved_type: Int(ResolvedIntType), node: <197:1> }), expression_type: Int(ResolvedIntType) }], expression: VariableAccess(ResolvedVariable { name: <75:10>, resolved_type: Enum(ResolvedEnumType { name: ResolvedLocalTypeIdentifier(<6:9>), assigned_name: "GameState", module_path: ["test"], number: 1 }), mutable_node: None, scope_index: 0, variable_index: 0 }) })

"#,
    );
}

#[test_log::test]
fn match_value() {
    check(
        r"
score = 2

match score {
    2 => 'low',
    _ => 'high',
}
        ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(2, <9:1>, ResolvedIntType)) })
Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: Literal(IntLiteral(2, <30:1>, ResolvedIntType)), expression: InterpolatedString(ResolvedStringType, [Literal(<36:3>, "low")]), expression_type: String(ResolvedStringType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<46:1>)]), expression: InterpolatedString(ResolvedStringType, [Literal(<52:4>, "high")]), expression_type: String(ResolvedStringType) }], expression: VariableAccess(ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }) })

"#,
    );
}

/*
#[test_log::test]
fn match_value_struct() {
    check(
        r"
struct Cool {
    score: Int,
    other: Int,
}

let cool = Cool { score: 99, other: 99 }

match cool {
    other: 100, score => score,
    _ => -11,
}
        ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(2, <9:1>, ResolvedIntType)) }))
Match(ResolvedMatch { arms: [ResolvedMatchArm { pattern: Literal(IntLiteral(2, <38:1>, ResolvedIntType)), expression: InterpolatedString(ResolvedStringType, [Literal(<44:3>, "low")]), expression_type: String(ResolvedStringType) }, ResolvedMatchArm { pattern: PatternList([Wildcard(<54:1>)]), expression: InterpolatedString(ResolvedStringType, [Literal(<60:4>, "high")]), expression_type: String(ResolvedStringType) }], expression: VariableAccess(ResolvedVariable { name: <1:5>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }) }))

"#,
    );
}
 */

#[test_log::test]
fn binary_op() {
    check(
        r"
a = 44
b = a > 43 && false
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(44, <5:2>, ResolvedIntType)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <8:1>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: BinaryOp(ResolvedBinaryOperator { left: BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: Literal(IntLiteral(43, <16:2>, ResolvedIntType)), kind: GreaterThan, node: <14:1>, resolved_type: Bool(ResolvedBoolType) }), right: Literal(BoolLiteral(false, <22:5>, ResolvedBoolType)), kind: LogicalAnd, node: <19:2>, resolved_type: Bool(ResolvedBoolType) }) })

",
    );
}

#[test_log::test]
fn unary_op() {
    check(
        r"
a = 44
b = !(a > 43)
        ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(44, <5:2>, ResolvedIntType)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <8:1>, resolved_type: Bool(ResolvedBoolType), mutable_node: None, scope_index: 0, variable_index: 1 }], expression: UnaryOp(ResolvedUnaryOperator { left: BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <1:1>, resolved_type: Int(ResolvedIntType), mutable_node: None, scope_index: 0, variable_index: 0 }), right: Literal(IntLiteral(43, <18:2>, ResolvedIntType)), kind: GreaterThan, node: <16:1>, resolved_type: Bool(ResolvedBoolType) }), kind: Not, resolved_type: Bool(ResolvedBoolType), node: <12:1> }) })

",
    );
}

#[test_log::test]
fn fn_mut_param() {
    check(
        r"
fn changing(mut x: Int) {
    x = 909
}

mut a = 43

changing(mut a)

        ",
        r#"

FunctionDef(Internal(ResolvedInternalFunctionDefinition { body: Block([ReassignVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <17:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<13:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(909, <35:3>, ResolvedIntType)) })]), name: ResolvedLocalIdentifier(<4:8>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "x", resolved_type: Int(ResolvedIntType), is_mutable: true, node: Some(ResolvedParameter) }], return_type: Unit(ResolvedUnitType) }, constants: [] }))
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <46:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<42:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(43, <50:2>, ResolvedIntType)) })
FunctionCall(FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "x", resolved_type: Int(ResolvedIntType), is_mutable: true, node: Some(ResolvedParameter) }], return_type: Unit(ResolvedUnitType) }, InternalFunctionAccess(ResolvedInternalFunctionDefinition { body: Block([ReassignVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <17:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<13:3>), scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(909, <35:3>, ResolvedIntType)) })]), name: ResolvedLocalIdentifier(<4:8>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "x", resolved_type: Int(ResolvedIntType), is_mutable: true, node: Some(ResolvedParameter) }], return_type: Unit(ResolvedUnitType) }, constants: [] }), [MutVariableRef(ResolvedMutVariable { variable_ref: ResolvedVariable { name: <46:1>, resolved_type: Int(ResolvedIntType), mutable_node: Some(<42:3>), scope_index: 0, variable_index: 0 } })])

"#,
    );
}

#[test_log::test]
fn impl_pos() {
    check(
        r"
struct Position {
    x: Float,
    y: Float,
}

impl Position {
    fn new() -> Position {
        Position {
            x: 101.99,
            y: -102.18,
        }
    }

    fn move(mut self, pos: Position) {
        self.x += pos.x
        self.y += pos.y
    }
}

        ",
        r#"

StructType(RefCell { value: ResolvedStructType { name: <8:8>, assigned_name: "Position", anon_struct_type: ResolvedAnonymousStructType { defined_fields: SeqMap("x": ResolvedAnonymousStructFieldType { identifier: Some(<23:1>), field_type: Float }, "y": ResolvedAnonymousStructFieldType { identifier: Some(<37:1>), field_type: Float }) }, functions: SeqMap("new": Internal(FunctionTypeSignature { first_parameter_is_self: false, parameters: [], return_type: Position }), "move": Internal(FunctionTypeSignature { first_parameter_is_self: true, parameters: [ResolvedTypeForParameter { name: "self", resolved_type: Position, is_mutable: true, node: Some(ResolvedParameter) }, ResolvedTypeForParameter { name: "pos", resolved_type: Position, is_mutable: false, node: Some(ResolvedParameter) }], return_type: () })) } })
ImplType(Position)

"#,
    );
}

#[test_log::test]
fn check_some_bug() {
    check(
        r"
        is_attacking = false
        c = if is_attacking { 3.5 } else { -13.3 }
    ",
        r"
..(),CreateVariable(ResolvedVariable { name: <9:12>, resolved_type: Bool, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Bool, node: <24:5>, kind: Literal(BoolLiteral(false)) }))
..(),CreateVariable(ResolvedVariable { name: <38:1>, resolved_type: Float, mutable_node: None, scope_index: 0, variable_index: 1 }, Immutable(ResolvedExpression { ty: Float, node: <45:12>, kind: If(ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <45:12>, kind: VariableAccess(ResolvedVariable { name: <9:12>, resolved_type: Bool, mutable_node: None, scope_index: 0, variable_index: 0 }) } }, ResolvedExpression { ty: Float, node: <58:7>, kind: Block([ResolvedExpression { ty: Float, node: <60:3>, kind: Literal(FloatLiteral(fp:3.500 (229376))) }]) }, Some(ResolvedExpression { ty: Float, node: <71:9>, kind: Block([ResolvedExpression { ty: Float, node: <73:6>, kind: UnaryOp(ResolvedUnaryOperator { left: ResolvedExpression { ty: Float, node: <74:4>, kind: Literal(FloatLiteral(fp:13.300 (871628))) }, kind: Negate, node: <73:1> }) }]) })) }))

",
    );
}

#[test_log::test]
fn create_variable() {
    check(
        r"
        is_attacking = false
        ",
        r"
        (),CreateVariable(ResolvedVariable { name: <9:12>, resolved_type: Bool, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Bool, node: <24:5>, kind: Literal(BoolLiteral(false)) }))

        ",
    );
}

#[test_log::test]
fn tuple_destructuring() {
    check(
        r"
        x, y = (2, 3)
    ",
        r"

TupleDestructuring([ResolvedVariable { name: <9:1>, resolved_type: Tuple(ResolvedTupleType([Int(ResolvedIntType), Int(ResolvedIntType)])), mutable_node: None, scope_index: 0, variable_index: 0 }, ResolvedVariable { name: <12:1>, resolved_type: Tuple(ResolvedTupleType([Int(ResolvedIntType), Int(ResolvedIntType)])), mutable_node: None, scope_index: 0, variable_index: 1 }], ResolvedTupleType([Int(ResolvedIntType), Int(ResolvedIntType)]), Literal(TupleLiteral(ResolvedTupleType([Int(ResolvedIntType), Int(ResolvedIntType)]), [Literal(IntLiteral(2, <17:1>, ResolvedIntType)), Literal(IntLiteral(3, <20:1>, ResolvedIntType))])))

",
    );
}

#[test_log::test]
fn definition_use() {
    check(
        r"
        use gameplay
    ",
        r"
Use(ResolvedUse { path: [<13:8>], items: [] })
",
    );
}

#[test_log::test]
fn definition_use_3() {
    check(
        r"
        use gameplay.other.some
    ",
        r"
Use(ResolvedUse { path: [<13:8>, <22:5>, <28:4>], items: [] })
",
    );
}

#[test_log::test]
fn constant() {
    check(
        r"
        const HELLO=3
    ",
        r#"
Constant(<15:5>, ResolvedConstant { name: <15:5>, assigned_name: "HELLO", id: 0, expr: ResolvedExpression { ty: Int, node: <21:1>, kind: Literal(IntLiteral(3)) }, resolved_type: Int })

"#,
    );
}

#[test_log::test]
fn constant_in_function() {
    check(
        r"
        fn some_fn() {
            const HELLO=3
            3
        }
    ",
        r#"

FunctionDef(Internal(ResolvedInternalFunctionDefinition { body: Block([Literal(IntLiteral(3, <62:1>, ResolvedIntType))]), name: ResolvedLocalIdentifier(<12:7>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [], return_type: Unit(ResolvedUnitType) }, constants: [ResolvedConstant { name: <42:5>, assigned_name: "HELLO", id: 0, expr: Literal(IntLiteral(3, <48:1>, ResolvedIntType)), resolved_type: Int(ResolvedIntType) }] }))

"#,
    );
}

#[test_log::test]
fn constant_access_in_function() {
    check(
        r"
        fn some_fn() -> Int {
            const HELLO=3
            HELLO
            HELLO
        }
    ",
        r#"

FunctionDef(Internal(ResolvedInternalFunctionDefinition { body: Block([ConstantAccess(ResolvedConstant { name: <49:5>, assigned_name: "HELLO", id: 0, expr: Literal(IntLiteral(3, <55:1>, ResolvedIntType)), resolved_type: Int(ResolvedIntType) }), ConstantAccess(ResolvedConstant { name: <49:5>, assigned_name: "HELLO", id: 0, expr: Literal(IntLiteral(3, <55:1>, ResolvedIntType)), resolved_type: Int(ResolvedIntType) })]), name: ResolvedLocalIdentifier(<12:7>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [], return_type: Int(ResolvedIntType) }, constants: [ResolvedConstant { name: <49:5>, assigned_name: "HELLO", id: 0, expr: Literal(IntLiteral(3, <55:1>, ResolvedIntType)), resolved_type: Int(ResolvedIntType) }] }))

"#,
    );
}

#[test_log::test]
fn function_ref() {
    check(
        r"
        fn caller(some_fn: (Int) -> Float)  {
            some_fn(2)
        }
    ",
        r#"
FunctionDef(Internal(ResolvedInternalFunctionDefinition { body: Block([FunctionCall(FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "", resolved_type: Int(ResolvedIntType), is_mutable: false, node: None }], return_type: Float(ResolvedFloatType) }, VariableAccess(ResolvedVariable { name: <19:7>, resolved_type: Function(FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "", resolved_type: Int(ResolvedIntType), is_mutable: false, node: None }], return_type: Float(ResolvedFloatType) }), mutable_node: None, scope_index: 0, variable_index: 0 }), [Literal(IntLiteral(2, <67:1>, ResolvedIntType))])]), name: ResolvedLocalIdentifier(<12:6>), signature: FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "some_fn", resolved_type: Function(FunctionTypeSignature { first_parameter_is_self: false, parameters: [ResolvedTypeForParameter { name: "", resolved_type: Int(ResolvedIntType), is_mutable: false, node: None }], return_type: Float(ResolvedFloatType) }), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Unit(ResolvedUnitType) }, constants: [] }))

"#,
    );
}

#[test_log::test]
fn function_call_basic() {
    check(
        r"
        fn some_fn(a: Int) -> Float {
            2.0
        }
        some_fn(1)
    ",
        r#"

FunctionDef(Internal(FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Some(Int), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Float }
ResolvedExpression { ty: Float, node: <37:27>, kind: Block([ResolvedExpression { ty: Float, node: <51:3>, kind: Literal(FloatLiteral(fp:2.000 (131072))) }]) }))
---
Float,FunctionCall(FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Some(Int), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Float }, ResolvedExpression { ty: FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Some(Int), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Float }, node: <73:7>, kind: InternalFunctionAccess(FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "a", resolved_type: Some(Int), is_mutable: false, node: Some(ResolvedParameter) }], return_type: Float }
ResolvedExpression { ty: Float, node: <37:27>, kind: Block([ResolvedExpression { ty: Float, node: <51:3>, kind: Literal(FloatLiteral(fp:2.000 (131072))) }]) }) }, [Immutable(ResolvedExpression { ty: Int, node: <81:1>, kind: Literal(IntLiteral(1)) })])

"#,
    );
}

#[test_log::test]
fn struct_calls() {
    check(
        r"
        struct Assets {
        }

        impl Assets {
            fn frame_fixed_grid_material_png(self, name: String) {
            }
        }
        fn start(assets: Assets) {
            assets.frame_fixed_grid_material_png('player/ship_16x16')
        }
         ",
        r#"

StructType(RefCell { value: struct "Assets" })
ImplType(Assets)
FunctionDef(Internal(FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "assets", resolved_type: Some(Assets), is_mutable: false, node: Some(ResolvedParameter) }], return_type: () }
ResolvedExpression { ty: (), node: <182:81>, kind: Block([ResolvedExpression { ty: (), node: <196:6>, kind: MemberCall(ResolvedMemberCall { function: Internal(FunctionTypeSignature { parameters: [ResolvedTypeForParameter { name: "self", resolved_type: Some(Assets), is_mutable: false, node: Some(ResolvedParameter) }, ResolvedTypeForParameter { name: "name", resolved_type: Some(String), is_mutable: false, node: Some(ResolvedParameter) }], return_type: () }
ResolvedExpression { ty: (), node: <123:15>, kind: Block([]) }), arguments: [Immutable(ResolvedExpression { ty: Assets, node: <196:6>, kind: VariableAccess(ResolvedVariable { name: <166:6>, resolved_type: Assets, mutable_node: None, scope_index: 0, variable_index: 0 }) }), Immutable(ResolvedExpression { ty: String, node: <233:19>, kind: InterpolatedString([Literal(<234:17>, "player/ship_16x16")]) })] }) }]) }))

"#,
    );
}

#[test_log::test]
fn with_simple() {
    check(
        r"
        x = 4
        y = 5

        with y {
            y + 3
        }
         ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(4, <14:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(5, <28:1>)) })
Block([InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <52:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }], expression: VariableAccess(ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }) }), Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <52:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), right: Literal(IntLiteral(3, <72:1>)), kind: Add, node: <70:1>, resolved_type: Int })])])

",
    );
}

#[test_log::test]
fn with_simple_two() {
    check(
        r"
        x = 4
        y = 5

        with y, x {
            y + 3
        }
         ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(4, <14:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(5, <28:1>)) })
Block([InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <52:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }], expression: VariableAccess(ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }) }), InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <55:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 1 }], expression: VariableAccess(ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }) }), Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <52:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), right: Literal(IntLiteral(3, <75:1>)), kind: Add, node: <73:1>, resolved_type: Int })])])

",
    );
}

#[test_log::test]
fn with_simple_assign() {
    check(
        r"
        x = 4
        y = 5

        a = with y {
            y + 3
        }
         ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(4, <14:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(5, <28:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <46:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 2 }], expression: Block([InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <55:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }], expression: VariableAccess(ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }) }), Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <55:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), right: Literal(IntLiteral(3, <75:1>)), kind: Add, node: <73:1>, resolved_type: Int })])]) })

",
    );
}

#[test_log::test]
fn with_simple_multi_assign() {
    check(
        r"
        x = 4
        y = 5

        a = with y, x {
            y + 3
        }
         ",
        r"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(IntLiteral(4, <14:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: Literal(IntLiteral(5, <28:1>)) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <47:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 2 }], expression: Block([InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <56:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }], expression: VariableAccess(ResolvedVariable { name: <24:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }) }), InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <59:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 1 }], expression: VariableAccess(ResolvedVariable { name: <10:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }) }), Block([BinaryOp(ResolvedBinaryOperator { left: VariableAccess(ResolvedVariable { name: <56:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }), right: Literal(IntLiteral(3, <79:1>)), kind: Add, node: <77:1>, resolved_type: Int })])]) })

",
    );
}

#[test_log::test]
fn with_simple_multi_assign_shadow() {
    check(
        r"
        x = 4
        y = 5

        a = with x {
            x + 3
            y = 3
            y
        }
         ",
        r"

..(),CreateVariable(ResolvedVariable { name: <9:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Int, node: <13:1>, kind: Literal(IntLiteral(4)) }))
..(),CreateVariable(ResolvedVariable { name: <23:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }, Immutable(ResolvedExpression { ty: Int, node: <27:1>, kind: Literal(IntLiteral(5)) }))
..(),CreateVariable(ResolvedVariable { name: <38:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 2 }, Immutable(ResolvedExpression { ty: Int, node: <49:61>, kind: Block([ResolvedExpression { ty: Int, node: <47:1>, kind: CreateVariable(ResolvedVariable { name: <47:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }, Immutable(ResolvedExpression { ty: Int, node: <47:1>, kind: VariableAccess(ResolvedVariable { name: <9:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }) })) }, ResolvedExpression { ty: Int, node: <49:61>, kind: Block([ResolvedExpression { ty: Int, node: <63:18>, kind: BinaryOp(ResolvedBinaryOperator { left: ResolvedExpression { ty: Int, node: <63:1>, kind: VariableAccess(ResolvedVariable { name: <47:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }) }, right: ResolvedExpression { ty: Int, node: <67:1>, kind: Literal(IntLiteral(3)) }, kind: Add, node: <65:1> }) }, ResolvedExpression { ty: (), node: <85:1>, kind: CreateVariable(ResolvedVariable { name: <81:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 1 }, Immutable(ResolvedExpression { ty: Int, node: <85:1>, kind: Literal(IntLiteral(3)) })) }, ResolvedExpression { ty: Int, node: <99:1>, kind: VariableAccess(ResolvedVariable { name: <81:1>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 1 }) }]) }]) }))


",
    );
}

#[test_log::test]
fn guard_single() {
    check(
        r"
        a = 3

        | a > 2 -> 'hello'

         ",
        r#"

..(),CreateVariable(ResolvedVariable { name: <9:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Int, node: <13:1>, kind: Literal(IntLiteral(3)) }))
..String,Guard([ResolvedGuard { condition: ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <26:6>, kind: BinaryOp(ResolvedBinaryOperator { left: ResolvedExpression { ty: Int, node: <26:1>, kind: VariableAccess(ResolvedVariable { name: <9:1>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 0 }) }, right: ResolvedExpression { ty: Int, node: <30:1>, kind: Literal(IntLiteral(2)) }, kind: GreaterThan, node: <28:1> }) } }, result: ResolvedExpression { ty: String, node: <35:7>, kind: InterpolatedString([Literal(<36:5>, "hello")]) } }], None)

"#,
    );
}

#[test_log::test]
fn guard_multi() {
    check(
        r"
        | 3 > 2 -> 'hello'
        | 3 != 0 -> 'goodbye'
        | _ -> 'not sure'
         ",
        r#"

String,Guard([ResolvedGuard { condition: ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <11:6>, kind: BinaryOp(ResolvedBinaryOperator { left: ResolvedExpression { ty: Int, node: <11:1>, kind: Literal(IntLiteral(3)) }, right: ResolvedExpression { ty: Int, node: <15:1>, kind: Literal(IntLiteral(2)) }, kind: GreaterThan, node: <13:1> }) } }, result: ResolvedExpression { ty: String, node: <20:7>, kind: InterpolatedString([Literal(<21:5>, "hello")]) } }, ResolvedGuard { condition: ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <38:7>, kind: BinaryOp(ResolvedBinaryOperator { left: ResolvedExpression { ty: Int, node: <38:1>, kind: Literal(IntLiteral(3)) }, right: ResolvedExpression { ty: Int, node: <43:1>, kind: Literal(IntLiteral(0)) }, kind: NotEqual, node: <40:2> }) } }, result: ResolvedExpression { ty: String, node: <48:9>, kind: InterpolatedString([Literal(<49:7>, "goodbye")]) } }], Some(ResolvedExpression { ty: String, node: <73:10>, kind: InterpolatedString([Literal(<74:8>, "not sure")]) }))

"#,
    );
}

#[test_log::test]
fn guard_multi_assign() {
    check(
        r"
        a =
            | 3 > 2 -> 'hello'
            | 3 != 0 -> 'goodbye'
            | _ -> 'you say'
         ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <9:1>, resolved_type: String, mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Guard([ResolvedGuard { condition: ResolvedBooleanExpression { expression: BinaryOp(ResolvedBinaryOperator { left: Literal(IntLiteral(3, <27:1>)), right: Literal(IntLiteral(2, <31:1>)), kind: GreaterThan, node: <29:1>, resolved_type: Bool }) }, result: InterpolatedString([Literal(<37:5>, "hello")]) }, ResolvedGuard { condition: ResolvedBooleanExpression { expression: BinaryOp(ResolvedBinaryOperator { left: Literal(IntLiteral(3, <58:1>)), right: Literal(IntLiteral(0, <63:1>)), kind: NotEqual, node: <60:2>, resolved_type: Bool }) }, result: InterpolatedString([Literal(<69:7>, "goodbye")]) }], Some(InterpolatedString([Literal(<98:7>, "you say")]))) })

"#,
    );
}

#[test_log::test]
fn array_range_access() {
    check(
        r"
        arr = [2, 3, 4, 5, 6]
        arr[3..4]
         ",
        r"

..(),VariableDefinition(ResolvedVariable { name: <9:3>, resolved_type: [Int], mutable_node: None, scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<15:15>[Int],Literal(Array(ResolvedArrayType { item_type: Int }, [<16:1>Int,Literal(IntLiteral(2)), <19:1>Int,Literal(IntLiteral(3)), <22:1>Int,Literal(IntLiteral(4)), <25:1>Int,Literal(IntLiteral(5)), <28:1>Int,Literal(IntLiteral(6))]))), is_mutable: None })
..[Int],PostfixChain(<39:3>[Int],VariableAccess(ResolvedVariable { name: <9:3>, resolved_type: [Int], mutable_node: None, scope_index: 0, variable_index: 0 }), [ResolvedPostfix { node: <43:4>, ty: [Int], kind: ArrayRangeIndex(ResolvedArrayType { item_type: Int }, <43:4>Iterable<Int>,Range(<43:1>Int,Literal(IntLiteral(3)), <46:1>Int,Literal(IntLiteral(4)), Exclusive)) }])


",
    );
}

#[test_log::test]
fn array_range_access_expr_exclusive() {
    check(
        r"
        fn some_fn() -> Int {
            1
        }

        a = [2, 4]
        some_var = 0
        a[some_fn()+33..some_var]
         ",
        r"

FunctionDef(Internal(FunctionTypeSignature { parameters: [], return_type: Int }
<29:25>Int,Block([<43:1>Int,Literal(IntLiteral(1))])))
---
..(),VariableDefinition(ResolvedVariable { name: <64:1>, resolved_type: [Int], mutable_node: None, scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<68:6>[Int],Literal(Array(ResolvedArrayType { item_type: Int }, [<69:1>Int,Literal(IntLiteral(2)), <72:1>Int,Literal(IntLiteral(4))]))), is_mutable: None })
..(),VariableDefinition(ResolvedVariable { name: <83:8>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<94:1>Int,Literal(IntLiteral(0))), is_mutable: None })
..[Int],PostfixChain(<104:1>[Int],VariableAccess(ResolvedVariable { name: <64:1>, resolved_type: [Int], mutable_node: None, scope_index: 0, variable_index: 0 }), [ResolvedPostfix { node: <106:22>, ty: [Int], kind: ArrayRangeIndex(ResolvedArrayType { item_type: Int }, <106:22>Iterable<Int>,Range(<106:12>Int,BinaryOp(ResolvedBinaryOperator { left: <106:7>Int,PostfixChain(<106:7>function ()->Int,InternalFunctionAccess(FunctionTypeSignature { parameters: [], return_type: Int }
<29:25>Int,Block([<43:1>Int,Literal(IntLiteral(1))])), [ResolvedPostfix { node: <113:2>, ty: Int, kind: FunctionCall([]) }]), right: <116:2>Int,Literal(IntLiteral(33)), kind: Add, node: <115:1> }), <120:8>Int,VariableAccess(ResolvedVariable { name: <83:8>, resolved_type: Int, mutable_node: None, scope_index: 0, variable_index: 1 }), Exclusive)) }])

",
    );
}

#[test_log::test]
fn string_range_access() {
    check(
        r"
        a = 'some string'
        a[2..4]
         ",
        r#"

..(),VariableDefinition(ResolvedVariable { name: <9:1>, resolved_type: String, mutable_node: None, scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<13:13>String,InterpolatedString([Literal(<14:11>, "some string")])), is_mutable: None })
..String,PostfixChain(<35:1>String,VariableAccess(ResolvedVariable { name: <9:1>, resolved_type: String, mutable_node: None, scope_index: 0, variable_index: 0 }), [ResolvedPostfix { node: <37:4>, ty: String, kind: StringRangeIndex(ResolvedRange { min: <37:1>Int,Literal(IntLiteral(2)), max: <40:1>Int,Literal(IntLiteral(4)), mode: Exclusive }) }])

"#,
    );
}

#[test_log::test]
fn string_iterable() {
    check(
        r"
        a = 'some string'
        for y in a {
        }
         ",
        r"


",
    );
}

#[test_log::test]
fn option_assign() {
    check(
        r"
        map = [2: 'hello']
        a = map[3]
        when x = a {
            //x
        }
         ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(Literal(IntLiteral(2, <16:1>)), InterpolatedString([Literal(<20:5>, "hello")]))])) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: MapIndexAccess(ResolvedMapIndexLookup { map_type: [Int:String], item_type: String, map_type_ref: ResolvedMapType { key_type: Int, value_type: String }, index_expression: Literal(IntLiteral(3, <44:1>)), map_expression: VariableAccess(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }) }) })
IfAssignExpression { variable: ResolvedVariable { name: <58:1>, resolved_type: String, mutable_node: None, scope_index: 1, variable_index: 0 }, optional_expr: VariableAccess(ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }), true_block: Block([]), false_block: None }

"#,
    );
}

#[test_log::test]
fn option_assign_mut() {
    check(
        r"
map = [2: 'hello']
mut a = map[2]
if another = a? {
    print('before: {another}')
    another = another + ' goodbye'
    print('after: {another}')
} else {
    print('not found')
}
         ",
        r#"

InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }], expression: Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(Literal(IntLiteral(2, <16:1>)), InterpolatedString([Literal(<20:5>, "hello")]))])) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: [ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }], expression: MapIndexAccess(ResolvedMapIndexLookup { map_type: [Int:String], item_type: String, map_type_ref: ResolvedMapType { key_type: Int, value_type: String }, index_expression: Literal(IntLiteral(3, <44:1>)), map_expression: VariableAccess(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }) }) })
IfAssignExpression { variable: ResolvedVariable { name: <58:1>, resolved_type: String, mutable_node: None, scope_index: 1, variable_index: 0 }, optional_expr: VariableAccess(ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }), true_block: Block([]), false_block: None }

"#,
    );
}

#[test_log::test]
fn option_var() {
    check(
        r"
        map = [2: 'hello']
        a = map[3]
        if a? {
            2
        }
         ",
        r#"

..(),CreateVariable(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: [Int:String], node: <15:12>, kind: Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(ResolvedExpression { ty: Int, node: <16:1>, kind: Literal(IntLiteral(2)) }, ResolvedExpression { ty: String, node: <19:7>, kind: InterpolatedString([Literal(<20:5>, "hello")]) })])) }))
..(),CreateVariable(ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }, Immutable(ResolvedExpression { ty: String?, node: <40:3>, kind: MapIndexAccess(ResolvedExpression { ty: [Int:String], node: <40:3>, kind: VariableAccess(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }) }, ResolvedMapType { key_type: Int, value_type: String }, ResolvedExpression { ty: Int, node: <44:1>, kind: Literal(IntLiteral(3)) }) }))
..String,IfOnlyVariable { variable: ResolvedVariable { name: <58:1>, resolved_type: String, mutable_node: None, scope_index: 1, variable_index: 0 }, optional_expr: ResolvedExpression { ty: String?, node: <58:1>, kind: VariableAccess(ResolvedVariable { name: <36:1>, resolved_type: String?, mutable_node: None, scope_index: 0, variable_index: 1 }) }, true_block: ResolvedExpression { ty: Int, node: <61:25>, kind: Block([ResolvedExpression { ty: Int, node: <75:1>, kind: Literal(IntLiteral(2)) }]) }, false_block: None }

"#,
    );
}

#[test_log::test]
fn option_var_mut() {
    check(
        r"
        map = [2: 'hello']
        mut a = map[3]
        if a? {
            a
        }
         ",
        r#"


..(),CreateVariable(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: [Int:String], node: <15:12>, kind: Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(ResolvedExpression { ty: Int, node: <16:1>, kind: Literal(IntLiteral(2)) }, ResolvedExpression { ty: String, node: <19:7>, kind: InterpolatedString([Literal(<20:5>, "hello")]) })])) }))
..(),CreateVariable(ResolvedVariable { name: <40:1>, resolved_type: String?, mutable_node: Some(<36:3>), scope_index: 0, variable_index: 1 }, Immutable(ResolvedExpression { ty: String?, node: <44:3>, kind: MapIndexAccess(ResolvedExpression { ty: [Int:String], node: <44:3>, kind: VariableAccess(ResolvedVariable { name: <9:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }) }, ResolvedMapType { key_type: Int, value_type: String }, ResolvedExpression { ty: Int, node: <48:1>, kind: Literal(IntLiteral(3)) }) }))
..String,IfOnlyVariable { variable: ResolvedVariable { name: <62:1>, resolved_type: String, mutable_node: Some(<0:0>), scope_index: 1, variable_index: 0 }, optional_expr: ResolvedExpression { ty: String?, node: <62:1>, kind: VariableAccess(ResolvedVariable { name: <40:1>, resolved_type: String?, mutable_node: Some(<36:3>), scope_index: 0, variable_index: 1 }) }, true_block: ResolvedExpression { ty: String, node: <65:25>, kind: Block([ResolvedExpression { ty: String, node: <79:1>, kind: VariableAccess(ResolvedVariable { name: <62:1>, resolved_type: String, mutable_node: Some(<0:0>), scope_index: 1, variable_index: 0 }) }]) }, false_block: None }

"#,
    );
}

#[test_log::test]
fn option_var_mut_simple() {
    check(
        r"
map = [2: 'hello']
mut a = map[3]

         ",
        r#"

..(),CreateVariable(ResolvedVariable { name: <1:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: [Int:String], node: <7:12>, kind: Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(ResolvedExpression { ty: Int, node: <8:1>, kind: Literal(IntLiteral(2)) }, ResolvedExpression { ty: String, node: <11:7>, kind: InterpolatedString([Literal(<12:5>, "hello")]) })])) }))
..(),CreateVariable(ResolvedVariable { name: <24:1>, resolved_type: String?, mutable_node: Some(<20:3>), scope_index: 0, variable_index: 1 }, Immutable(ResolvedExpression { ty: String?, node: <28:3>, kind: MapIndexAccess(ResolvedExpression { ty: [Int:String], node: <28:3>, kind: VariableAccess(ResolvedVariable { name: <1:3>, resolved_type: [Int:String], mutable_node: None, scope_index: 0, variable_index: 0 }) }, ResolvedMapType { key_type: Int, value_type: String }, ResolvedExpression { ty: Int, node: <32:1>, kind: Literal(IntLiteral(3)) }) }))

"#,
    );
}

#[test_log::test]
fn var_assignment_coerce() {
    check(
        r"
booster: Int? = none

         ",
        r"
Int?,CreateVariable(ResolvedVariable { name: <1:7>, resolved_type: Int?, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Int?, node: <17:4>, kind: Literal(NoneLiteral) }))



",
    );
}

#[test_log::test]
fn var_assignment_coerce_if() {
    check(
        r"
booster: Int? = none
if booster? {
 true
}
         ",
        r"
InitializeVariable(ResolvedVariableAssignment { variable_refs: ResolvedVariable { name: <1:7>, resolved_type: Int?, mutable_node: None, scope_index: 0, variable_index: 0 }, expression: Literal(NoneLiteral(<17:4>)) })
IfOnlyVariable { variable: ResolvedVariable { name: <25:7>, resolved_type: Int, mutable_node: None, scope_index: 1, variable_index: 0 }, optional_expr: VariableAccess(ResolvedVariable { name: <1:7>, resolved_type: Int?, mutable_node: None, scope_index: 0, variable_index: 0 }), true_block: Block([Literal(BoolLiteral(true, <37:4>))]), false_block: None }

",
    );
}

#[test_log::test]
fn fail_var_assignment_coerce_if() {
    check_fail(
        r"
booster: Int? = none
if booster? {
 true
} else {
        2
}

         ",
        r"
Error { node: <59:1>, kind: IncompatibleTypes(Bool, Int) }
",
    );
}

#[test_log::test]
fn val_assign_coerce() {
    check(
        r"
booster_value: Int? = if false 0 else none
         ",
        r"
Int?,CreateVariable(ResolvedVariable { name: <1:13>, resolved_type: Int?, mutable_node: None, scope_index: 0, variable_index: 0 }, Immutable(ResolvedExpression { ty: Int?, node: <26:5>, kind: If(ResolvedBooleanExpression { expression: ResolvedExpression { ty: Bool, node: <26:5>, kind: Literal(BoolLiteral(false)) } }, ResolvedExpression { ty: Int?, node: <32:1>, kind: Option(Some(ResolvedExpression { ty: Int, node: <32:1>, kind: Literal(IntLiteral(0)) })) }, Some(ResolvedExpression { ty: Int?, node: <39:4>, kind: Literal(NoneLiteral) })) }))

         ",
    );
}

#[test_log::test]
fn mutable_location_field_access() {
    check(
        r"
struct Brain {
    i: Int,
}

struct Enemy {
    brain: Brain,
}

impl Brain {
    fn think(mut self) -> Bool {
        self.i += 1
        self.i > 10
    }
}

mut enemy = Enemy { brain: Brain { i: 0 } }

want_to_attack = enemy.brain.think()

         ",
        r#"
StructType(RefCell { value: struct "Brain" })
StructType(RefCell { value: struct "Enemy" })
ImplType(Brain)
---
InitializeVariable(ResolvedVariableAssignment { variable_refs: ResolvedVariable { name: <138:5>, resolved_type: Enemy, mutable_node: Some(<134:3>), scope_index: 0, variable_index: 0 }, expression: StructInstantiation(ResolvedStructInstantiation { source_order_expressions: [(0, StructInstantiation(ResolvedStructInstantiation { source_order_expressions: [(0, Literal(IntLiteral(0, <172:1>)))], struct_type_ref: RefCell { value: struct "Brain" } }))], struct_type_ref: RefCell { value: struct "Enemy" } }) })
InitializeVariable(ResolvedVariableAssignment { variable_refs: ResolvedVariable { name: <179:14>, resolved_type: (), mutable_node: None, scope_index: 0, variable_index: 1 }, expression: MemberCall(ResolvedMemberCall { function: Internal(FunctionTypeSignature { first_parameter_is_self: true, parameters: [ResolvedTypeForParameter { name: "self", resolved_type: Brain, is_mutable: true, node: Some(ResolvedParameter) }], return_type: () }
Block([FieldCompoundAssignment(VariableAccess(ResolvedVariable { name: <97:4>, resolved_type: Brain, mutable_node: Some(<93:3>), scope_index: 0, variable_index: 0 }), [FieldIndex(<118:1>, 0)], ResolvedCompoundOperator { node: <120:2>, kind: Add }, Literal(IntLiteral(1, <123:1>)))])), arguments: [], self_expression: FieldAccess(VariableAccess(ResolvedVariable { name: <138:5>, resolved_type: Enemy, mutable_node: Some(<134:3>), scope_index: 0, variable_index: 0 }), ResolvedStructTypeField { struct_type_ref: RefCell { value: struct "Enemy" }, field_name: ResolvedLocalIdentifier(<202:5>), resolved_type: Brain, index: 0 }, [FieldIndex(<202:5>, 0)]), self_is_mutable: true }) })

         "#,
    );
}

#[test_log::test]
fn compound_op_in_member_mutable() {
    check_fail(
        r"

struct Something

struct Position {
    x: Int,
    y: Int,
}

impl Something {
    fn not_allowed( pos : Position) {
        pos.y += 1
    }
}

mut pos = Position { x: 10, y : 20 }
Something::not_allowed(pos)
         ",
        r"
        Error { node: <128:3>, kind: VariableIsNotMutable }
        ",
    );
}

#[test_log::test]
fn while_simple() {
    check(
        r"

mut a = 3

while a < 3 {
    a += 1
}
         ",
        r"
        Error { node: <128:3>, kind: VariableIsNotMutable }
        ",
    );
}

#[test_log::test]
fn map_assign() {
    check(
        "
    mut a = [2:'hello', -1:    'world']
    a[3] = 'ossian'

    ",
        r#"
..[Int:String],VariableDefinition(ResolvedVariable { name: <9:1>, resolved_type: [Int:String], mutable_node: Some(<5:3>), scope_index: 0, variable_index: 0 }, ResolvedMutOrImmutableExpression { expression_or_location: Expression(<13:27>[Int:String],Literal(Map(ResolvedMapType { key_type: Int, value_type: String }, [(<14:1>Int,Literal(IntLiteral(2)), <16:7>String,InterpolatedString([Literal(<17:5>, "hello")])), (<25:2>Int,UnaryOp(ResolvedUnaryOperator { left: <26:1>Int,Literal(IntLiteral(1)), kind: Negate, node: <25:1> }), <32:7>String,InterpolatedString([Literal(<33:5>, "world")]))]))), is_mutable: None })
..(),MapAssignment(ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression { kind: MutVariableRef, node: <45:1>, ty: [Int:String], starting_variable: ResolvedVariable { name: <9:1>, resolved_type: [Int:String], mutable_node: Some(<5:3>), scope_index: 0, variable_index: 0 }, access_chain: [] }), <47:1>Int,Literal(IntLiteral(3)), <52:8>String,InterpolatedString([Literal(<53:6>, "ossian")]))
"#,
    );
}


#[test_log::test]
fn infer_anonymous_struct() {
    check(
        "
    struct Something {
       a: Int,
    }

    result: Something = { a: 3 }
    ",
        r#"
Unit,VariableDefinition(Variable { name: <52:6>, resolved_type: Something, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<72:8>Something,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <77:1>Int,Literal(IntLiteral(3)))], struct_type_ref: RefCell { value: struct "Something" } })), is_mutable: None })

"#,
    );
}

#[test_log::test]
fn impl_on_enum() {
    check(
        "
    enum State {
       StartingUp,
       NotWorking(Int),
       Operational { duration: Int },
    }

    impl State {
        fn is_probably_working(self) -> Bool {
            self == State::NotWorking
        }
    }

    state = State::StartingUp

    state.is_probably_working()
    ",
        r#"
Unit,VariableDefinition(Variable { name: <52:6>, resolved_type: Something, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<72:8>Something,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <77:1>Int,Literal(IntLiteral(3)))], struct_type_ref: RefCell { value: struct "Something" } })), is_mutable: None })

"#,
    );
}

#[test_log::test]
fn annotation() {
    check(
        "
    a  = [0:2.4, 1:4.8]
    ",
        r#"
Unit,VariableDefinition(Variable { name: <52:6>, resolved_type: Something, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<72:8>Something,StructInstantiation(StructInstantiation { source_order_expressions: [(0, <77:1>Int,Literal(IntLiteral(3)))], struct_type_ref: RefCell { value: struct "Something" } })), is_mutable: None })

"#,
    );
}



#[test_log::test]
fn type_generator() {
    check(
        "
    a = Sparse<Int>::new()
    ",
        r#"
Namespace { path: ["test"], symbol_table: SymbolTable { symbols: SeqMap() } }
Unit,VariableDefinition(Variable { name: <5:1>, resolved_type: "Sparse"?<[Int]>, mutable_node: None, scope_index: 0, variable_index: 0 }, MutOrImmutableExpression { expression_or_location: Expression(<9:6>RustType Sparse<Int>,IntrinsicCallGeneric(SparseNew, [Int], [])), is_mutable: None })

"#,
    );
}


#[test_log::test]
fn blueprint_add() {
    check(
        "
    struct JustTest<T> {
    }
    ",
        r#"
Namespace { path: ["test"], symbol_table: SymbolTable { symbols: SeqMap("JustTest": Blueprint(ParameterizedTypeBlueprint { kind: Struct(struct "JustTest"), type_variables: ["T"] })) } }
"#,
    );
}

*/
