/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::util::check;

mod util;

/*

#[test_log::test]
fn match_value_expressions() {
    let script = r#"
    match temperature {
        x > 15 => "Warm"
        x < -10 => "Cold"
        _ -> "Neither warm nor cold"
    }
        "#;

    check(script, r#""#);
}
*/

#[test_log::test]
fn assignment() {
    check(
        "a = 3",
        r"
<0:5>VariableAssignment(<0:1>, MutableOrImmutableExpression { is_mutable: None, expression: <4:1>Literal(Int) })
    ",
    );
}

#[test_log::test]
fn assignment_add() {
    check(
        "a = n + 6",
        "<0:9>VariableAssignment(<0:1>, MutableOrImmutableExpression { is_mutable: None, expression: <4:5>BinaryOp(<4:1>IdentifierReference(<4:1>), BinaryOperator { kind: Add, node: <6:1> }, <8:1>Literal(Int)) })",
    );
}

#[test_log::test]
fn function_call() {
    let script = "
            fn add(x: Int, y: Int) -> Int {
                x + y
            }
            result = add(10, 20)
        ";
    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <16:3>, params: [Parameter { variable: <20:1>, param_type: Int(<23:3>) }, Parameter { variable: <28:1>, param_type: Int(<31:3>) }], self_parameter: None, return_type: Some(Int(<39:3>)) }, body: Block([BinaryOp(VariableAccess(<61:1>), Add(<63:1>), VariableAccess(<65:1>))]) }))
---
VariableAssignment(<93:6>, FunctionCall(VariableAccess(<102:3>), [Literal(Int(<106:2>)), Literal(Int(<110:2>))]))



    ",
    );
}

#[test_log::test]
fn struct_def() {
    let script = "
            struct Person { first_field: Int, second_field: String }
        ";
    check(
        script,
        "StructDef(StructType { identifier: LocalTypeIdentifier(<20:6>), fields: [FieldType { field_name: FieldName(<29:11>), field_type: Int(<42:3>) }, FieldType { field_name: FieldName(<47:12>), field_type: String(<61:6>) }] })",
    );
}

#[test_log::test]
fn struct_init() {
    let script = r#"
            person = Person { first_field: 1, second_field: "Bob" }
        "#;
    check(
        script,
        r"
 VariableAssignment(<13:6>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<22:6>), module_path: None }, [FieldExpression { field_name: FieldName(<31:11>), expression: Literal(Int(<44:1>)) }, FieldExpression { field_name: FieldName(<47:12>), expression: Literal(String(<61:5>)) }]))
",
    );
}

#[test_log::test]
fn while_loop() {
    let script = "
            a = 0
            while a < 3 {
                print(a)
                a = a + 1
            }
        ";
    check(
        script,
        r"
VariableAssignment(<13:1>, Literal(Int(<17:1>)))
WhileLoop(BinaryOp(VariableAccess(<37:1>), LessThan(<39:1>), Literal(Int(<41:1>))), Block([FunctionCall(VariableAccess(<61:5>), [VariableAccess(<67:1>)]), VariableAssignment(<86:1>, BinaryOp(VariableAccess(<90:1>), Add(<92:1>), Literal(Int(<94:1>))))]))

        ",
    );
}

#[test_log::test]
fn if_expression() {
    let script = "
            c = if true {
                a
            } else {
                b
            }
        ";

    check(
        script,
        "VariableAssignment(<13:1>, If(Literal(Bool(<20:4>)), Block([VariableAccess(<43:1>)]), Some(Block([VariableAccess(<82:1>)]))))",
    );
}

#[test_log::test]
fn struct_def_and_instantiation() {
    check(
        r#"
            struct Person { first_field: Int, second_field: String }
            person = Person { first_field: 1, second_field: "Bob" }
        "#,
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<20:6>), fields: [FieldType { field_name: FieldName(<29:11>), field_type: Int(<42:3>) }, FieldType { field_name: FieldName(<47:12>), field_type: String(<61:6>) }] })
---
VariableAssignment(<82:6>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:6>), module_path: None }, [FieldExpression { field_name: FieldName(<100:11>), expression: Literal(Int(<113:1>)) }, FieldExpression { field_name: FieldName(<116:12>), expression: Literal(String(<130:5>)) }]))

    ",
    );
}

#[test_log::test]
fn nested_function_calls() {
    check(
        "result = add(mul(2, 3), div(10, 2))",
        "VariableAssignment(<0:6>, FunctionCall(VariableAccess(<9:3>), [FunctionCall(VariableAccess(<13:3>), [Literal(Int(<17:1>)), Literal(Int(<20:1>))]), FunctionCall(VariableAccess(<24:3>), [Literal(Int(<28:2>)), Literal(Int(<32:1>))])]))
",
    );
}

#[test_log::test]
fn complex_expressions() {
    let script = "
        result = (2 + 3) * 4 - (10 / 2)
        flag = true && !false || 5 > 3
    ";
    check(
        script,
        r"

VariableAssignment(<9:6>, BinaryOp(BinaryOp(BinaryOp(Literal(Int(<19:1>)), Add(<21:1>), Literal(Int(<23:1>))), Multiply(<26:1>), Literal(Int(<28:1>))), Subtract(<30:1>), BinaryOp(Literal(Int(<33:2>)), Divide(<36:1>), Literal(Int(<38:1>)))))
VariableAssignment(<49:4>, BinaryOp(BinaryOp(Literal(Bool(<56:4>)), LogicalAnd(<61:2>), UnaryOp(Not(<64:1>), Literal(Bool(<65:5>)))), LogicalOr(<71:2>), BinaryOp(Literal(Int(<74:1>)), GreaterThan(<76:1>), Literal(Int(<78:1>)))))

        ",
    );
}

#[test_log::test]
fn not_operator() {
    let script = "
        !x
    ";
    check(
        script,
        r"
<9:7>UnaryOp(Not(<9:1>), <10:1>VariableAccess(<10:1>))
        ",
    );
}

#[test_log::test]
fn struct_field_access_with_struct_init() {
    let script = r#"
        person = Person { first_field: 1, second_field: "Bob" }
        name = person.second_field
    "#;
    check(
        script,
        r#"

<9:64>VariableAssignment(<9:6>, MutableOrImmutableExpression { is_mutable: None, expression: <18:46>StructLiteral(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:6>), module_path: None, generic_params: [] }, [FieldExpression { field_name: FieldName(<27:11>), expression: <40:1>Literal(Int) }, FieldExpression { field_name: FieldName(<43:12>), expression: <57:5>Literal(String("Bob")) }], false) })
<73:31>VariableAssignment(<73:4>, MutableOrImmutableExpression { is_mutable: None, expression: <80:19>PostfixChain(PostfixChain { base: <80:6>IdentifierReference(<80:6>), suffixes: [FieldAccess(<87:12>)] }) })


    "#,
    );
}

#[test_log::test]
fn struct_field_access() {
    let script = "
        name = person.second_field
    ";
    check(
        script,
        "<9:31>VariableAssignment(<9:4>, MutableOrImmutableExpression { is_mutable: None, expression: <16:19>PostfixChain(PostfixChain { base: <16:6>IdentifierReference(<16:6>), suffixes: [FieldAccess(<23:12>)] }) })
",
    );
}

#[test_log::test]
fn struct_field_access_assign() {
    let script = "
        person.second_field.another = name
    ";
    check(
        script,
        r"
<9:39>Assignment(<9:27>PostfixChain(PostfixChain { base: <9:6>IdentifierReference(<9:6>), suffixes: [FieldAccess(<16:12>), FieldAccess(<29:7>)] }), <39:4>IdentifierReference(<39:4>))

        ",
    );
}

#[test_log::test]
fn struct_field_assignment_chain() {
    let script = "
        a.b.c.d = 3
    ";
    check(
        script,
        "FieldAssignment(FieldAccess(FieldAccess(VariableAccess(<9:1>), <11:1>), <13:1>), <15:1>, Literal(Int(<19:1>)))",
    );
}

#[test_log::test]
fn struct_field_assignment_chain2() {
    let script = "
        a = [10, 20]
        a[1]
    ";
    check(
        script,
        r"
VariableAssignment(<9:1>, Literal(Array([Literal(Int(<14:2>)), Literal(Int(<18:2>))])))
IndexAccess(VariableAccess(<30:1>), Literal(Int(<32:1>)))
",
    );
}

#[test_log::test]
fn struct_field_assignment_chain7() {
    let script = "
        a = [10, 20]
        a.remove(99)
    ";
    check(
        script,
        r"

VariableAssignment(<9:1>, Literal(Array([Literal(Int(<14:2>)), Literal(Int(<18:2>))])))
MemberCall(VariableAccess(<30:1>), <32:6>, [Literal(Int(<39:2>))])

",
    );
}

#[test_log::test]
fn compound_conditions() {
    let script = "
        while x > 0 && y < 10 {
            x = x - 1
            y = y + 1
        }
    ";
    check(
        script,
        "

<9:77>WhileLoop(<15:16>BinaryOp(<15:6>BinaryOp(<15:1>IdentifierReference(<15:1>), BinaryOperator { kind: GreaterThan, node: <17:1> }, <19:1>Literal(Int)), BinaryOperator { kind: LogicalAnd, node: <21:2> }, <24:7>BinaryOp(<24:1>IdentifierReference(<24:1>), BinaryOperator { kind: LessThan, node: <26:1> }, <28:2>Literal(Int))), <31:55>Block([<45:22>VariableAssignment(<45:1>, MutableOrImmutableExpression { is_mutable: None, expression: <49:18>BinaryOp(<49:1>IdentifierReference(<49:1>), BinaryOperator { kind: Subtract, node: <51:1> }, <53:1>Literal(Int)) }), <67:18>VariableAssignment(<67:1>, MutableOrImmutableExpression { is_mutable: None, expression: <71:14>BinaryOp(<71:1>IdentifierReference(<71:1>), BinaryOperator { kind: Add, node: <73:1> }, <75:1>Literal(Int)) })]))

",
    );
}

#[test_log::test]
fn nested_loops() {
    let script = "
        x = 0
        while x < 3 {
            y = 0
            while y < 2 {
                print(x + y)
                y = y + 1
            }
            x = x + 1
        }
    ";
    check(
        script,
        r"
VariableAssignment(<9:1>, Literal(Int(<13:1>)))
WhileLoop(BinaryOp(VariableAccess(<29:1>), LessThan(<31:1>), Literal(Int(<33:1>))), Block([VariableAssignment(<49:1>, Literal(Int(<53:1>))), WhileLoop(BinaryOp(VariableAccess(<73:1>), LessThan(<75:1>), Literal(Int(<77:1>))), Block([FunctionCall(VariableAccess(<97:5>), [BinaryOp(VariableAccess(<103:1>), Add(<105:1>), VariableAccess(<107:1>))]), VariableAssignment(<126:1>, BinaryOp(VariableAccess(<130:1>), Add(<132:1>), Literal(Int(<134:1>))))])), VariableAssignment(<162:1>, BinaryOp(VariableAccess(<166:1>), Add(<168:1>), Literal(Int(<170:1>))))]))
 ",
    );
}

#[test_log::test]
fn mixed_expressions_with_chain() {
    let script = "
        struct Point { x: Int, y: Int }
        p1 = Point { x: 5, y: 10 }
        dist = add(mul(p1.x, p1.x), mul(p1.y, p1.y))
    ";
    check(
        script,
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<24:1>), field_type: Int(<27:3>) }, FieldType { field_name: FieldName(<32:1>), field_type: Int(<35:3>) }] })
---
VariableAssignment(<49:2>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<54:5>), module_path: None }, [FieldExpression { field_name: FieldName(<62:1>), expression: Literal(Int(<65:1>)) }, FieldExpression { field_name: FieldName(<68:1>), expression: Literal(Int(<71:2>)) }]))
VariableAssignment(<84:4>, FunctionCall(VariableAccess(<91:3>), [FunctionCall(VariableAccess(<95:3>), [FieldAccess(VariableAccess(<99:2>), <102:1>), FieldAccess(VariableAccess(<105:2>), <108:1>)]), FunctionCall(VariableAccess(<112:3>), [FieldAccess(VariableAccess(<116:2>), <119:1>), FieldAccess(VariableAccess(<122:2>), <125:1>)])]))

    ",
    );
}

#[test_log::test]
fn small_call() {
    let script = "
        mul(2)
    ";
    check(
        script,
        "FunctionCall(VariableAccess(<9:3>), [Literal(Int(<13:1>))])",
    );
}

#[test_log::test]
fn small_chain() {
    let script = "
        p1.x.mul(2)
    ";
    check(
        script,
        "<9:11>PostfixChain(PostfixChain { base: <9:2>IdentifierReference(<9:2>), suffixes: [FieldAccess(<12:1>), MemberCall(<14:3>, [MutableOrImmutableExpression { is_mutable: None, expression: <18:1>Literal(Int) }])] })",
    );
}

#[test_log::test]
fn only_chain() {
    let script = "
        p1.x.mul(p1.x).add(p1.y.mul(p1.y))
    ";
    check(
        script,
        r"
MemberCall(MemberCall(FieldAccess(VariableAccess(<9:2>), <12:1>), <14:3>, [FieldAccess(VariableAccess(<18:2>), <21:1>)]), <24:3>, [MemberCall(FieldAccess(VariableAccess(<28:2>), <31:1>), <33:3>, [FieldAccess(VariableAccess(<37:2>), <40:1>)])])
    ",
    );
}

#[test_log::test]
fn method_chaining() {
    let script = r"
        struct Point { x: Int, y: Int }
        p1 = Point { x: 5, y: 10 }
        dist = p1.x.mul(p1.x).add(p1.y.mul(p1.y))
    ";

    check(
        script,
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<24:1>), field_type: Int(<27:3>) }, FieldType { field_name: FieldName(<32:1>), field_type: Int(<35:3>) }] })
---
VariableAssignment(<49:2>, StructInstantiation(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<54:5>), module_path: None }, [FieldExpression { field_name: FieldName(<62:1>), expression: Literal(Int(<65:1>)) }, FieldExpression { field_name: FieldName(<68:1>), expression: Literal(Int(<71:2>)) }]))
VariableAssignment(<84:4>, MemberCall(MemberCall(FieldAccess(VariableAccess(<91:2>), <94:1>), <96:3>, [FieldAccess(VariableAccess(<100:2>), <103:1>)]), <106:3>, [MemberCall(FieldAccess(VariableAccess(<110:2>), <113:1>), <115:3>, [FieldAccess(VariableAccess(<119:2>), <122:1>)])]))

    ",
    );
}

#[test_log::test]
fn function_definition() {
    let script = r"
        fn add(x: Int, y: Int) -> Int {
            x + y
        }
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [Parameter { variable: <16:1>, param_type: Int(<19:3>) }, Parameter { variable: <24:1>, param_type: Int(<27:3>) }], self_parameter: None, return_type: Some(Int(<35:3>)) }, body: <39:29>Block([<53:14>BinaryOp(<53:1>IdentifierReference(<53:1>), BinaryOperator { kind: Add, node: <55:1> }, <57:1>IdentifierReference(<57:1>))]) }))
    ",
    );
}

#[test_log::test]
fn function_with_no_parameters() {
    let script = "
        fn add() -> Int {
            42
        }
    ";

    check(
        script,
        "

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [], self_parameter: None, return_type: Some(Int(<21:3>)) }, body: <25:26>Block([<39:2>Literal(Int)]) }))

    ",
    );
}

#[test_log::test]
fn function_with_no_parameters_return() {
    let script = "
        fn add() -> Int {
            return 42
        }
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:3>, params: [], self_parameter: None, return_type: Some(Int(<21:3>)) }, body: <25:33>Block([<39:18>Return(Some(<46:2>Literal(Int)))]) }))


    ",
    );
}

#[test_log::test]
fn function_call_with_no_parameters() {
    let script = "
        fn single() -> Int {
            42
        }
        result = single()
    ";

    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:6>, params: [], self_parameter: None, return_type: Some(Int(<24:3>)) }, body: <28:26>Block([<42:2>Literal(Int)]) }))
---
<63:22>VariableAssignment(<63:6>, MutableOrImmutableExpression { is_mutable: None, expression: <72:8>PostfixChain(PostfixChain { base: <72:6>IdentifierReference(<72:6>), suffixes: [FunctionCall(<78:2>, [])] }) })


    ",
    );
}

#[test_log::test]
fn array() {
    let script = r"
        x = [1, 2, 3]
    ";

    check(
        script,
        r"
<9:13>VariableAssignment(<9:1>, MutableOrImmutableExpression { is_mutable: None, expression: <13:9>Literal(Slice([<14:1>Literal(Int), <17:1>Literal(Int), <20:1>Literal(Int)])) })

    ",
    );
}

#[test_log::test]
fn empty_array() {
    let script = r"
        x = []
    ";

    check(
        script,
        r"
VariableAssignment(<9:1>, Literal(Array([])))
    ",
    );
}

#[test_log::test]
fn empty_array_expression() {
    let script = r"
        []
    ";

    check(
        script,
        r"
Literal(Array([]))
    ",
    );
}

#[test_log::test]
fn empty_array_expression_call() {
    let script = r"
        [].len()
    ";

    check(
        script,
        r"
MemberCall(Literal(Array([])), <12:3>, [])
        ",
    );
}

#[test_log::test]
fn string_expression_call() {
    let script = r#"
        "hello, world".len()
    "#;

    check(
        script,
        r"
MemberCall(Literal(String(<9:14>)), <24:3>, [])
        ",
    );
}

#[test_log::test]
fn real_round() {
    let script = r"
        2.2.round()
    ";

    check(
        script,
        r"
MemberCall(Literal(Float(<9:3>)), <13:5>, [])
        ",
    );
}

#[test_log::test]
fn real_negative() {
    let script = r"
        -2.2
    ";

    check(
        script,
        r"
<9:9>UnaryOp(Negate(<9:1>), <10:3>Literal(Float))
        ",
    );
}

#[test_log::test]
fn real_negative_round() {
    let script = r"
        -2.2.round()
    ";

    check(
        script,
        r"
UnaryOp(Negate(<9:1>), MemberCall(Literal(Float(<10:3>)), <14:5>, []))
        ",
    );
}

#[test_log::test]
fn real_literal() {
    let script = r"
        2.2
    ";

    check(
        script,
        r"
<9:3>Literal(Float)
        ",
    );
}

#[test_log::test]
fn string_literal() {
    let script = r#"
    "hello"
     "#;
    check(
        script,
        r"
Literal(String(<5:7>))
        ",
    );
}

#[test_log::test]
fn tuple_literal() {
    let script = r#"
    if true {
    }
    ( "hello", 1, 2.2 )
     "#;
    check(
        script,
        r#"
<5:20>If(<8:4>Literal(Bool), <13:7>Block([]), None)
<25:19>Literal(Tuple([<27:7>Literal(String("hello")), <36:1>Literal(Int), <39:3>Literal(Float)]))
        "#,
    );
}

#[test_log::test]
fn tuple_type() {
    let script = r#"
    fn some_tuple() -> ( String, Int, Float ) {
        ( "hello", 1, 2.2 )
         }
     "#;
    check(
        script,
        r"

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <8:10>, params: [], self_parameter: None, return_type: Some(Tuple([String(<26:6>), Int(<34:3>), Float(<39:5>)])) }, body: Block([Literal(Tuple([Literal(String(<59:7>)), Literal(Int(<68:1>)), Literal(Float(<71:3>))]))]) }))

        ",
    );
}

#[test_log::test]
fn enum_type() {
    let script = "
    enum Custom {
        Idle,
        Running(Int, Float),
        Sleeping { hours: Int },
    }
    ";
    check(
        script,
        "
EnumDef(LocalTypeIdentifierWithOptionalTypeVariables { name: <10:6>, type_variables: [] }, [Simple(<27:4>), Tuple(<41:7>, [Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<49:3>), module_path: None, generic_params: [] }), Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<54:5>), module_path: None, generic_params: [] })]), Struct(<70:8>, AnonymousStructType { fields: [StructTypeField { field_name: FieldName(<81:5>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<88:3>), module_path: None, generic_params: [] }) }] })])


        ",
    );
}

#[test_log::test]
fn return_bool() {
    check(
        "  fn default() -> Bool {
    false
  }",
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <5:7>, params: [], self_parameter: None, return_type: Some(Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:4>), module_path: None, generic_params: [] })), generic_variables: [] }, body: <23:15>Block([<29:5>Literal(Bool)]) }))
        ",
    );
}

#[test_log::test]
fn enum_type2() {
    let script = "
    enum Custom {
        Idle { one: Int, two: (Int, Float) },
        Running(Int, Float, String, Int),
    }
    ";
    check(
        script,
        "

EnumDef(<10:6>, [Struct(<27:4>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<34:3>), field_type: Int(<39:3>) }, FieldType { field_name: FieldName(<44:3>), field_type: Tuple([Int(<50:3>), Float(<55:5>)]) }] }), Tuple(<73:7>, [Int(<81:3>), Float(<86:5>), String(<93:6>), Int(<101:3>)])])

        ",
    );
}

#[test_log::test]
fn match_expression() {
    let script = r"
        enum Custom {
            Idle,
            Running(Int, Float),
            Sleeping { hours: Int },
        }


        v = match state {
            Running speed, _ => speed,
            Sleeping  hours   => hours + 10,
            _ => 0,
        }

";

    check(
        script,
        r"

EnumDef(<14:6>, [Simple(<35:4>), Tuple(<53:7>, [Int(<61:3>), Float(<66:5>)]), Struct(<86:8>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<97:5>), field_type: Int(<104:3>) }] })])
---
VariableAssignment(<131:1>, Match(VariableAccess(<141:5>), [MatchArm { pattern: NormalPattern(EnumPattern(<161:7>, Some([Variable(<169:5>), Wildcard(<176:1>)])), None), expression: VariableAccess(<181:5>) }, MatchArm { pattern: NormalPattern(EnumPattern(<200:8>, Some([Variable(<210:5>)])), None), expression: BinaryOp(VariableAccess(<220:5>), Add(<226:1>), Literal(Int(<228:2>))) }, MatchArm { pattern: Wildcard(<244:1>), expression: Literal(Int(<249:1>)) }]))

",
    );
}

#[test_log::test]
fn match_expression_minimal() {
    let script = r"
       match state {
            _ => 0,
        }
";

    check(
        script,
        r"
Match(VariableAccess(<14:5>), [MatchArm { pattern: Wildcard(<34:1>), expression: Literal(Int(<39:1>)) }])
",
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms() {
    let script = r"
       match state {
            EnumType ident, _ => ident,
            _ => 0,
        }
";

    check(
        script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: NormalPattern(EnumPattern(<34:8>, Some([Variable(<43:5>), Wildcard(<50:1>)])), None), expression: VariableAccess(<55:5>) }, MatchArm { pattern: Wildcard(<74:1>), expression: Literal(Int(<79:1>)) }])

",
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms_guard() {
    let script = r"
       match state {
            EnumType | false && true  => ident,
            _ => 0,
        }
";

    check(
        script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: NormalPattern(EnumPattern(<34:8>, Some([Variable(<43:5>), Wildcard(<50:1>)])), None), expression: VariableAccess(<55:5>) }, MatchArm { pattern: Wildcard(<74:1>), expression: Literal(Int(<79:1>)) }])

",
    );
}

#[test_log::test]
fn match_expression_minimal_two_arms_enum_struct() {
    let script = r"
       match state {
            EnumType something, another => another,
            _ => 0,
        }
";

    check(
        script,
        r"

Match(VariableAccess(<14:5>), [MatchArm { pattern: NormalPattern(EnumPattern(<34:8>, Some([Variable(<43:9>), Variable(<54:7>)])), None), expression: VariableAccess(<65:7>) }, MatchArm { pattern: Wildcard(<86:1>), expression: Literal(Int(<91:1>)) }])

",
    );
}

#[test_log::test]
fn deconstructing_struct() {
    let script = "x, y = pos"; // No raw string literal
    check(
        script,
        "MultiVariableAssignment([<0:1>, <3:1>], VariableAccess(<7:3>))",
    );
}

#[test_log::test]
fn operator_precedence() {
    let script = "z = y * 2 - x";
    check(
        script,
        "VariableAssignment(<0:1>, BinaryOp(BinaryOp(VariableAccess(<4:1>), Multiply(<6:1>), Literal(Int(<8:1>))), Subtract(<10:1>), VariableAccess(<12:1>)))",
    );
}

#[test_log::test]
fn operator_precedence_expression() {
    let script = "y * 2 - x";
    check(
        script,
        "
BinaryOp(BinaryOp(VariableAccess(<0:1>), Multiply(<2:1>), Literal(Int(<4:1>))), Subtract(<6:1>), VariableAccess(<8:1>))",
    );
}

#[test_log::test]
fn for_range() {
    let script = r"
for x in 1..10 {
}

    ";
    check(
        script,
        "
ForLoop(Single(ForVar { identifier: <5:1>, is_mut: None }), IteratableExpression { is_mut: None, expression: ExclusiveRange(Literal(Int(<10:1>)), Literal(Int(<13:2>))) }, Block([]))

",
    );
}

#[test_log::test]
fn range_literal() {
    let script = r"

    0..39

    ";
    check(
        script,
        "
ExclusiveRange(Literal(Int(<6:1>)), Literal(Int(<9:2>)))",
    );
}

#[test_log::test]
fn inclusive_range_literal() {
    let script = r"

    0..=39

    ";
    check(
        script,
        "
InclusiveRange(Literal(Int(<6:1>)), Literal(Int(<10:2>)))",
    );
}

#[test_log::test]
fn for_array() {
    let script = r"

for x in [1, 2, 3] {
}

    ";
    check(
        script,
        "
<2:22>ForLoop(Single(ForVar { identifier: <6:1>, is_mut: None }), IterableExpression { expression: MutableOrImmutableExpression { is_mutable: None, expression: <11:9>Literal(Array([<12:1>Literal(Int), <15:1>Literal(Int), <18:1>Literal(Int)])) } }, <21:3>Block([]))


"
    );
}

#[test_log::test]
fn enum_literal_basic() {
    check(
        r"
        state = State::Running

    ",
        r"

<9:28>VariableAssignment(<9:5>, MutableOrImmutableExpression { is_mutable: None, expression: <17:20>Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<17:5>), module_path: None, generic_params: [] }, LocalTypeIdentifier(<24:7>)))) })

        ",
    );
}

#[test_log::test]
fn enum_literal() {
    check(
        r"
        enum State {
            Running,
            Stopped,
        }

        state = State::Running
        print(state)
    ",
        r"

EnumDef(<14:5>, [Simple(<34:7>), Simple(<55:7>)])
---
VariableAssignment(<83:5>, Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:5>), module_path: None }, LocalTypeIdentifier(<98:7>)))))
FunctionCall(VariableAccess(<114:5>), [VariableAccess(<120:5>)])

        ",
    );
}

/*
#[test_log::test]
fn enum_literal_with_path() {
    check(
        r"
        enum State {
            Running,
            Stopped,
        }

        state = mangrove::save_game::State::Running
        print(state)
    ",
        r#"

EnumDef(<14:5>, [Simple(<34:7>), Simple(<55:7>)])
---
VariableAssignment(<83:5>, Literal(EnumVariant(Simple(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<91:5>), module_path: None })))))
FunctionCall(VariableAccess(<114:5>), [VariableAccess(<120:5>)]))

        "#,
    );
}
*/

#[test_log::test]
fn impl_def() {
    check(
        r"
            impl SomeTypeName {
                fn something(self) -> Int {
                    self.x
                }

                fn another(mut self, v: Int) -> Int {
                    self.x = 3
                }

                fn no_self_here() -> Float {
                    3.2
                }
            }
        ",
        r"

ImplDef(<18:12>, [Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <52:9>, params: [], self_parameter: Some(SelfParameter { is_mutable: None, self_node: <62:4> }), return_type: Some(Int(<71:3>)) }, body: Block([FieldAccess(VariableAccess(<97:4>), <102:1>)]) }), Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <142:7>, params: [Parameter { variable: <160:1>, param_type: Int(<163:3>) }], self_parameter: Some(SelfParameter { is_mutable: Some(<150:3>), self_node: <154:4> }), return_type: Some(Int(<171:3>)) }, body: Block([FieldAssignment(VariableAccess(<197:4>), <202:1>, Literal(Int(<206:1>)))]) }), Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <246:12>, params: [], self_parameter: None, return_type: Some(Float(<264:5>)) }, body: Block([Literal(Float(<292:3>))]) })])

        ",
    );
}

#[test_log::test]
fn match_pattern_literal() {
    check(
        r#"
        match x {
            5 => "five",
            "hello" => "greeting",
            true => "yes",
            _ => "something else"
        }"#,
        r"

Match(VariableAccess(<15:1>), [MatchArm { pattern: Literal(Int(<31:1>)), expression: Literal(String(<36:6>)) }, MatchArm { pattern: Literal(String(<56:7>)), expression: Literal(String(<67:10>)) }, MatchArm { pattern: Literal(Bool(<91:4>)), expression: Literal(String(<99:5>)) }, MatchArm { pattern: PatternList([Wildcard(<118:1>)]), expression: Literal(String(<123:16>)) }])

        ",
    );
}

#[test_log::test]
fn match_comment() {
    check(
        r#"
        print("hello") // first
        /// this has some documentation
        print("world") // second
       "#,
        r"

FunctionCall(VariableAccess(<9:5>), [Literal(String(<15:7>))])
FunctionCall(VariableAccess(<81:5>), [Literal(String(<87:7>))])

        ",
    );
}

#[test_log::test]
fn multiple_assignments() {
    check(
        r"
        x = y = z = 10
        ",
        r"
VariableAssignment(<9:1>, VariableAssignment(<13:1>, VariableAssignment(<17:1>, Literal(Int(<21:2>)))))
        ",
    );
}

#[test_log::test]
fn enum_variant_construction() {
    check(
        r"

        shape = Shape::Rectangle { width: 10, height: 20 }

        ",
        r"
<10:60>VariableAssignment(<10:5>, <18:42>Literal(EnumVariant(Struct(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:5>), module_path: None, generic_params: [] }, LocalTypeIdentifier(<25:9>), [FieldExpression { field_name: FieldName(<37:5>), expression: <44:2>Literal(Int) }, FieldExpression { field_name: FieldName(<48:6>), expression: <56:2>Literal(Int) }]))))

",
    );
}

#[test_log::test]
fn enum_variant_tuple_construction() {
    check(
        r"
        shape = Shape::Something(2, 4.4)
        ",
        r"
<10:60>VariableAssignment(<10:5>, <18:42>Literal(EnumVariant(Struct(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:5>), module_path: None, generic_params: [] }, LocalTypeIdentifier(<25:9>), [FieldExpression { field_name: FieldName(<37:5>), expression: <44:2>Literal(Int) }, FieldExpression { field_name: FieldName(<48:6>), expression: <56:2>Literal(Int) }]))))

        ",
    );
}

#[test_log::test]
fn array_access() {
    check(
        r"
        arr[3]
        ",
        "<9:6>PostfixChain(PostfixChain { base: <9:3>IdentifierReference(<9:3>), suffixes: [Subscript(<13:1>Literal(Int))] })",
    );
}

#[test_log::test]
fn array_set() {
    check(
        r"
        arr[3] = 42
        ",
        r"
<9:20>Assignment(<9:6>PostfixChain(PostfixChain { base: <9:3>IdentifierReference(<9:3>), suffixes: [Subscript(<13:1>Literal(Int))] }), <18:2>Literal(Int))
        ",
    );
}

#[test_log::test]
fn reassign() {
    check(
        r"
        mut a = 42
        a = 43
        ",
        r"
<9:19>VariableDefinition(mut <9:3> <13:1>, None, MutableOrImmutableExpression { is_mutable: None, expression: <17:2>Literal(Int) })
<28:15>VariableAssignment(<28:1>, MutableOrImmutableExpression { is_mutable: None, expression: <32:2>Literal(Int) })
",
    );
}

#[test_log::test]
fn string_interpolation_basic_spaces() {
    check(
        "'   this is interpolated {x}   with hex  {y}  '",
        r#"
<0:47>InterpolatedString([Literal(<1:24>, "   this is interpolated "), Interpolation(<26:1>IdentifierReference(<26:1>), None), Literal(<28:13>, "   with hex  "), Interpolation(<42:1>IdentifierReference(<42:1>), None), Literal(<44:2>, "  ")])

    "#,
    );
}

#[test_log::test]
fn string_interpolation() {
    check(
        "'this is interpolated {x} with hex {y:x}'",
        r#"
<0:41>InterpolatedString([Literal(<1:21>, "this is interpolated "), Interpolation(<23:1>IdentifierReference(<23:1>), None), Literal(<25:10>, " with hex "), Interpolation(<36:1>IdentifierReference(<36:1>), Some(LowerHex(<38:1>)))])

    "#,
    );
}

#[test_log::test]
fn string_interpolation_call() {
    check(
        "'this is interpolated {x:x}    with hex  {mul(a, 2)}'",
        r#"
<0:53>InterpolatedString([Literal(<1:21>, "this is interpolated "), Interpolation(<23:1>IdentifierReference(<23:1>), Some(LowerHex(<25:1>))), Literal(<27:14>, "    with hex  "), Interpolation(<42:9>PostfixChain(PostfixChain { base: <42:3>IdentifierReference(<42:3>), suffixes: [FunctionCall(<45:6>, [MutableOrImmutableExpression { is_mutable: None, expression: <46:1>IdentifierReference(<46:1>) }, MutableOrImmutableExpression { is_mutable: None, expression: <49:1>Literal(Int) }])] }), None)])

    "#,
    );
}

#[test_log::test]
fn string_interpolation_call_simple() {
    check(
        "'result: {mul(a,2)}'",
        r"
InterpolatedString([Literal(<1:8>), Interpolation(FunctionCall(VariableAccess(<10:3>), [VariableAccess(<14:1>), Literal(Int(<16:1>))]), None)])
    ",
    );
}

#[test_log::test]
fn string_interpolation_simple() {
    check(
        "'this is interpolated {x}'",
        r"
InterpolatedString([Literal(<1:21>), Interpolation(VariableAccess(<23:1>), None)])
    ",
    );
}

#[test_log::test]
fn string_interpolation_simple_no_space() {
    check(
        "'this is interpolated{x}'",
        r"
InterpolatedString([Literal(<1:20>), Interpolation(VariableAccess(<22:1>), None)])
    ",
    );
}

#[test_log::test]
fn function_call_mul() {
    check(
        "mul(a, 2)",
        r"
<0:9>PostfixChain(PostfixChain { base: <0:3>IdentifierReference(<0:3>), suffixes: [FunctionCall(<3:6>, [MutableOrImmutableExpression { is_mutable: None, expression: <4:1>IdentifierReference(<4:1>) }, MutableOrImmutableExpression { is_mutable: None, expression: <7:1>Literal(Int) }])] })

    ",
    );
}

#[test_log::test]
fn enum_match() {
    check(
        "
        msg = match result {
            Ok value => value,
            Err err => err
        }
        ",
        "

<9:88>VariableAssignment(<9:3>, MutableOrImmutableExpression { is_mutable: None, expression: <15:82>Match(<21:6>IdentifierReference(<21:6>), [MatchArm { pattern: NormalPattern(<42:9>, EnumPattern(<42:2>, Some([Variable(<45:5>)])), None), expression: <54:5>IdentifierReference(<54:5>) }, MatchArm { pattern: NormalPattern(<73:8>, EnumPattern(<73:3>, Some([Variable(<77:3>)])), None), expression: <84:3>IdentifierReference(<84:3>) }]) })

",
    );
}

#[test_log::test]
fn enum_match_simple() {
    check(
        "
        msg = match result {
            Ok value => value,
            Err err => err,
            Simple => 2
        }
        ",
        "
 VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Variable(<45:5>)])), expression: VariableAccess(<54:5>) }, MatchArm { pattern: EnumPattern(<73:3>, Some([Variable(<77:3>)])), expression: VariableAccess(<84:3>) }, MatchArm { pattern: EnumPattern(<101:6>, None), expression: Literal(Int(<111:1>)) }]))
     ",
    );
}

#[test_log::test]
fn enum_match_wildcard() {
    check(
        "
        msg = match result {
            Ok(value) => value,
            Err(err) => err,
            Simple => 2,
            _ => 99
        }
        ",
        "
<9:136>VariableAssignment(<9:3>, MutableOrImmutableExpression { is_mutable: None, expression: <15:130>Match(<21:6>IdentifierReference(<21:6>), [MatchArm { pattern: NormalPattern(<42:10>, EnumPattern(<42:2>, Some([Expression(<44:7>Literal(Tuple([<45:5>IdentifierReference(<45:5>)])))])), None), expression: <55:5>IdentifierReference(<55:5>) }, MatchArm { pattern: NormalPattern(<74:9>, EnumPattern(<74:3>, Some([Expression(<77:5>Literal(Tuple([<78:3>IdentifierReference(<78:3>)])))])), None), expression: <86:3>IdentifierReference(<86:3>) }, MatchArm { pattern: NormalPattern(<103:7>, EnumPattern(<103:6>, None), None), expression: <113:1>Literal(Int) }, MatchArm { pattern: Wildcard(<128:1>), expression: <133:2>Literal(Int) }]) })
      ",
    );
}

#[test_log::test]
fn enum_match_with_wildcard() {
    check(
        r"
        msg = match result {
            Ok(value) => value,
            _ => 99
        }
        ",
        "
VariableAssignment(<9:3>, Match(VariableAccess(<21:6>), [MatchArm { pattern: EnumPattern(<42:2>, Some([Expression(VariableAccess(<45:5>))])), expression: VariableAccess(<55:5>) }, MatchArm { pattern: PatternList([Wildcard(<74:1>)]), expression: Literal(Int(<79:2>)) }]))
",
    );
}
#[test_log::test]
fn enum_match_struct_y() {
    check(
        r#"

        enum Action {
            Jumping,
            Target { x: Int, y: Int },
            Other(String),
        }

        action = Action::Target { x:42, y: -999 }

        /*
        match the actions
            - Jumping
            - Target
        */

        match action {
            Jumping => "jumping",
            Target y => y,
            _ => "can not find it!",
        }
    "#,
        "

EnumDef(<15:6>, [Simple(<36:7>), Struct(<57:6>, AnonymousStructType { fields: [FieldType { field_name: FieldName(<66:1>), field_type: Int(<69:3>) }, FieldType { field_name: FieldName(<74:1>), field_type: Int(<77:3>) }] }), Tuple(<96:5>, [String(<102:6>)])])
---
VariableAssignment(<130:6>, Literal(EnumVariant(Struct(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<139:6>), module_path: None }, LocalTypeIdentifier(<147:6>), [FieldExpression { field_name: FieldName(<156:1>), expression: Literal(Int(<158:2>)) }, FieldExpression { field_name: FieldName(<162:1>), expression: UnaryOp(Negate(<165:1>), Literal(Int(<166:3>))) }]))))
Match(VariableAccess(<279:6>), [MatchArm { pattern: EnumPattern(<300:7>, None), expression: Literal(String(<311:9>)) }, MatchArm { pattern: EnumPattern(<334:6>, Some([Variable(<341:1>)])), expression: VariableAccess(<346:1>) }, MatchArm { pattern: PatternList([Wildcard(<361:1>)]), expression: Literal(String(<366:18>)) }])
        ",
    );
}

#[test_log::test]
fn enum_match_tuple_basic() {
    check(
        r#"
           match v {
Tuple(i, s, b) => {
// Tuple is an imaginary enum variant with name Tuple
                    print("Tuple:")
                    print(i)
                    print(s)
                    print(b)
                }
            }
    "#,
        r#"

<12:254>Match(<18:1>IdentifierReference(<18:1>), [MatchArm { pattern: NormalPattern(<38:15>, EnumPattern(<38:5>, Some([Expression(<43:9>Literal(Tuple([<44:1>IdentifierReference(<44:1>), <47:1>IdentifierReference(<47:1>), <50:1>IdentifierReference(<50:1>)])))])), None), expression: <132:15>PostfixChain(PostfixChain { base: <132:5>IdentifierReference(<132:5>), suffixes: [FunctionCall(<137:10>, [MutableOrImmutableExpression { is_mutable: None, expression: <138:8>Literal(String("Tuple:")) }])] }) }])

    "#,
    );
}

#[test_log::test]
fn print_if() {
    check(
        r#"
       if x > 41 {
            "x is greater than 41"
        }
    "#,
        r#"<8:61>If(<11:7>BinaryOp(<11:1>IdentifierReference(<11:1>), BinaryOperator { kind: GreaterThan, node: <13:1> }, <15:2>Literal(Int)), <32:22>Literal(String("x is greater than 41")), None)
"#,
    );
}

#[test_log::test]
fn for_continue() {
    check(
        r#"
       for i in 1..5 {

            if i % 2 == 0 {
                continue
            }

            print("no continue")
        }
        "#,
        r"

ForLoop(Single(ForVar { identifier: <12:1>, is_mut: None }), IteratableExpression { is_mut: None, expression: ExclusiveRange(Literal(Int(<17:1>)), Literal(Int(<20:1>))) }, Block([If(BinaryOp(BinaryOp(VariableAccess(<40:1>), Modulo(<42:1>), Literal(Int(<44:1>))), Equal(<46:2>), Literal(Int(<49:1>))), Block([Continue(<69:8>)]), None), FunctionCall(VariableAccess(<105:5>), [Literal(String(<111:13>))])]))

    ",
    );
}

#[test_log::test]
fn else_problem() {
    check(
        r#"
         if x < 0 {
                return "negative"
        } else if x == 0 {
            return "positive"
        }
            "#,
        r"

If(BinaryOp(VariableAccess(<13:1>), LessThan(<15:1>), Literal(Int(<17:1>))), Block([Return(Some(Literal(String(<44:10>))))]), Some(If(BinaryOp(VariableAccess(<73:1>), Equal(<75:2>), Literal(Int(<78:1>))), Block([Return(Some(Literal(String(<101:10>))))]), None)))

            ",
    );
}

#[test_log::test]
fn option_operator() {
    check(
        r"
         a?
            ",
        r"
PostfixOp(Unwrap(<11:1>), VariableAccess(<10:1>))

            ",
    );
}

#[test_log::test]
fn option_operator_expr() {
    check(
        "
         b + a?
            ",
        "

<10:19>BinaryOp(<10:1>IdentifierReference(<10:1>), BinaryOperator { kind: Add, node: <12:1> }, <14:2>PostfixChain(PostfixChain { base: <14:1>IdentifierReference(<14:1>), suffixes: [OptionUnwrap(<15:1>)] }))

            ",
    );
}

#[test_log::test]
fn option_operator_if_variable() {
    check(
        r"
         if a? {
         'this is {a}'
         } else {
         'not here'
         }
            ",
        r"

If(PostfixOp(Unwrap(<14:1>), VariableAccess(<13:1>)), Block([InterpolatedString([Literal(<28:8>), Interpolation(VariableAccess(<37:1>), None)])]), Some(Block([InterpolatedString([Literal(<69:8>)])])))

            ",
    );
}

#[test_log::test]
fn option_operator_if_expression() {
    check(
        r"
         if (b*3+99+something.call(42))? {
         'expression is something'
         } else {
         'must be none'
         }
            ",
        r"

If(PostfixOp(Unwrap(<40:1>), BinaryOp(BinaryOp(BinaryOp(VariableAccess(<14:1>), Multiply(<15:1>), Literal(Int(<16:1>))), Add(<17:1>), Literal(Int(<18:2>))), Add(<20:1>), MemberCall(VariableAccess(<21:9>), <31:4>, [Literal(Int(<36:2>))]))), Block([InterpolatedString([Literal(<54:23>)])]), Some(Block([InterpolatedString([Literal(<107:12>)])])))

            ",
    );
}

#[test_log::test]
fn option_operator_assignment() {
    check(
        r"
         a = another.get_current()?
            ",
        r"

VariableAssignment(<10:1>, PostfixOp(Unwrap(<35:1>), MemberCall(VariableAccess(<14:7>), <22:11>, [])))
            ",
    );
}

#[test_log::test]
fn option_operator_assignment_chained() {
    check(
        r"
         a = another.get_current()?.another_call(b, 42)?
            ",
        r"

VariableAssignment(<10:1>, PostfixOp(Unwrap(<56:1>), MemberCall(PostfixOp(Unwrap(<35:1>), MemberCall(VariableAccess(<14:7>), <22:11>, [])), <37:12>, [VariableAccess(<50:1>), Literal(Int(<53:2>))])))

            ",
    );
}

#[test_log::test]
fn option_operator_if_let_expression() {
    check(
        r"
         if a = another.get_current()? {
               'this is {a}'
         } else {
            'must be none'
         }
            ",
        r"

If(VariableAssignment(<13:1>, PostfixOp(Unwrap(<38:1>), MemberCall(VariableAccess(<17:7>), <25:11>, []))), Block([InterpolatedString([Literal(<58:8>), Interpolation(VariableAccess(<67:1>), None)])]), Some(Block([InterpolatedString([Literal(<102:12>)])])))
          ",
    );
}

#[test_log::test]
fn option_operator_if_let_expression_multiple_calls() {
    check(
        r"
         if a = another.get_current()?.another_call(b, 42)? {
               'this is {a}'
         } else {
            'must be none'
         }
            ",
        r"

If(VariableAssignment(<13:1>, PostfixOp(Unwrap(<59:1>), MemberCall(PostfixOp(Unwrap(<38:1>), MemberCall(VariableAccess(<17:7>), <25:11>, [])), <40:12>, [VariableAccess(<53:1>), Literal(Int(<56:2>))]))), Block([InterpolatedString([Literal(<79:8>), Interpolation(VariableAccess(<88:1>), None)])]), Some(Block([InterpolatedString([Literal(<123:12>)])])))

            ",
    );
}

#[test_log::test]
fn none_assignment() {
    check(
        r"
        a = none
            ",
        r"
VariableAssignment(<9:1>, Literal(None(<13:4>)))

            ",
    );
}

#[test_log::test]
fn if_assignment() {
    check(
        "
    a = 3
    c = if b = a > 3 {
        b + 1
    } else {
        b + 2
    }

            ",
        "

VariableAssignment(<5:1>, Literal(Int(<9:1>)))
VariableAssignment(<15:1>, If(VariableAssignment(<22:1>, BinaryOp(VariableAccess(<26:1>), GreaterThan(<28:1>), Literal(Int(<30:1>)))), Block([BinaryOp(VariableAccess(<42:1>), Add(<44:1>), Literal(Int(<46:1>)))]), Some(Block([BinaryOp(VariableAccess(<69:1>), Add(<71:1>), Literal(Int(<73:1>)))]))))

            ",
    );
}

#[test_log::test]
fn struct_field_optional() {
    check(
        "
  struct Struct {
    some_field: Int?
   }
            ",
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<10:6>), fields: [FieldType { field_name: FieldName(<23:10>), field_type: Optional(Int(<35:3>), <38:1>) }] })

            ",
    );
}

#[test_log::test]
fn map_literal() {
    check(
        "
  a = [2: 'Hello', 3: 'World']

            ",
        r"

VariableAssignment(<3:1>, Literal(Map([(Literal(Int(<8:1>)), InterpolatedString([Literal(<12:5>)])), (Literal(Int(<20:1>)), InterpolatedString([Literal(<24:5>)]))])))

            ",
    );
}

#[test_log::test]
fn map_literal_no_spaces() {
    check(
        "
  a = [2:'Hello',3:'World']

            ",
        r"

VariableAssignment(<3:1>, Literal(Map([(Literal(Int(<8:1>)), InterpolatedString([Literal(<11:5>)])), (Literal(Int(<18:1>)), InterpolatedString([Literal(<21:5>)]))])))

            ",
    );
}

#[test_log::test]
fn map_fn() {
    check(
        "
fn map_creator() -> [Int: String] {
[2: 'hello', -1: 'world']
}

            ",
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:11>, params: [], self_parameter: None, return_type: Some(Map(Int(<22:3>), String(<27:6>))) }, body: Block([Literal(Map([(Literal(Int(<38:1>)), InterpolatedString([Literal(<42:5>)])), (UnaryOp(Negate(<50:1>), Literal(Int(<51:1>))), InterpolatedString([Literal(<55:5>)]))]))]) }))
            ",
    );
}

#[test_log::test]
fn int_fn() {
    check(
        "
fn test() -> Int { 42 }
            ",
        "

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:4>, params: [], self_parameter: None, return_type: Some(Int(<14:3>)) }, body: Block([Literal(Int(<20:2>))]) }))


            ",
    );
}

#[test_log::test]
fn generic_type() {
    check(
        "
fn nothing() -> SomeType<Int, Float> {
}

            ",
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:7>, params: [], self_parameter: None, return_type: Some(Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<17:8>), module_path: None, generic_params: [Int(<26:3>), Float(<31:5>)] }), [Int(<26:3>), Float(<31:5>)])) }, body: Block([]), constants: [] }))

           ",
    );
}

#[test_log::test]
fn sparse_map_static_call() {
    check(
        r"
        result = SparseMap<Int>::new()
        ",
        r"
<9:39>VariableAssignment(<9:6>, MutableOrImmutableExpression { is_mutable: None, expression: <18:21>PostfixChain(PostfixChain { base: <18:19>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<18:9>), module_path: None, generic_params: [Int(<28:3>)] }, <34:3>), suffixes: [FunctionCall(<37:2>, [])] }) })

",
    );
}

#[test_log::test]
fn assignment_op_add() {
    check(
        "a += 6",
        "<0:6>CompoundAssignment(<0:1>IdentifierReference(<0:1>), CompoundOperator { node: <2:2>, kind: Add }, <5:1>Literal(Int))",
    );
}

#[test_log::test]
fn check_some_bug() {
    check(
        "
        is_attacking = false
        c = if is_attacking { 3.5 } else { -13.3 }
    ",
        r"
<9:29>VariableAssignment(<9:12>, MutableOrImmutableExpression { is_mutable: None, expression: <24:5>Literal(Bool) })
<38:42>VariableAssignment(<38:1>, MutableOrImmutableExpression { is_mutable: None, expression: <42:38>If(<45:13>IdentifierReference(QualifiedIdentifier { name: <45:12>, module_path: None, generic_params: [] }), <58:7>Block([<60:3>Literal(Float)]), Some(<71:9>Block([<73:6>UnaryOp(Negate(<73:1>), <74:4>Literal(Float))]))) })


        ",
    );
}

#[test_log::test]
fn check_bool() {
    check(
        r"
        is_attacking = false
    ",
        "
<9:25>VariableAssignment(<9:12>, MutableOrImmutableExpression { is_mutable: None, expression: <24:5>Literal(Bool) })

        ",
    );
}

#[test_log::test]
fn check_mut_bool() {
    check(
        r"
        mut is_attacking = false
    ",
        "
<9:29>VariableDefinition(mut <9:3> <13:12>, None, MutableOrImmutableExpression { is_mutable: None, expression: <28:5>Literal(Bool) })

        ",
    );
}

#[test_log::test]
fn check_return_type() {
    check(
        r"
        fn x(a: Int) -> (Int, Float) {
        }
    ",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:1>, params: [Parameter { variable: <14:1>, param_type: Int(<17:3>) }], self_parameter: None, return_type: Some(Tuple([Int(<26:3>), Float(<31:5>)])) }, body: <38:11>Block([]) }))

    ",
    );
}

#[test_log::test]
fn check_prefix_and_generic() {
    check(
        r"
        struct Logic {
tick_count: Int, /// how many ticks have passed
            explosions: std::Sparse<Explosion>,
         }
",
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<16:5>), fields: [FieldType { field_name: FieldName(<36:10>), field_type: Int(<48:3>) }, FieldType { field_name: FieldName(<96:10>), field_type: Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<113:6>), module_path: Some(ModulePath([<108:3>])), generic_params: [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<120:9>), module_path: None, generic_params: [] })] }), [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<120:9>), module_path: None, generic_params: [] })]) }] })

",
    );
}

#[test_log::test]
fn check_boolean_expression() {
    check(
        r"
    if !enemy.rect.intersects(shot_rect) {
    }
",
        r"
<5:45>If(<8:33>UnaryOp(Not(<8:1>), <9:32>PostfixChain(PostfixChain { base: <9:5>IdentifierReference(<9:5>), suffixes: [FieldAccess(<15:4>), MemberCall(<20:10>, [MutableOrImmutableExpression { is_mutable: None, expression: <31:9>IdentifierReference(<31:9>) }])] })), <42:7>Block([]), None)

        ",
    );
}

#[test_log::test]
fn check_boolean_expression_no_block() {
    check(
        r"
    if !enemy.rect.intersects(shot_rect) 2 else 3
",
        "
<5:46>If(<8:33>UnaryOp(Not(<8:1>), <9:32>PostfixChain(PostfixChain { base: <9:5>IdentifierReference(<9:5>), suffixes: [FieldAccess(<15:4>), MemberCall(<20:10>, [MutableOrImmutableExpression { is_mutable: None, expression: <31:9>IdentifierReference(<31:9>) }])] })), <42:1>Literal(Int), Some(<49:1>Literal(Int)))

        ",
    );
}

#[test_log::test]
fn documentation_comment() {
    check(
        r"
/// This is a doc comment
x = if true 2 else 4
",
        r"
VariableAssignment(<35:1>, If(Literal(Bool(<42:4>)), Literal(Int(<47:1>)), Some(Literal(Int(<54:1>)))))
        ",
    );
}

#[test_log::test]
fn external_function() {
    check(
        r"
/// This is a doc comment
external fn something(i: Int) -> Float
",
        r"
FunctionDef(External(FunctionDeclaration { name: <47:9>, params: [Parameter { variable: <57:1>, param_type: Int(<60:3>) }], self_parameter: None, return_type: Some(Float(<68:5>)) }))
        ",
    );
}

#[test_log::test]
fn external_member_function() {
    check(
        r"
        struct Api {
            x : Int,
        }

        impl Api {
            external fn something(self, i: Int) -> Float {}
        }
",
        r"
If(UnaryOp(Not(<8:1>), MemberCall(FieldAccess(VariableAccess(<9:5>), <15:4>), <20:10>, [VariableAccess(<31:9>)])), Literal(Int(<42:1>)), Some(Literal(Int(<49:1>))))
        ",
    );
}

#[test_log::test]
fn variable() {
    check(
        r"
some_variable
",
        r"
VariableAccess(<1:13>)
        ",
    );
}

#[test_log::test]
fn block() {
    check(
        r"
if true {
    {
        a = 3
    }
}
",
        r"
<1:38>If(<4:4>Literal(Bool), <9:29>Block([<15:21>Block([<25:10>VariableAssignment(<25:1>, MutableOrImmutableExpression { is_mutable: None, expression: <29:1>Literal(Int) })])]), None)

        ",
    );
}

#[test_log::test]
fn unary() {
    check(
        r"
-a
",
        r"
UnaryOp(Negate(<1:1>), VariableAccess(<2:1>))
        ",
    );
}

#[test_log::test]
fn unary_not() {
    check(
        r"
!a
",
        r"
UnaryOp(Not(<1:1>), VariableAccess(<2:1>))
        ",
    );
}

#[test_log::test]
fn mut_variable() {
    check(
        r"
mut a = 3
",
        r"
VariableAssignment(mut <1:3> <5:1>, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn compound_sub() {
    check(
        r"
a -= 3
",
        r"
<1:7>AssignmentCompound(<1:1>VariableAccess(<1:1>), CompoundOperator { node: <3:2>, kind: Sub }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn compound_div() {
    check(
        r"
a /= 3
",
        r"
<1:7>CompoundAssignment(<1:1>IdentifierReference(<1:1>), CompoundOperator { node: <3:2>, kind: Div }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn compound_mul() {
    check(
        r"
a *= 3
",
        r"
<1:7>CompoundAssignment(<1:1>IdentifierReference(<1:1>), CompoundOperator { node: <3:2>, kind: Mul }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn compound_modulo() {
    check(
        r"
a %= 3
",
        r"
<1:7>CompoundAssignment(<1:1>IdentifierReference(<1:1>), CompoundOperator { node: <3:2>, kind: Modulo }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn index_compound_modulo() {
    check(
        r"
a[1] %= 3
",
        r"
<1:10>CompoundAssignment(<1:4>PostfixChain(PostfixChain { base: <1:1>IdentifierReference(<1:1>), suffixes: [Subscript(<3:1>Literal(Int))] }), CompoundOperator { node: <6:2>, kind: Modulo }, <9:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn index_compound_add() {
    check(
        r"
a[i] += 3
",
        r"
<1:10>CompoundAssignment(<1:4>PostfixChain(PostfixChain { base: <1:1>IdentifierReference(<1:1>), suffixes: [Subscript(<3:1>IdentifierReference(<3:1>))] }), CompoundOperator { node: <6:2>, kind: Add }, <9:1>Literal(Int))

        ",
    );
}

#[test_log::test]
fn index_compound_sub() {
    check(
        r"
a[i] -= 3
",
        r"
<1:10>CompoundAssignment(<1:4>PostfixChain(PostfixChain { base: <1:1>IdentifierReference(<1:1>), suffixes: [Subscript(<3:1>IdentifierReference(<3:1>))] }), CompoundOperator { node: <6:2>, kind: Sub }, <9:1>Literal(Int))

        ",
    );
}

#[test_log::test]
fn index_compound_mul() {
    check(
        r"
a[i] *= 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Mul }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn index_compound_div() {
    check(
        r"
a[i] /= 3
",
        r"
IndexCompoundAssignment(VariableAccess(<1:1>), VariableAccess(<3:1>), CompoundOperator { node: <6:2>, kind: Div }, Literal(Int(<9:1>)))
        ",
    );
}

#[test_log::test]
fn neq() {
    check(
        r"
a != 5
",
        r"
<1:7>BinaryOp(<1:1>IdentifierReference(<1:1>), BinaryOperator { kind: NotEqual, node: <3:2> }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn lte() {
    check(
        r"
a <= 5
",
        r"
<1:7>BinaryOp(<1:1>IdentifierReference(<1:1>), BinaryOperator { kind: LessEqual, node: <3:2> }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn gte() {
    check(
        r"
a >= 5
",
        r"
<1:7>BinaryOp(<1:1>IdentifierReference(<1:1>), BinaryOperator { kind: GreaterEqual, node: <3:2> }, <6:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn interpolate() {
    check(
        r"
'{a:X} {b:b}  '
",
        r#"
<1:15>InterpolatedString([Interpolation(<3:1>IdentifierReference(<3:1>), Some(UpperHex(<5:1>))), Literal(<7:1>, " "), Interpolation(<9:1>IdentifierReference(<9:1>), Some(Binary(<11:1>))), Literal(<13:2>, "  ")])

        "#,
    );
}

#[test_log::test]
fn generic() {
    check(
        r"
struct Something {
    sparse: Sparse<Int>
}
",
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<8:9>), fields: [FieldType { field_name: FieldName(<24:6>), field_type: Generic(TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<32:6>), module_path: None, generic_params: [Int(<39:3>)] }), [Int(<39:3>)]) }] })

        ",
    );
}

#[test_log::test]
fn optional() {
    check(
        r"
struct Hello {
    first_name: String?,
}
",
        r"
StructDef(StructType { identifier: LocalTypeIdentifier(<8:5>), fields: [FieldType { field_name: FieldName(<20:10>), field_type: Optional(String(<32:6>), <38:1>) }] })

        ",
    );
}

#[test_log::test]
fn pattern_list() {
    check(
        r"
match x {
    a, b => print(a)
}
",
        r"
Match(VariableAccess(<7:1>), [MatchArm { pattern: PatternList([Variable(<15:1>), Variable(<18:1>)]), expression: FunctionCall(VariableAccess(<23:5>), [VariableAccess(<29:1>)]) }])
        ",
    );
}

#[test_log::test]
fn array_type() {
    check(
        r"
fn something(a: Int) -> [Bool] {
    [true, false]
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:9>, params: [Parameter { variable: <14:1>, param_type: Int(<17:3>) }], self_parameter: None, return_type: Some(Array(Bool(<26:4>))) }, body: <32:21>Block([<38:13>Literal(Array([<39:4>Literal(Bool), <45:5>Literal(Bool)]))]) }))
        ",
    );
}

#[test_log::test]
fn destructuring() {
    check(
        "
a, b = 3
",
        "
<8:2>DestructuringAssignment([<1:1>, <4:1>], <8:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn multi_var_assign() {
    check(
        "
a = b = c = 3
",
        "
<1:14>VariableAssignment(<1:1>, MutableOrImmutableExpression { is_mutable: None, expression: <5:10>VariableAssignment(<5:1>, MutableOrImmutableExpression { is_mutable: None, expression: <9:6>VariableAssignment(<9:1>, MutableOrImmutableExpression { is_mutable: None, expression: <13:1>Literal(Int) }) }) })

        ",
    );
}

#[test_log::test]
fn mut_arg() {
    check(
        r"
fn write(mut x: Int) {
    x = 4
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:5>, params: [Parameter { variable: <10:5>, param_type: Int(<17:3>) }], self_parameter: None, return_type: None }, body: <22:13>Block([<28:6>VariableAssignment(<28:1>, MutableOrImmutableExpression { is_mutable: None, expression: <32:1>Literal(Int) })]) }))

        ",
    );
}

#[test_log::test]
fn mut_parameter() {
    let script = r"
/// increments the value
fn increment(mut x: Int) -> Int {
    x = x + 1
    x
}

    ";
    check(
        script,
        "
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <29:9>, params: [Parameter { variable: mut <39:3> <43:1>, param_type: Int(<46:3>) }], self_parameter: None, return_type: Some(Int(<54:3>)) }, body: Block([VariableAssignment(<64:1>, BinaryOp(VariableAccess(<68:1>), Add(<70:1>), Literal(Int(<72:1>)))), VariableAccess(<78:1>)]) }))
",
    );
}

#[test_log::test]
fn mut_let() {
    check(
        "mut x = 3",
        "<0:9>VariableDefinition(mut <0:3> <4:1>, None, MutableOrImmutableExpression { is_mutable: None, expression: <8:1>Literal(Int) })",
    );
}
#[test_log::test]
fn mut_variable_list() {
    check(
        r"
a, mut b = 3
",
        r"
<12:2>DestructuringAssignment([<1:1>, mut <4:3> <8:1>], <12:1>Literal(Int))
        ",
    );
}

#[test_log::test]
fn mut_static_call() {
    check(
        "
Something::update(a, mut b)
",
        "
<1:27>PostfixChain(PostfixChain { base: <1:17>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<1:9>), module_path: None, generic_params: [] }, <12:6>), suffixes: [FunctionCall(<18:10>, [MutableOrImmutableExpression { is_mutable: None, expression: <19:1>IdentifierReference(<19:1>) }, MutableOrImmutableExpression { is_mutable: Some(<22:3>), expression: <26:1>IdentifierReference(<26:1>) }])] })

        ",
    );
}

#[test_log::test]
fn mut_location_field() {
    check(
        r"
something(mut field.a)
",
        r"
<1:22>PostfixChain(PostfixChain { base: <1:9>IdentifierReference(<1:9>), suffixes: [FunctionCall(<10:13>, [MutableOrImmutableExpression { is_mutable: Some(<11:3>), expression: <15:7>PostfixChain(PostfixChain { base: <15:5>IdentifierReference(<15:5>), suffixes: [FieldAccess(<21:1>)] }) }])] })
        ",
    );
}

#[test_log::test]
fn mut_expr() {
    check(
        r"
mut a = mut array[1]
         ",
        r"

<1:20>VariableDefinition(mut <1:3> <5:1>, None, MutableOrImmutableExpression { is_mutable: Some(<9:3>), expression: <13:8>PostfixChain(PostfixChain { base: <13:5>IdentifierReference(<13:5>), suffixes: [Subscript(<19:1>Literal(Int))] }) })

",
    );
}

#[test_log::test]
fn fail_mut_expr() {
    check(
        r"
mut a = mut func()
         ",
        r"

ParseError { span: <9:20>, specific: MutOnlyForVariables }


",
    );
}

#[test_log::test]
fn none() {
    check(
        r"
call(none)
",
        r"
<1:10>PostfixChain(PostfixChain { base: <1:4>IdentifierReference(<1:4>), suffixes: [FunctionCall(<5:6>, [MutableOrImmutableExpression { is_mutable: None, expression: <6:4>Literal(None) }])] })

        ",
    );
}

#[test_log::test]
fn destruct() {
    check(
        r"
x, y = logic.pos.floor()
",
        r"
<8:18>DestructuringAssignment([<1:1>, <4:1>], <8:17>PostfixChain(PostfixChain { base: <8:5>IdentifierReference(<8:5>), suffixes: [FieldAccess(<14:3>), MemberCall(<18:5>, [])] }))
        ",
    );
}

#[test_log::test]
fn use_statement() {
    check(
        r"
use gameplay
",
        r#"
Use(Use { module_path: ModulePath([<5:8>]), assigned_path: ["gameplay"], items: [] })
        "#,
    );
}

#[test_log::test]
fn use_statement_3() {
    check(
        r"
use gameplay.other.another
",
        r#"
Use(Use { module_path: ModulePath([<5:8>, <13:6>, <19:8>]), assigned_path: ["gameplay", ".other", ".another"], items: [] })
        "#,
    );
}

#[test_log::test]
fn use_statement_items() {
    check(
        r"
use gameplay { some_fn, SomeType }
",
        r#"
Use(Use { module_path: ModulePath([<5:8>]), assigned_path: ["gameplay"], items: [Identifier(LocalIdentifier(<16:7>)), Type(LocalTypeIdentifier(<25:8>))] })
        "#,
    );
}

#[test_log::test]
fn use_statement_3_items() {
    check(
        r"
use gameplay.something.other { some_fn, SomeType }
",
        r#"
Use(Use { module_path: ModulePath([<5:8>, <14:9>, <24:5>]), assigned_path: ["gameplay", "something", "other"], items: [Identifier(LocalIdentifier(<32:7>)), Type(LocalTypeIdentifier(<41:8>))] })
        "#,
    );
}

#[test_log::test]
fn qualified_function_call() {
    check(
        r"
gamplay::utility_func(2)
",
        r"
FunctionCall(FunctionAccess(QualifiedIdentifier { name: <10:12>, module_path: Some(ModulePath([<1:7>])) }), [Literal(Int(<23:1>))])
        ",
    );
}

#[test_log::test]
fn qualified_function_call_3() {
    check(
        r"
gamplay::some::other::utility_func(2)
",
        r"
FunctionCall(FunctionAccess(QualifiedIdentifier { name: <23:12>, module_path: Some(ModulePath([<1:7>, <10:4>, <16:5>])) }), [Literal(Int(<36:1>))])
        ",
    );
}

#[test_log::test]
fn qualified_static_call() {
    check(
        r"
gameplay::Something::new()
",
        r"
StaticCall(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<11:9>), module_path: Some(ModulePath([<1:8>])), generic_params: [] }, <22:3>, [])

        ",
    );
}

#[test_log::test]
fn qualified_static_call_generic() {
    check(
        r"
gameplay::Something<TypeParam>::new()
",
        r"
<1:37>PostfixChain(PostfixChain { base: <1:35>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<11:9>), module_path: Some(ModulePath([<1:8>])), generic_params: [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<21:9>), module_path: None, generic_params: [] })] }, <33:3>), suffixes: [FunctionCall(<36:2>, [])] })

        ",
    );
}

#[test_log::test]
fn struct_instantiation_rest() {
    check(
        r"
SomeStruct {
    x: 3,
    ..
}
",
        r"
<1:31>StructLiteral(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<1:10>), module_path: None, generic_params: [] }, [FieldExpression { field_name: FieldName(<18:1>), expression: <21:1>Literal(Int) }], true)

        ",
    );
}

#[test_log::test]
fn struct_instantiation_no_rest() {
    check(
        r"
SomeStruct {
    x: 3,
}
",
        r"
<1:24>StructLiteral(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<1:10>), module_path: None, generic_params: [] }, [FieldExpression { field_name: FieldName(<18:1>), expression: <21:1>Literal(Int) }], false)

        ",
    );
}

#[test_log::test]
fn struct_instantiation_no_rest_no_trailing() {
    check(
        r"
SomeStruct {
    x: 3,
    y: 4
}
",
        r"
<1:33>StructLiteral(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<1:10>), module_path: None, generic_params: [] }, [FieldExpression { field_name: FieldName(<18:1>), expression: <21:1>Literal(Int) }, FieldExpression { field_name: FieldName(<28:1>), expression: <31:1>Literal(Int) }], false)

        ",
    );
}

#[test_log::test]
fn constant_def() {
    check(
        r"
const B = 3
",
        r"
Constant(ConstantInfo { constant_identifier: ConstantIdentifier(<7:1>), expression: <11:1>Literal(Int) })
        ",
    );
}

#[test_log::test]
fn constant_in_function() {
    check(
        r"
fn some_func() {
    const AB = 3
}
",
        r"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:9>, params: [], self_parameter: None, return_type: None }, body: Block([]), constants: [ConstantInfo { constant_identifier: ConstantIdentifier(<28:2>), expression: Literal(Int(<33:1>)) }] }))        ",
    );
}

#[test_log::test]
fn constant_access_in_function() {
    check(
        r"
fn some_func() -> Int {
    const AB = 3

    AB
}
",
        r"

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:9>, params: [], self_parameter: None, return_type: Some(Int(<19:3>)) }, body: Block([ConstantAccess(ConstantIdentifier(<51:2>))]), constants: [ConstantInfo { constant_identifier: ConstantIdentifier(<35:2>), expression: Literal(Int(<40:1>)) }] }))

",
    );
}

#[test_log::test]
fn function_ref() {
    check(
        r"
fn caller(some_fn: (Int) -> Float) -> Float {
    some_fn(2)
}
",
        r"

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:6>, params: [Parameter { variable: <11:7>, param_type: Function([Int(<21:3>)], Float(<29:5>)) }], self_parameter: None, return_type: Some(Float(<39:5>)) }, body: Block([FunctionCall(VariableAccess(<51:7>), [Literal(Int(<59:1>))])]), constants: [] }))
",
    );
}

#[test_log::test]
fn basic_function_call() {
    check(
        r"
fn some_fn(a: Int) -> Float {
    2.0
}

some_fn(-1)

",
        r"

FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <4:7>, params: [Parameter { variable: <12:1>, param_type: Int(<15:3>) }], self_parameter: None, return_type: Some(Float(<23:5>)) }, body: <29:11>Block([<35:3>Literal(Float)]) }))
---
<42:11>PostfixChain(PostfixChain { base: <42:7>IdentifierReference(<42:7>), suffixes: [FunctionCall(<49:4>, [MutableOrImmutableExpression { is_mutable: None, expression: <50:2>UnaryOp(Negate(<50:1>), <51:1>Literal(Int)) }])] })

",
    );
}

#[test_log::test]
fn static_member_call() {
    check(
        r"        struct Test {
            i: Int,
        }

        impl Test {
            fn static_in_member(a: Int) -> Int {
              a * 2
            }
        }

        fn caller(fn: (Int) -> Int, arg: Int) -> Int {
            fn(arg)
        }

        caller(Test::static_in_member, -10)",
        r"

StructDef(StructType { identifier: LocalTypeIdentifier(<15:4>), fields: [FieldType { field_name: FieldName(<34:1>), field_type: Int(<37:3>) }] })
ImplDef(<66:4>, [Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <88:16>, params: [Parameter { variable: <105:1>, param_type: Int(<108:3>) }], self_parameter: None, return_type: Some(Int(<116:3>)) }, body: <120:35>Block([<136:18>BinaryOp(<136:1>IdentifierReference(<136:1>), BinaryOperator { kind: Multiply, node: <138:1> }, <140:1>Literal(Int))]) })])
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <178:6>, params: [Parameter { variable: <185:2>, param_type: Function([TypeForParameter { ast_type: Int(<190:3>), is_mutable: false }], Int(<198:3>)) }, Parameter { variable: <203:3>, param_type: Int(<208:3>) }], self_parameter: None, return_type: Some(Int(<216:3>)) }, body: <220:31>Block([<234:7>PostfixChain(PostfixChain { base: <234:2>IdentifierReference(<234:2>), suffixes: [FunctionCall(<236:5>, [MutableOrImmutableExpression { is_mutable: None, expression: <237:3>IdentifierReference(<237:3>) }])] })]) }))
---
<261:35>PostfixChain(PostfixChain { base: <261:6>IdentifierReference(<261:6>), suffixes: [FunctionCall(<267:29>, [MutableOrImmutableExpression { is_mutable: None, expression: <268:22>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<268:4>), module_path: None, generic_params: [] }, <274:16>) }, MutableOrImmutableExpression { is_mutable: None, expression: <292:3>UnaryOp(Negate(<292:1>), <293:2>Literal(Int)) }])] })

",
    );
}

#[test_log::test]
fn static_member_call_new() {
    check(
        r"
        Sparse<Shot>::new()
         ",
        r"
<9:19>PostfixChain(PostfixChain { base: <9:17>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<9:6>), module_path: None, generic_params: [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<16:4>), module_path: None, generic_params: [] })] }, <23:3>), suffixes: [FunctionCall(<26:2>, [])] })


",
    );
}

#[test_log::test]
fn static_member_call_new_args() {
    check(
        r"
        Sparse<Shot>::new(2, 3)
         ",
        r"
<9:23>PostfixChain(PostfixChain { base: <9:17>StaticMemberFunctionReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<9:6>), module_path: None, generic_params: [TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<16:4>), module_path: None, generic_params: [] })] }, <23:3>), suffixes: [FunctionCall(<26:6>, [MutableOrImmutableExpression { is_mutable: None, expression: <27:1>Literal(Int) }, MutableOrImmutableExpression { is_mutable: None, expression: <30:1>Literal(Int) }])] })


",
    );
}

#[test_log::test]
fn struct_calls() {
    check(
        r"
        fn start(a: Assets) {
            assets.frame_fixed_grid_material_png('player/ship_16x16')
        }
         ",
        r#"
FunctionDef(Internal(FunctionWithBody { declaration: FunctionDeclaration { name: <12:5>, params: [Parameter { variable: <18:1>, param_type: TypeReference(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<21:6>), module_path: None, generic_params: [] }) }], self_parameter: None, return_type: None }, body: <29:81>Block([<43:57>PostfixChain(PostfixChain { base: <43:6>IdentifierReference(<43:6>), suffixes: [MemberCall(<50:29>, [MutableOrImmutableExpression { is_mutable: None, expression: <80:19>InterpolatedString([Literal(<81:17>, "player/ship_16x16")]) }])] })]) }))


"#,
    );
}

#[test_log::test]
fn with_block() {
    check(
        "
        with x, y {
            a = x + y
            a
        }
         ",
        "

<9:57>With([VariableBinding { variable: <14:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <14:1>IdentifierReference(<14:1>) } }, VariableBinding { variable: <17:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <17:2>IdentifierReference(<17:1>) } }], <19:47>Block([<33:22>VariableAssignment(<33:1>, MutableOrImmutableExpression { is_mutable: None, expression: <37:18>BinaryOp(<37:1>IdentifierReference(<37:1>), BinaryOperator { kind: Add, node: <39:1> }, <41:1>IdentifierReference(<41:1>)) }), <55:1>IdentifierReference(<55:1>)]))


",
    );
}

#[test_log::test]
fn with_block_assign() {
    check(
        r"
        with x, y=3 {
            a = x + y
            a
        }
         ",
        r"

<9:59>With([VariableBinding { variable: <14:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <14:1>IdentifierReference(<14:1>) } }, VariableBinding { variable: <17:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <19:1>Literal(Int) } }], <21:47>Block([<35:22>VariableAssignment(<35:1>, MutableOrImmutableExpression { is_mutable: None, expression: <39:18>BinaryOp(<39:1>IdentifierReference(<39:1>), BinaryOperator { kind: Add, node: <41:1> }, <43:1>IdentifierReference(<43:1>)) }), <57:1>IdentifierReference(<57:1>)]))

",
    );
}

#[test_log::test]
fn with_block_mut() {
    check(
        r"
        with x, mut y {
            a = x + y
            y = 4
            a
        }
         ",
        r"

<9:79>With([VariableBinding { variable: <14:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <14:1>IdentifierReference(<14:1>) } }, VariableBinding { variable: mut <17:3> <21:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <17:6>IdentifierReference(mut <17:3> <21:1>) } }], <23:65>Block([<37:22>VariableAssignment(<37:1>, MutableOrImmutableExpression { is_mutable: None, expression: <41:18>BinaryOp(<41:1>IdentifierReference(<41:1>), BinaryOperator { kind: Add, node: <43:1> }, <45:1>IdentifierReference(<45:1>)) }), <59:18>VariableAssignment(<59:1>, MutableOrImmutableExpression { is_mutable: None, expression: <63:1>Literal(Int) }), <77:1>IdentifierReference(<77:1>)]))

",
    );
}

#[test_log::test]
fn with_assign() {
    check(
        r"
        result = with x, mut y {
            a = x + y
            y = 4
            a
        }
         ",
        r"

<9:88>VariableAssignment(<9:6>, MutableOrImmutableExpression { is_mutable: None, expression: <18:79>With([VariableBinding { variable: <23:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <23:1>IdentifierReference(<23:1>) } }, VariableBinding { variable: mut <26:3> <30:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <26:6>IdentifierReference(mut <26:3> <30:1>) } }], <32:65>Block([<46:22>VariableAssignment(<46:1>, MutableOrImmutableExpression { is_mutable: None, expression: <50:18>BinaryOp(<50:1>IdentifierReference(<50:1>), BinaryOperator { kind: Add, node: <52:1> }, <54:1>IdentifierReference(<54:1>)) }), <68:18>VariableAssignment(<68:1>, MutableOrImmutableExpression { is_mutable: None, expression: <72:1>Literal(Int) }), <86:1>IdentifierReference(<86:1>)])) })

",
    );
}

#[test_log::test]
fn with_assign_expr() {
    check(
        r"
        result = with x = fn(2) + 2, mut y = mut struct.field {
            a = x + y
            y = 4
            a
        }
         ",
        r"

<9:119>VariableAssignment(<9:6>, MutableOrImmutableExpression { is_mutable: None, expression: <18:110>With([VariableBinding { variable: <23:1>, expression: MutableOrImmutableExpression { is_mutable: None, expression: <27:9>BinaryOp(<27:5>PostfixChain(PostfixChain { base: <27:2>IdentifierReference(<27:2>), suffixes: [FunctionCall(<29:3>, [MutableOrImmutableExpression { is_mutable: None, expression: <30:1>Literal(Int) }])] }), BinaryOperator { kind: Add, node: <33:1> }, <35:1>Literal(Int)) } }, VariableBinding { variable: mut <38:3> <42:1>, expression: MutableOrImmutableExpression { is_mutable: Some(<46:3>), expression: <50:12>PostfixChain(PostfixChain { base: <50:6>IdentifierReference(<50:6>), suffixes: [FieldAccess(<57:5>)] }) } }], <63:65>Block([<77:22>VariableAssignment(<77:1>, MutableOrImmutableExpression { is_mutable: None, expression: <81:18>BinaryOp(<81:1>IdentifierReference(<81:1>), BinaryOperator { kind: Add, node: <83:1> }, <85:1>IdentifierReference(<85:1>)) }), <99:18>VariableAssignment(<99:1>, MutableOrImmutableExpression { is_mutable: None, expression: <103:1>Literal(Int) }), <117:1>IdentifierReference(<117:1>)])) })

",
    );
}
#[test_log::test]
fn guard_expression() {
    check(
        r"
a = | some_bool_expression -> result
         ",
        r"

VariableAssignment(<2:1>, Guard([GuardExpr { condition: VariableAccess(<8:20>), result: VariableAccess(<32:6>) }], None))

",
    );
}

#[test_log::test]
fn multiple_guards() {
    check(
        r"
a =
| some_bool_expression -> result
| a > 0 || b < 3 -> 'hello'
         ",
        r"

ariableAssignment(<2:1>, Guard([GuardExpr { condition: VariableAccess(<9:20>), result: VariableAccess(<33:6>) }, GuardExpr { condition: BinaryOp(BinaryOp(VariableAccess(<42:1>), GreaterThan(<44:1>), Literal(Int(<46:1>))), LogicalOr(<48:2>), BinaryOp(VariableAccess(<51:1>), LessThan(<53:1>), Literal(Int(<55:1>)))), result: InterpolatedString([Literal(<61:5>)]) }], None))


",
    );
}

#[test_log::test]
fn multiple_guards_with_wildcard() {
    check(
        r"
a =
| some_bool_expression -> result
| a > 0 || b < 3 -> 'hello'
| _ -> 'saved by the wildcard'
         ",
        r"

VariableAssignment(<2:1>, Guard([GuardExpr { condition: VariableAccess(<9:20>), result: VariableAccess(<33:6>) }, GuardExpr { condition: BinaryOp(BinaryOp(VariableAccess(<42:1>), GreaterThan(<44:1>), Literal(Int(<46:1>))), LogicalOr(<48:2>), BinaryOp(VariableAccess(<51:1>), LessThan(<53:1>), Literal(Int(<55:1>)))), result: InterpolatedString([Literal(<61:5>)]) }], Some(InterpolatedString([Literal(<76:21>)]))))


",
    );
}

#[test_log::test]
fn default_operator() {
    check(
        r"
a = none
a ?? 23
         ",
        r"

VariableAssignment(<2:1>, Guard([GuardExpr { condition: VariableAccess(<9:20>), result: VariableAccess(<33:6>) }, GuardExpr { condition: BinaryOp(BinaryOp(VariableAccess(<42:1>), GreaterThan(<44:1>), Literal(Int(<46:1>))), LogicalOr(<48:2>), BinaryOp(VariableAccess(<51:1>), LessThan(<53:1>), Literal(Int(<55:1>)))), result: InterpolatedString([Literal(<61:5>)]) }], Some(InterpolatedString([Literal(<76:21>)]))))


",
    );
}

#[test_log::test]
fn default_operator_chain() {
    check(
        r"
a = none
a ?? 23 ?? 99
         ",
        r"

VariableAssignment(<2:1>, Guard([GuardExpr { condition: VariableAccess(<9:20>), result: VariableAccess(<33:6>) }, GuardExpr { condition: BinaryOp(BinaryOp(VariableAccess(<42:1>), GreaterThan(<44:1>), Literal(Int(<46:1>))), LogicalOr(<48:2>), BinaryOp(VariableAccess(<51:1>), LessThan(<53:1>), Literal(Int(<55:1>)))), result: InterpolatedString([Literal(<61:5>)]) }], Some(InterpolatedString([Literal(<76:21>)]))))


",
    );
}

#[test_log::test]
fn array_range_access() {
    check(
        r"
a[3..4]
         ",
        r"

RangeAccess(VariableAccess(<1:1>), Literal(Int(<3:1>)), Literal(Int(<6:1>)), Exclusive)

",
    );
}

#[test_log::test]
fn array_range_access_expr_exclusive() {
    check(
        r"
a[some_fn()+33..some_var]
         ",
        r"

RangeAccess(VariableAccess(<1:1>), BinaryOp(FunctionCall(VariableAccess(<3:7>), []), Add(<12:1>), Literal(Int(<13:2>))), VariableAccess(<17:8>), Exclusive)


",
    );
}

#[test_log::test]
fn array_range_access_expr_inclusive() {
    check(
        r"
a[some_fn()+33..=some_var]
         ",
        r"

RangeAccess(VariableAccess(<1:1>), BinaryOp(FunctionCall(VariableAccess(<3:7>), []), Add(<12:1>), Literal(Int(<13:2>))), VariableAccess(<18:8>), Inclusive)


",
    );
}

#[test_log::test]
fn assignment_coerce() {
    check(
        r"
a:Int = some_func()
         ",
        r"

<1:29>VariableDefinition(<1:1>, Some(Int(<3:3>)), MutableOrImmutableExpression { is_mutable: None, expression: <9:11>PostfixChain(PostfixChain { base: <9:9>IdentifierReference(<9:9>), suffixes: [FunctionCall(<18:2>, [])] }) })


",
    );
}

#[test_log::test]
fn assignment_coerce_if() {
    check(
        r"
a:Int? = if true 0 else none
         ",
        r"

<1:38>VariableDefinition(<1:1>, Some(Optional(Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<3:3>), module_path: None, generic_params: [] }), <6:1>)), MutableOrImmutableExpression { is_mutable: None, expression: <10:29>If(<13:4>Literal(Bool), <18:1>Literal(Int), Some(<25:4>Literal(None))) })

",
    );
}

#[test_log::test]
fn assignment_simple() {
    check(
        r"
a = fn()
         ",
        r"

<1:18>VariableAssignment(<1:1>, MutableOrImmutableExpression { is_mutable: None, expression: <5:4>PostfixChain(PostfixChain { base: <5:2>IdentifierReference(<5:2>), suffixes: [FunctionCall(<7:2>, [])] }) })


",
    );
}

#[test_log::test]
fn assignment_simple_add() {
    check(
        r"
a = b + c
         ",
        r"

<1:19>VariableAssignment(<1:1>, MutableOrImmutableExpression { is_mutable: None, expression: <5:15>BinaryOp(<5:1>IdentifierReference(<5:1>), BinaryOperator { kind: Add, node: <7:1> }, <9:1>IdentifierReference(<9:1>)) })


",
    );
}

#[test_log::test]
fn reassignment() {
    check(
        r"
some.field = b + c
         ",
        r"

<1:28>Assignment(<1:10>PostfixChain(PostfixChain { base: <1:4>IdentifierLookup(<1:4>), suffixes: [FieldAccess(<6:5>)] }), <14:15>BinaryOp(<14:1>IdentifierLookup(<14:1>), BinaryOperator { kind: Add, node: <16:1> }, <18:1>IdentifierLookup(<18:1>)))

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
<5:35>VariableDefinition(mut <5:3> <9:1>, None, MutableOrImmutableExpression { is_mutable: None, expression: <13:27>Literal(Map([(<14:1>Literal(Int), <16:7>InterpolatedString([Literal(<17:5>, "hello")])), (<25:2>UnaryOp(Negate(<25:1>), <26:1>Literal(Int)), <32:7>InterpolatedString([Literal(<33:5>, "world")]))])) })
<45:21>Assignment(<45:4>PostfixChain(PostfixChain { base: <45:1>IdentifierReference(<45:1>), suffixes: [Subscript(<47:1>Literal(Int))] }), <52:8>InterpolatedString([Literal(<53:6>, "ossian")]))

"#,
    );
}

#[test_log::test]
fn no_reserved_keywords() {
    check(
        r"
if if = 3 {
}
         ",
        r"

<1:23>If(<4:7>VariableAssignment(<4:2>, MutableOrImmutableExpression { is_mutable: None, expression: <9:1>Literal(Int) }), <11:3>Block([]), None)

",
    );
}

#[test_log::test]
fn no_reserved_keywords_when() {
    check(
        r"
when when {
}

         ",
        r"

<1:24>When([WhenBinding { variable: <6:4>, expression: None }], <11:3>Block([]), None)

",
    );
}

#[test_log::test]
fn no_reserved_keywords_while() {
    check(
        "
while while {
    if = 3
}

         ",
        "
<1:26>WhileLoop(<7:6>IdentifierReference(QualifiedIdentifier { name: <7:5>, module_path: None, generic_params: [] }), <13:14>Block([<19:7>VariableAssignment(<19:2>, MutableOrImmutableExpression { is_mutable: None, expression: <24:1>Literal(Int) })]))
",
    );
}

#[test_log::test]
fn anonymous_struct() {
    check(
        "
struct SomeStruct {
   anon: { x: Int, y: Int },
   x: Int,
}
         ",
        "

NamedStructDef(NamedStructDef { identifier: LocalTypeIdentifierWithOptionalTypeVariables { name: <8:10>, type_variables: [] }, struct_type: AnonymousStructType { fields: [StructTypeField { field_name: FieldName(<24:4>), field_type: AnonymousStruct(AnonymousStructType { fields: [StructTypeField { field_name: FieldName(<32:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<35:3>), module_path: None, generic_params: [] }) }, StructTypeField { field_name: FieldName(<40:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<43:3>), module_path: None, generic_params: [] }) }] }) }, StructTypeField { field_name: FieldName(<53:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<56:3>), module_path: None, generic_params: [] }) }] } })


",
    );
}

#[test_log::test]
fn test_enum_variant_anonymous_struct() {
    check(
        "
enum Something {
   First,
   Another { x: Int, y: Float },
}
         ",
        "

EnumDef(LocalTypeIdentifierWithOptionalTypeVariables { name: <6:9>, type_variables: [] }, [Simple(<21:5>), Struct(<31:7>, AnonymousStructType { fields: [StructTypeField { field_name: FieldName(<41:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<44:3>), module_path: None, generic_params: [] }) }, StructTypeField { field_name: FieldName(<49:1>), field_type: Named(QualifiedTypeIdentifier { name: LocalTypeIdentifier(<52:5>), module_path: None, generic_params: [] }) }] })])


",
    );
}

#[test_log::test]
fn lambda_no_parameters() {
    check(
        "
| | {
    // This is a block
}
         ",
        "

<1:30>Lambda([], <5:26>Block([]))

",
    );
}

#[test_log::test]
fn lambda_one_param() {
    check(
        "
|
a
| {
    // This is a block
}
         ",
        "
<1:32>Lambda([<3:1>], <7:26>Block([]))
",
    );
}

#[test_log::test]
fn lambda_two_params() {
    check(
        "
|
    a,
    b,
| {
    // This is a block
}
         ",
        "
<1:44>Lambda([<7:1>, <14:1>], <19:26>Block([]))
",
    );
}

#[test_log::test]
fn lambda_two_params_expr() {
    check(
        "
|
    a,
    b,
| a + b
         ",
        "
<1:33>Lambda([<7:1>, <14:1>], <19:15>BinaryOp(<19:2>IdentifierReference(QualifiedIdentifier { name: <19:1>, module_path: None, generic_params: [] }), BinaryOperator { kind: Add, node: <21:1> }, <23:11>IdentifierReference(QualifiedIdentifier { name: <23:1>, module_path: None, generic_params: [] })))

",
    );
}

#[test_log::test]
fn lambda_no_params_expr() {
    check(
        "
|| 23
         ",
        "
<1:15>Lambda([], <4:2>Literal(Int))
",
    );
}
