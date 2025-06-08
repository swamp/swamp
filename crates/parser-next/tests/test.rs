use lexer::Lexer;
use swamp_ast::ExpressionKind;
use swamp_parser_next::Parser;

#[must_use]
pub fn lex(str: &str) -> (Lexer, Parser) {
    let lex = Lexer::new(str);
    let parser = Parser::new();
    (lex, parser)
}

#[test]
fn anon_struct() {
    let (mut lexer, mut parser) = lex(r#"
        {

           field_name: 23, another: "hello",
              another: CONSTANT_SOMETHING_3,..
           }
    "#);

    let expression = parser.parse_struct_literal(&mut lexer);
    let ExpressionKind::AnonymousStructLiteral(fields, rest) = expression.kind else {
        panic!("must be anonymous")
    };

    eprintln!("expr: {fields:?} rest:{rest} errors: {:?}", parser.errors);

    assert!(parser.errors.is_empty());
    assert!(rest);
    assert_eq!(fields.len(), 3);
}
