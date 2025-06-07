use lexer::{Lexer, TokenKind};

#[test]
fn string_literal() {
    let mut lexer = Lexer::new("   \"hello\" ");

    if let TokenKind::StringLiteral(string) = lexer.next_token().kind {
        assert_eq!(string, "\"hello\"");
    } else {
        panic!("must be string literal")
    }
}

#[test]
fn identifier() {
    let mut lexer = Lexer::new("   xx_y=3 ");

    if let TokenKind::Identifier(ident) = lexer.next_token().kind {
        assert_eq!(ident, "xx_y");
    } else {
        panic!("must be identifer literal")
    }
}

#[test]
fn identifier_underscores() {
    let mut lexer = Lexer::new("   __xx_32=3 ");

    if let TokenKind::Identifier(ident) = lexer.next_token().kind {
        assert_eq!(ident, "__xx_32");
    } else {
        panic!("must be identifier literal")
    }
}

#[test]
fn integer() {
    let mut lexer = Lexer::new("   \n-3_98\n");

    if let TokenKind::Integer(value) = lexer.next_token().kind {
        assert_eq!(value, -398);
    } else {
        panic!("must be integer literal")
    }
}

#[test]
fn fixed() {
    let mut lexer = Lexer::new("   \n  -342.99 ");

    if let TokenKind::Fixed(value) = lexer.next_token().kind {
        assert_eq!(value, -22_478_184);
    } else {
        panic!("must be fixed literal")
    }
}

#[test]
fn arrow() {
    let mut lexer = Lexer::new("   xx_y=>3 ");

    if let TokenKind::Identifier(ident) = lexer.next_token().kind {
        assert_eq!(ident, "xx_y");
        assert_eq!(TokenKind::EqualGreater, lexer.next_token().kind);
    } else {
        panic!("must be identifier literal")
    }
}
