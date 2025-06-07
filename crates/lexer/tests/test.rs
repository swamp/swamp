use lexer::{Lexer, TokenKind};

#[test]
fn string_literal() {
    let mut lexer = Lexer::new("   \"hello\" ");

    assert_eq!(get_string(&mut lexer), "\"hello\"");
}

#[test]
fn identifier() {
    let mut lexer = Lexer::new("   xx_y=3 ");

    assert_eq!(get_identifier(&mut lexer), "xx_y");
}

fn get_identifier(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if let TokenKind::Identifier = tok.kind {
        lexer.get_string(&tok)
    } else {
        panic!("not a string")
    }
}

fn get_string(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringLiteral {
        lexer.get_string(&tok)
    } else {
        panic!("not a string")
    }
}

fn get_string_start(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringStart {
        lexer.get_string(&tok)
    } else {
        panic!("not a string start {tok:?}")
    }
}

fn get_string_middle(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringMiddle {
        lexer.get_string(&tok)
    } else {
        panic!("not a string")
    }
}

fn get_string_end(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if let TokenKind::StringEnd = tok.kind {
        lexer.get_string(&tok)
    } else {
        panic!("not a string")
    }
}

#[test]
fn identifier_underscores() {
    let mut lexer = Lexer::new("   __xx_32=42 ");
    assert_eq!(get_identifier(&mut lexer), "__xx_32");
    assert_eq!(TokenKind::Integer(42), lexer.peek_n(1).kind);
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

    assert_eq!(get_identifier(&mut lexer), "xx_y");

    assert_eq!(TokenKind::FatArrow, lexer.next_token().kind);
    assert_eq!(TokenKind::Integer(3), lexer.peek().kind);
}

#[test]
fn minus() {
    let mut lexer = Lexer::new("   xx_y = -y ");
    assert_eq!(get_identifier(&mut lexer), "xx_y");
    assert_eq!(TokenKind::Equal, lexer.next_token().kind);
    assert_eq!(TokenKind::Minus, lexer.next_token().kind);
    assert_eq!(get_identifier(&mut lexer), "y");
}

#[test]
fn string_interpolation() {
    let mut lexer = Lexer::new("'hello {x+1} world '");

    assert_eq!(get_string_start(&mut lexer), "hello ",);
    assert_eq!(TokenKind::LBrace, lexer.next_token().kind);
    assert_eq!(get_identifier(&mut lexer), "x");
    assert_eq!(TokenKind::Plus, lexer.next_token().kind);
    assert_eq!(TokenKind::Integer(1), lexer.next_token().kind);
    assert_eq!(TokenKind::RBrace, lexer.next_token().kind);
    assert_eq!(get_string_end(&mut lexer), " world ",);
    assert_eq!(TokenKind::EOF, lexer.next_token().kind);
}
