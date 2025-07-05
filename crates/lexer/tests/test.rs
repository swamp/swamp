/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

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
    if tok.kind == TokenKind::Identifier {
        lexer.get_string(&tok.node)
    } else {
        panic!("not a string")
    }
}

fn get_string(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringLiteral {
        lexer.get_string(&tok.node)
    } else {
        panic!("not a string {tok:?}")
    }
}

fn get_string_start(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringStart {
        lexer.get_string(&tok.node)
    } else {
        panic!("not a string start {tok:?}")
    }
}

fn get_doc_comment(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::DocComment {
        lexer.get_string(&tok.node)
    } else {
        panic!("get_doc_comment {tok:?}")
    }
}

fn get_block_comment(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::BlockComment {
        lexer.get_string(&tok.node)
    } else {
        panic!("get_block_comment {tok:?}")
    }
}

fn get_line_comment(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::LineComment {
        lexer.get_string(&tok.node)
    } else {
        panic!("get_block_comment {tok:?}")
    }
}

fn get_string_middle(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringMiddle {
        lexer.get_string(&tok.node)
    } else {
        panic!("not a get_string_middle {tok:?}")
    }
}

fn get_string_end(lexer: &mut Lexer) -> String {
    let tok = lexer.next_token();
    if tok.kind == TokenKind::StringEnd {
        lexer.get_string(&tok.node)
    } else {
        panic!("not a string end {tok:?}")
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

#[test]
fn test_line_comment() {
    let mut lexer = Lexer::new("  // This is a comment\nx = 1");

    assert_eq!(get_line_comment(&mut lexer), "// This is a comment");
    assert_eq!(get_identifier(&mut lexer), "x");
}

#[test]
fn test_doc_comment() {
    let mut lexer = Lexer::new("    /// Documentation\nfn test() {}");

    assert_eq!(get_doc_comment(&mut lexer), "/// Documentation");
    assert_eq!(get_identifier(&mut lexer), "fn");
}

#[test]
fn test_block_comment() {
    let mut lexer = Lexer::new(" /* Block\ncomment\n*/\nx = 1");

    assert_eq!(get_block_comment(&mut lexer), "/* Block\ncomment\n*/");
    assert_eq!(get_identifier(&mut lexer), "x");
}

#[test]
fn test_unterminated_block_comment() {
    let mut lexer = Lexer::new("/* Unterminated");

    if lexer.next_token().kind == TokenKind::Unknown('*') {
        // Test passed
    } else {
        panic!("Expected unknown token for unterminated block comment");
    }
}

#[test]
fn all_tokens() {
    let script = r"
    use std::{print}
    
    const SOME_CONSTANT: Int = 3
    
    struct Battle {
    seed: Int,
    id_gen: IDSource,
    mod_data: ModifierData,
    teams: [Alliance: Team; 2],
    tick_counter: Int,
    turn_counter: Int,
    sudden_death_check: Bool,
    sudden_death_counter: Int,
    game_end: Bool,
    turn_phase: Phase,
    turn_alliance: Alliance,
    arena: Arena,
    objective: Objective,
    units_spawned: [Int: Int; 100],
    unit_turn_order: Vec<Int; 100>,
    unit_current: Int?,
    stack: Vec<String; 100>,
    next_stack: Vec<String; 100>,
    triggers: [Int: Trigger; 100],
    tick_log: Vec<String; 100>
}

impl Battle {
    fn fake_new() -> Battle {
         Battle {
            seed: 0,
            id_gen: IDSource::new(),
            mod_data: ModifierData{modifiers: []},
            teams: [:],
            tick_counter: 0,
            turn_counter: 0,
            sudden_death_check: false,
            sudden_death_counter: 0,
            game_end: false,
            turn_phase: Phase::Start,
            turn_alliance: Alliance::West, // west always starts
            arena: Arena::new(),
            objective: Objective {     id: 0,
                position: Point { x: 0, y: 0 },
                holder_id: none,
                holder_alliance: none,
                alignment: 0 } ,
            units_spawned: [:],
            unit_turn_order: [],
            unit_current: none,
            stack: [],
            next_stack: [],
            triggers: [:],
            tick_log: []
        }
    }


    fn new() -> Battle {
        player_order = [0,1]
        team_owners = [Owner::Player, Owner::Opponent]
        mut id_gen = IDSource::new()

        mut objective = Objective::new(id_gen.get_id(), OBJECTIVE_SPAWN_POINT)

        mut default_triggers: [Int:Trigger;42] = [:]  // allocation ?
        for t in DEFAULT_TRIGGERS {
            id = id_gen.get_id()
            default_triggers[id] = t
        }


        decks = [
            Alliance::West: [1:'Hej'],
            Alliance::East: [1:'Hej']
        ]

        teams = [
            Alliance::West: Team::new(Alliance::West, decks[Alliance::West], 1, team_owners[player_order[0]], 0, &id_gen),
            Alliance::East: Team::new(Alliance::East, decks[Alliance::East], 2, team_owners[player_order[1]], 1, &id_gen)
        ]

        empty_unit_id_list: Vec<Int; 25> = []

        Battle{
            seed: 0,
            id_gen: id_gen,
            mod_data: ModifierData{modifiers: []},
            teams: teams,
            tick_counter: 0,
            turn_counter: 0,
            sudden_death_check: false,
            sudden_death_counter: 0,
            game_end: false,
            turn_phase: Phase::Start,
            turn_alliance: Alliance::West, // west always starts
            arena: Arena::new(),
            objective: objective,
            units_spawned: [:],
            unit_turn_order: [],
            unit_current: none,
            stack: [],
            next_stack: [],
            triggers: default_triggers,
            tick_log: []
        }
    }

}


#[test]
fn test() {
    battle = Battle::fake_new()
    print('{battle.arena.width}')
}

    ";

    let mut lexer = Lexer::new(script);

    for _ in 0..200 {
        let tok = lexer.next_token();
        if let TokenKind::Unknown(c) = tok.kind {
            panic!("unknown {c}");
        }
        if tok.kind == TokenKind::EOF {
            break;
        }

        eprintln!("tok: {tok:?}");
    }
}
