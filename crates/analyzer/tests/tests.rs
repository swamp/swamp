/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use source_map_cache::SourceMap;
use source_map_node::FileId;
use swamp_analyzer::Analyzer;
use swamp_modules::modules::Modules;
use swamp_modules::symtbl::{SymbolTable, SymbolTableRef};
use swamp_semantic::ProgramState;

#[test]
fn test_if_compiles() {
    let mut program_state = ProgramState::default();
    let modules = Modules::default();
    let core_symbol_table = SymbolTable::new(&[]);
    let source_map = SourceMap::new(&SeqMap::new()).unwrap();

    let test_module_path: &[String] = &[];
    let file_id: FileId = 23;
    let analyzer = Analyzer::new(
        &mut program_state,
        &modules,
        SymbolTableRef::from(core_symbol_table),
        &source_map,
        test_module_path,
        file_id,
    );
}
