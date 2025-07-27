/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use source_map_cache::{FileId, SourceMap};
use swamp_modules::prelude::ModuleRef;
use swamp_modules::{
    prelude::Modules,
    symtbl::{DefinitionTable, SymbolTableRef},
};
use swamp_semantic::ProgramState;

pub struct SharedState<'a> {
    pub state: &'a mut ProgramState,
    pub lookup_table: DefinitionTable,
    pub definition_table: DefinitionTable,
    pub modules: &'a Modules,
    pub source_map: &'a SourceMap,
    pub file_id: FileId,
    pub core_symbol_table: SymbolTableRef,
}

impl<'a> SharedState<'a> {
    #[must_use]
    pub fn get_definition_table(&'a self, path: &[String]) -> Option<&'a DefinitionTable> {
        if path.is_empty() {
            return Some(&self.lookup_table);
        }
        self.get_module(path).map(|module| &module.definition_table)
    }

    #[must_use]
    pub fn get_module(&'a self, path: &[String]) -> Option<&'a ModuleRef> {
        let resolved_path = path.to_vec();

        if path.len() == 1
            && let Some(module_ref) = self.lookup_table.get_module_link(&path[0])
        {
            return Some(module_ref);
        }

        if let Some(x) = self.modules.get(&resolved_path) {
            return Some(x);
        }

        None
    }
}
