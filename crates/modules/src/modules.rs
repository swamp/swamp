/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::symtbl::DefinitionTable;
use seq_map::SeqMap;
use source_map_node::FileId;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_semantic::prelude::Error;
use swamp_semantic::{Expression, ExpressionKind, InternalMainExpression};

#[derive(Debug)]
pub struct Modules {
    modules: SeqMap<Vec<String>, ModuleRef>,
    file_id_to_module: SeqMap<FileId, ModuleRef>,
}

impl Default for Modules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Module {
    pub main_expression: Option<InternalMainExpression>,
    pub definition_table: DefinitionTable,
    pub file_id: FileId,
    pub errors: Vec<Error>,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " {:?}", self.definition_table)?;

        if let Some(internal_main) = &self.main_expression {
            pretty_print(f, &internal_main.expression, 0)?;
        }

        Ok(())
    }
}

/// # Errors
///
pub fn pretty_print(
    f: &mut Formatter<'_>,
    resolved_expression: &Expression,
    tabs: usize,
) -> std::fmt::Result {
    if let ExpressionKind::Block(expressions) = &resolved_expression.kind {
        for internal_expr in expressions {
            pretty_print(f, internal_expr, tabs + 1)?;
        }
        Ok(())
    } else {
        let tab_str = "..".repeat(tabs);
        writeln!(
            f,
            "{}{},{:?}",
            tab_str, resolved_expression.ty, resolved_expression.kind
        )
    }
}

pub type ModuleRef = Rc<Module>;

impl Module {
    #[must_use]
    pub const fn new(
        symbol_table: DefinitionTable,
        errors: Vec<Error>,
        expression: Option<InternalMainExpression>,
        file_id: FileId,
    ) -> Self {
        Self {
            definition_table: symbol_table,
            file_id,
            main_expression: expression,
            errors,
        }
    }
}

impl Modules {
    #[must_use]
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
            file_id_to_module: SeqMap::new(),
        }
    }

    #[must_use]
    pub const fn modules(&self) -> &SeqMap<Vec<String>, ModuleRef> {
        &self.modules
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(&module_path.to_vec())
    }

    pub fn add(&mut self, module: ModuleRef) {
        let path = module.definition_table.module_path();

        self.modules.insert(path, module.clone()).expect("could not insert");
        self.file_id_to_module.insert(module.file_id, module.clone());
    }

    pub fn link_module(&mut self, module_path: &[String], referred_module: ModuleRef) {
        self.modules
            .insert(module_path.to_vec(), referred_module.clone())
            .expect("could not insert");
        self.file_id_to_module.insert(referred_module.file_id, referred_module).expect("could not insert");
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<&ModuleRef> {
        self.modules.get(&module_path.to_vec())
    }

    #[must_use]
    pub fn get_from_file_id(&self, file_id: FileId) -> Option<&ModuleRef> {
        self.file_id_to_module.get(&file_id)
    }
}
