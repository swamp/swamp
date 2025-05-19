/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::symtbl::SymbolTable;
use seq_map::SeqMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_semantic::{Expression, ExpressionKind, InternalMainExpression};

#[derive(Debug)]
pub struct Modules {
    modules: SeqMap<Vec<String>, ModuleRef>,
}

impl Default for Modules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Module {
    pub main_expression: Option<InternalMainExpression>,
    pub symbol_table: SymbolTable,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " {:?}", self.symbol_table)?;

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
    pub fn new(symbol_table: SymbolTable, expression: Option<InternalMainExpression>) -> Self {
        Self {
            symbol_table,
            main_expression: expression,
        }
    }
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
        }
    }

    pub fn modules(&self) -> &SeqMap<Vec<String>, ModuleRef> {
        &self.modules
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(&module_path.to_vec())
    }

    pub fn add(&mut self, module: ModuleRef) {
        let path = module.symbol_table.module_path().clone();

        self.modules.insert(path, module).expect("could not insert");
    }

    pub fn link_module(&mut self, module_path: &[String], referred_module: ModuleRef) {
        self.modules
            .insert(module_path.to_vec(), referred_module)
            .expect("todo");
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<&ModuleRef> {
        self.modules.get(&module_path.to_vec())
    }
}
