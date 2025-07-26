pub mod prelude;

use seq_map::SeqMap;
use source_map_node::Node;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymbolKind {
    // Top-level
    Function,
    Type,
    Struct,
    Enum,
    Const,
    Module,

    // Scoped
    Variable,
    Parameter,
    PatternBinding,

    // Inner components
    EnumVariant,
    Field,
    Alias,
    NamedStruct,
    MemberFunction,
    EnumPayloadStructField,
    AnonStructField,
    NamedStructField,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
    pub fn inner(&self) -> u32 {
        self.0
    }
}

impl SymbolId {
    pub fn new_illegal() -> SymbolId {
        SymbolId(0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TopLevelSymbolId(pub SymbolId);

impl TopLevelSymbolId {
    pub fn new_illegal() -> Self {
        Self(SymbolId::new_illegal())
    }
}

impl From<TopLevelSymbolId> for SymbolId {
    fn from(id: TopLevelSymbolId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopedSymbolId(pub SymbolId);


impl ScopedSymbolId {
    pub fn new_illegal() -> ScopedSymbolId {
        ScopedSymbolId(SymbolId::new_illegal())
    }
}

impl From<ScopedSymbolId> for SymbolId {
    fn from(id: ScopedSymbolId) -> Self {
        id.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub id: SymbolId,
    pub kind: SymbolKind,
    pub source_map_node: Node,
    pub name: Node,
}


#[derive(Clone, Debug)]
pub struct SymbolIdAllocator {
    next: u32,
}

impl SymbolIdAllocator {
    pub fn new() -> Self {
        Self { next: 0 }
    }

    pub fn alloc(&mut self) -> SymbolId {
        self.next += 1;
        let id = SymbolId(self.next);
        id
    }

    pub fn alloc_top_level(&mut self) -> TopLevelSymbolId {
        TopLevelSymbolId(self.alloc())
    }

    pub fn alloc_scoped(&mut self) -> ScopedSymbolId {
        ScopedSymbolId(self.alloc())
    }
}


#[derive(Clone, Debug)]
pub struct Symbols {
    pub top_level_symbols: SeqMap<TopLevelSymbolId, Symbol>,
    pub scoped_symbols: SeqMap<ScopedSymbolId, Symbol>,
}

impl Symbols {
    pub fn new() -> Self {
        Self {
            top_level_symbols: SeqMap::new(),
            scoped_symbols: SeqMap::new(),
        }
    }
    pub fn insert_top(&mut self, symbol_id: TopLevelSymbolId, symbol: Symbol) {
        self.top_level_symbols.insert(symbol_id, symbol).unwrap();
    }

    pub fn insert_scoped(&mut self, symbol_id: ScopedSymbolId, symbol: Symbol) {
        self.scoped_symbols.insert(symbol_id, symbol).unwrap();
    }

    pub fn iter_all(&self) -> impl Iterator<Item=&Symbol> {
        self.top_level_symbols.values().chain(self.scoped_symbols.values())
    }

    pub fn get(&self, symbol_id: SymbolId) -> Option<&Symbol> {
        if let Some(sym) = self.top_level_symbols.get(&TopLevelSymbolId(symbol_id)) {
            return Some(sym)
        }
        if let Some(sym) = self.scoped_symbols.get(&ScopedSymbolId(symbol_id)) {
            return Some(sym)
        }
        None
    }
}
