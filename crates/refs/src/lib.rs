pub mod prelude;

use seq_map::SeqMap;
use source_map_node::Node;
use swamp_symbol::SymbolId;

#[derive(Debug, Clone)]
pub struct ReferenceTracker {
    map: SeqMap<SymbolId, Vec<Node>>,
}

impl Default for ReferenceTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl ReferenceTracker {
    #[must_use]
    pub fn new() -> Self {
        Self { map: SeqMap::new() }
    }

    pub fn add(&mut self, symbol: SymbolId, usage_site: Node) {
        if let Some(vec) = self.map.get_mut(&symbol) {
            vec.push(usage_site);
        } else {
            self.map.insert(symbol, vec![usage_site]).unwrap();
        }
    }

    #[must_use]
    pub fn get(&self, symbol: SymbolId) -> Option<&[Node]> {
        self.map.get(&symbol).map(std::vec::Vec::as_slice)
    }

    #[must_use]
    pub fn is_used(&self, symbol: &SymbolId) -> bool {
        self.map.get(symbol).is_some_and(|v| !v.is_empty())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&SymbolId, &[Node])> {
        self.map.iter().map(|(id, nodes)| (id, nodes.as_slice()))
    }
}

#[derive(Clone, Debug)]
pub struct SymbolReference {
    pub usage_node: Node,
    pub pointing_to_symbol_id: SymbolId,
}

#[derive(Clone, Debug)]
pub struct ModuleSymbolReferences {
    pub refs: Vec<SymbolReference>,
}

impl Default for ModuleSymbolReferences {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleSymbolReferences {
    #[must_use]
    pub const fn new() -> Self {
        Self { refs: Vec::new() }
    }
    pub fn add(&mut self, symbol_id: SymbolId, usage_site: Node) {
        self.refs.push(SymbolReference {
            pointing_to_symbol_id: symbol_id,
            usage_node: usage_site,
        });
    }
}
