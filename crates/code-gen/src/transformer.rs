use std::rc::Rc;
use swamp_vm_types::prelude::{BasicType, BasicTypeId, BasicTypeKind, BasicTypeRef};
use swamp_vm_types::{
    MAP_ITERATOR_ALIGNMENT, MAP_ITERATOR_SIZE, MemoryAlignment, MemorySize,
    RANGE_ITERATOR_ALIGNMENT, RANGE_ITERATOR_SIZE, SPARSE_ITERATOR_ALIGNMENT, SPARSE_ITERATOR_SIZE,
    STRING_ITERATOR_ALIGNMENT, STRING_ITERATOR_SIZE, VEC_ITERATOR_ALIGNMENT, VEC_ITERATOR_SIZE,
};

#[derive(Copy, Clone, Debug)]
pub enum Transformer {
    For,
    Filter,
    Find,
    Map,
    Any,
    All,
    FilterMap,
    While,
}

pub enum TransformerLambdaResultConversion {
    NoConversion,
    SkipOnFalse,
    SkipOnNone,
}

#[derive(Debug)]
pub enum TransformerResult {
    Unit,
    Bool,
    VecWithLambdaResult,
    VecFromSourceCollection,
    WrappedValueFromSourceCollection,
}

impl Transformer {
    pub(crate) const fn return_type(self) -> TransformerResult {
        match self {
            Self::Filter => TransformerResult::VecFromSourceCollection,
            Self::FilterMap | Self::Map => TransformerResult::VecWithLambdaResult,
            Self::All | Self::Any => TransformerResult::Bool,
            Self::Find => TransformerResult::WrappedValueFromSourceCollection,
            Self::For => TransformerResult::Unit,
            Self::While => TransformerResult::Unit,
        }
    }

    #[must_use]
    pub const fn lambda_result_conversion(self) -> TransformerLambdaResultConversion {
        match self {
            Self::Filter => TransformerLambdaResultConversion::SkipOnFalse,
            Self::Find => TransformerLambdaResultConversion::SkipOnNone,
            _ => TransformerLambdaResultConversion::NoConversion,
        }
    }

    pub(crate) const fn needs_tag_removed(self) -> bool {
        matches!(self, Self::FilterMap)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Collection {
    Vec,
    Map,
    Grid,
    String,
    Range,
    Sparse,
}

impl Collection {
    #[must_use]
    pub fn iterator_size_and_alignment(&self) -> (MemorySize, MemoryAlignment) {
        match self {
            Self::Vec => (VEC_ITERATOR_SIZE, VEC_ITERATOR_ALIGNMENT),
            Self::Map => (MAP_ITERATOR_SIZE, MAP_ITERATOR_ALIGNMENT),
            Self::Grid => todo!(),
            Self::String => (STRING_ITERATOR_SIZE, STRING_ITERATOR_ALIGNMENT),
            Self::Sparse => (SPARSE_ITERATOR_SIZE, SPARSE_ITERATOR_ALIGNMENT),
            Self::Range => (RANGE_ITERATOR_SIZE, RANGE_ITERATOR_ALIGNMENT),
        }
    }

    #[must_use]
    pub fn iterator_gen_type(&self) -> BasicTypeRef {
        let kind = match self {
            Self::Vec => BasicTypeKind::InternalVecIterator,
            Self::Map => BasicTypeKind::InternalMapIterator,
            //Self::Grid => BasicTypeKind::InternalGridIterator,
            Self::String => BasicTypeKind::InternalStringIterator,
            Self::Range => BasicTypeKind::InternalRangeIterator,
            Self::Sparse => BasicTypeKind::InternalSparseIterator,
            _ => todo!(),
        };
        let (size, alignment) = self.iterator_size_and_alignment();
        // HACK: Should use cache
        Rc::new(BasicType {
            id: BasicTypeId::EMPTY, // HACK
            kind,
            total_size: size,
            max_alignment: alignment,
        })
    }
}
