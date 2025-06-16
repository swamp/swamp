use crate::supporting_types::{AnonymousStructType, EnumType, NamedStructType, Signature};
use crate::Type;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum TypeKind {
    // Primitives
    Int,
    Float,
    String,
    Bool,
    Unit, // Empty or nothing

    // Aggregate type, Containers
    Tuple(Vec<Rc<Type>>),
    NamedStruct(NamedStructType),
    AnonymousStruct(AnonymousStructType),
    Range(AnonymousStructType),

    Enum(EnumType),

    Function(Signature),

    Optional(Rc<Type>),

    MutableReference(Rc<Type>),

    // Fixed capacity `[T; N]
    FixedCapacityAndLengthArray(Rc<Type>, usize),

    // View `[T]`
    SliceView(Rc<Type>),

    // Collections
    VecStorage(Rc<Type>, usize),
    DynamicLengthVecView(Rc<Type>),

    StackStorage(Rc<Type>, usize),
    StackView(Rc<Type>),

    QueueStorage(Rc<Type>, usize),
    QueueView(Rc<Type>),

    MapStorage(Rc<Type>, Rc<Type>, usize),
    DynamicLengthMapView(Rc<Type>, Rc<Type>),

    SparseView(Rc<Type>),
    SparseStorage(Rc<Type>, usize),

    GridStorage(Rc<Type>, usize, usize),
    GridView(Rc<Type>),
}
