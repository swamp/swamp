use source_map_node::Node;

#[derive(Clone, Debug)]
pub struct Error {
    pub node: Node,
    pub kind: ErrorKind,
}
#[derive(Clone, Debug)]
pub enum ErrorKind {
    CanNotCreateTempArgument,
}
