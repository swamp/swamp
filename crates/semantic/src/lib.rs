/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod err;
pub mod intr;
pub mod prelude;
use crate::err::{Error, ErrorKind};
use crate::intr::IntrinsicFunction;
use crate::prelude::IntrinsicFunctionDefinitionRef;
pub use fixed32::Fp;
use seq_map::SeqMap;
use source_map_node::Node;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use swamp_attributes::Attributes;
use swamp_refs::ReferenceTracker;
use swamp_symbol::{ScopedSymbolId, SymbolIdAllocator, Symbols, TopLevelSymbolId};
use swamp_types::prelude::*;
use swamp_types::{Type, TypeRef};
use tracing::error;

#[derive(Debug, Clone)]
pub struct TypeWithMut {
    pub resolved_type: TypeRef,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub enum SemanticError {
    CouldNotInsertStruct,
    DuplicateTypeAlias(String),
    CanOnlyUseStructForMemberFunctions,
    ResolveNotStruct,
    DuplicateStructName(String),
    DuplicateEnumType(String),
    DuplicateEnumVariantType(String, String),
    DuplicateFieldName(String),
    DuplicateExternalFunction(String),
    DuplicateRustType(String),
    DuplicateConstName(String),
    CircularConstantDependency(Vec<ConstantId>),
    DuplicateConstantId(ConstantId),
    IncompatibleTypes,
    WasNotImmutable,
    WasNotMutable,
    DuplicateSymbolName(String),
    DuplicateNamespaceLink(String),
    MismatchedTypes {
        expected: TypeRef,
        found: Vec<TypeRef>,
    },
    UnknownImplOnType,
    UnknownTypeVariable,
    DuplicateDefinition(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct LocalIdentifier(pub Node);

#[derive(Debug)]
pub struct InternalMainExpression {
    pub expression: Expression,
    pub scopes: VariableScopes,
    pub program_unique_id: InternalFunctionId,
}

pub fn formal_function_name(internal_fn_def: &InternalFunctionDefinition) -> String {
    let prefix = internal_fn_def
        .associated_with_type
        .as_ref()
        .map_or_else(String::new, |associated_with_type| {
            format!("{associated_with_type}::")
        });
    format!(
        "{}::{}{}",
        formal_module_name(&internal_fn_def.defined_in_module_path),
        prefix,
        internal_fn_def.assigned_name
    )
}

//#[derive(Debug,Clone)]
pub struct InternalFunctionDefinition {
    pub symbol_id: TopLevelSymbolId,
    pub body: Expression,
    pub name: LocalIdentifier,
    pub assigned_name: String,
    pub associated_with_type: Option<TypeRef>,
    pub defined_in_module_path: Vec<String>,
    pub signature: Signature,
    pub function_variables: VariableScopes,
    pub program_unique_id: InternalFunctionId,
    pub attributes: Attributes,
}

impl Default for InternalFunctionDefinition {
    fn default() -> Self {
        Self {
            body: Expression {
                ty: Rc::new(Type {
                    id: TypeId::new(0),
                    flags: TypeFlags::default(),
                    kind: Rc::new(TypeKind::Byte),
                }),
                node: Node::default(),
                kind: ExpressionKind::NoneLiteral,
            },
            name: LocalIdentifier(Node::default()),
            assigned_name: String::new(),
            associated_with_type: None,
            defined_in_module_path: vec![],
            signature: Signature {
                parameters: vec![],
                return_type: Rc::new(Type {
                    id: TypeId::new(0),
                    flags: TypeFlags::default(),
                    kind: Rc::new(TypeKind::Byte),
                }),
            },
            function_variables: VariableScopes::default(),
            symbol_id: TopLevelSymbolId::new_illegal(),
            program_unique_id: 0,
            attributes: Attributes::default(),
        }
    }
}

impl InternalFunctionDefinition {}

impl Debug for InternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}\n{:?}", self.signature, self.body)
    }
}

impl PartialEq<Self> for InternalFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for InternalFunctionDefinition {}

pub type InternalFunctionDefinitionRef = Rc<InternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

pub type InternalFunctionId = u16;

pub type ConstantId = u32;

#[must_use]
pub fn pretty_module_name(parts: &[String]) -> String {
    if parts[0] == "crate" {
        parts[1..].join("::")
    } else {
        parts.join("::")
    }
}

#[must_use]
pub fn formal_module_name(parts: &[String]) -> String {
    parts.join("::")
}

#[derive(Eq, PartialEq)]
pub struct ExternalFunctionDefinition {
    pub name: Node,
    pub assigned_name: String,
    pub signature: Signature,
    pub id: ExternalFunctionId,
}

impl Debug for ExternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "external fn")
    }
}

pub type ExternalFunctionDefinitionRef = Rc<crate::ExternalFunctionDefinition>;

#[derive(Debug, Eq, Clone, PartialEq)]
pub enum BlockScopeMode {
    Open,
    Closed,
    Lambda,
}

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub mode: BlockScopeMode,
    pub lookup: SeqMap<String, VariableRef>,
    pub variables: SeqMap<usize, VariableRef>,
    pub register_watermark: usize,
}

impl Display for BlockScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(f, "-- scope {:?}", self.mode)?;

        for (index, (name, var)) in self.variables.iter().enumerate() {
            writeln!(f, "  var({index}): {name}:{var:?}")?;
        }
        Ok(())
    }
}

impl Default for BlockScope {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockScope {
    #[must_use]
    pub fn new() -> Self {
        Self {
            mode: BlockScopeMode::Open,
            variables: SeqMap::new(),
            lookup: SeqMap::new(),
            register_watermark: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VariableScope {
    pub mode: BlockScopeMode,
    pub variables: SeqMap<usize, VariableRef>,
}

#[derive(Clone, Debug)]
pub struct VariableScopes {
    //pub block_scope_stack: Vec<VariableScope>,
    pub current_register: usize,
    pub highest_virtual_register: usize,
    pub all_variables: Vec<VariableRef>,
}

impl VariableScopes {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            //block_scope_stack: vec![],
            current_register: 0,
            all_variables: vec![],
            highest_virtual_register: 0,
        }
    }

    pub const fn finalize(&mut self) {
        self.highest_virtual_register = self.current_register;
    }
}
impl Default for VariableScopes {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
pub struct FunctionScopeState {
    pub block_scope_stack: Vec<BlockScope>,
    pub variable_index: usize,
}

#[derive(Clone, Debug)]
pub struct ScopeInfo {
    pub active_scope: FunctionScopeState,
    pub total_scopes: VariableScopes,
}

impl ScopeInfo {
    #[must_use]
    pub fn new() -> Self {
        Self {
            active_scope: FunctionScopeState::default(),
            total_scopes: VariableScopes::default(),
        }
    }
}

impl Default for ScopeInfo {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for FunctionScopeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        for (index, scope) in self.block_scope_stack.iter().enumerate() {
            writeln!(f, "block({index}):\n{scope}")?;
        }
        Ok(())
    }
}

impl FunctionScopeState {
    pub const fn emit_variable_index(&mut self) -> usize {
        self.variable_index += 1;

        self.variable_index
    }
}

impl Default for FunctionScopeState {
    fn default() -> Self {
        Self::new()
    }
}

impl FunctionScopeState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            block_scope_stack: vec![BlockScope::new()],
            //return_type,
            variable_index: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub enum VariableType {
    Local,
    Parameter,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub symbol_id: ScopedSymbolId,
    pub name: Node,
    pub assigned_name: String,
    pub resolved_type: TypeRef,
    pub mutable_node: Option<Node>,
    pub variable_type: VariableType,

    pub scope_index: usize,
    pub variable_index: usize,
    pub unique_id_within_function: usize,
    pub virtual_register: u8,

    pub is_unused: bool,
}

impl Variable {
    #[must_use]
    pub fn create_err(unit_type: TypeRef) -> Self {
        Self {
            symbol_id: ScopedSymbolId::new_illegal(),
            name: Node::default(),
            assigned_name: "err".to_string(),
            resolved_type: unit_type,
            mutable_node: None,
            variable_type: VariableType::Local,
            scope_index: 0,
            variable_index: 0,
            virtual_register: 0,
            unique_id_within_function: 0,
            is_unused: false,
        }
    }
}

impl Variable {
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.mutable_node.is_some()
    }

    #[must_use]
    pub const fn is_immutable(&self) -> bool {
        !self.is_mutable()
    }
}

pub type VariableRef = Rc<Variable>;

#[derive(Debug, Clone)]
pub struct MutVariable {
    pub variable_ref: VariableRef,
}

//type MutVariableRef = Rc<MutVariable>;

#[derive(Debug, Clone)]
pub enum BinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LogicalOr,
    LogicalAnd,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    NoneCoalesce,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub kind: BinaryOperatorKind,
    pub node: Node,
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    Not,
    Negate,
}
#[derive(Debug, Clone)]
pub struct UnaryOperator {
    pub left: Box<Expression>,
    pub kind: UnaryOperatorKind,
    pub node: Node,
}

#[derive()]
pub struct InternalFunctionCall {
    pub arguments: Vec<ArgumentExpression>,

    pub function_definition: InternalFunctionDefinitionRef,
    pub function_expression: Box<Expression>,
}

impl Debug for InternalFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "InFuncCall({:?} {:?})",
            self.function_expression, self.arguments
        )
    }
}

#[derive(Debug, Clone)]
pub struct ExternalFunctionCall {
    pub arguments: Vec<ArgumentExpression>,
    pub function_definition: ExternalFunctionDefinitionRef,
    pub function_expression: Box<Expression>,
}

pub fn comma_tuple_ref<K: Display, V: Display>(values: &[(&K, &V)]) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{key}: {value}").as_str());
    }
    result
}

#[derive(Debug, Clone)]
pub struct MemberCall {
    pub function: FunctionRef,
    pub arguments: Vec<ArgumentExpression>,
}

#[derive(Debug, Clone)]
pub struct ArrayItem {
    pub item_type: Rc<TypeRef>,
    pub int_expression: Expression,
    pub array_expression: Expression,
    pub array_type: Rc<TypeRef>,
}

pub type ArrayItemRef = Rc<ArrayItem>;

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float,
    String,
}

#[derive(Debug, Clone)]
pub enum FormatSpecifierKind {
    LowerHex,                            // :x
    UpperHex,                            // :X
    Binary,                              // :b
    Float,                               // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug, Clone)]
pub struct FormatSpecifier {
    pub node: Node,
    pub kind: FormatSpecifierKind,
}

pub type FunctionRef = Rc<Function>;

#[derive(Debug, Eq, Clone, PartialEq)]
pub enum Function {
    Internal(InternalFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
    Intrinsic(IntrinsicFunctionDefinitionRef),
}

impl Function {
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::Internal(x) => x.assigned_name.clone(),
            Self::External(y) => y.assigned_name.clone(),
            Self::Intrinsic(i) => i.name.clone(),
        }
    }

    #[must_use]
    pub fn symbol_id(&self) -> TopLevelSymbolId {
        match self {
            Self::Internal(x) => x.symbol_id.clone(),
            Self::External(y) => TopLevelSymbolId::new_illegal(),
            Self::Intrinsic(i) => TopLevelSymbolId::new_illegal(),
        }
    }

    #[must_use]
    pub fn maybe_node(&self) -> Option<&Node> {
        match self {
            Self::Internal(x) => Some(&x.name.0),
            Self::External(y) => Some(&y.name),
            Self::Intrinsic(_i) => None,
        }
    }

    #[must_use]
    pub fn node(&self) -> Node {
        match self {
            Self::Internal(x) => x.name.0.clone(),
            Self::External(_y) => Node::new_unknown(),
            Self::Intrinsic(_i) => Node::new_unknown(),
        }
    }

    #[must_use]
    pub fn signature(&self) -> &Signature {
        match self {
            Self::Internal(internal) => &internal.signature,
            Self::External(external) => &external.signature,
            Self::Intrinsic(i) => &i.signature,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    #[allow(unused)]
    pub expression: Box<Expression>,
}

// TODO: Maybe have different Match types, one specific for enums and one for other values
#[derive(Debug, Clone)]
pub struct Match {
    pub arms: Vec<MatchArm>,
    pub expression: Box<Expression>,
}

impl Match {
    #[must_use]
    pub fn contains_wildcard(&self) -> bool {
        for arm in &self.arms {
            if let Pattern::Wildcard(_) = arm.pattern {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    #[allow(unused)]
    pub pattern: Pattern,
    pub expression: Box<Expression>,
    pub expression_type: TypeRef,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Normal(NormalPattern, Option<BooleanExpression>),
    Wildcard(Node),
}

#[derive(Debug, Clone)]
pub enum NormalPattern {
    PatternList(Vec<PatternElement>),
    EnumPattern(EnumVariantType, Option<Vec<PatternElement>>),
    Literal(Expression),
}

#[derive(Debug, Clone)]
pub enum PatternElement {
    Variable(VariableRef),
    VariableWithFieldIndex(VariableRef, usize),
    Wildcard(Node),
}

#[derive(Debug, Clone)]
pub struct Iterable {
    pub key_type: Option<TypeRef>, // It does not have to support a key type
    pub value_type: TypeRef,

    pub resolved_expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AnonymousStructLiteral {
    pub source_order_expressions: Vec<(usize, Option<Node>, Expression)>,
    pub struct_like_type: TypeRef,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CompoundOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

#[derive(Debug, Clone)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[derive(Debug, Clone)]
pub struct VariableCompoundAssignment {
    pub variable_ref: VariableRef, // compound only support single variable
    pub expression: Box<Expression>,
    pub compound_operator: CompoundOperator,
}

#[derive(Debug, Clone)]
pub struct Guard {
    pub condition: Option<BooleanExpression>,
    pub result: Expression,
}

#[derive(Debug, Clone)]
pub struct Postfix {
    pub node: Node,
    pub ty: TypeRef,
    pub kind: PostfixKind,
}

#[derive(Debug, Clone)]
pub struct SliceViewType {
    pub element: TypeRef,
}

#[derive(Debug, Clone)]
pub struct VecType {
    pub element: TypeRef,
}

#[derive(Debug, Clone)]
pub struct GridType {
    pub element: TypeRef,
}

#[derive(Debug, Clone)]
pub struct SparseType {
    pub element: TypeRef,
}

#[derive(Debug, Clone)]
pub struct MapType {
    pub key: TypeRef,
    pub value: TypeRef,
}

#[derive(Debug, Clone)]
pub enum PostfixKind {
    StructField(TypeRef, usize),
    MemberCall(FunctionRef, Vec<ArgumentExpression>),
    OptionalChainingOperator, // ? operator
    // Subscripts
    VecSubscript(VecType, Expression),
    VecSubscriptRange(VecType, Expression),
    SparseSubscript(SparseType, Expression),
    MapSubscript(MapType, Expression),
    GridSubscript(GridType, Expression, Expression),
}

#[derive(Debug, Clone)]
pub enum LocationAccessKind {
    FieldIndex(AnonymousStructType, usize),
    SliceViewSubscript(SliceViewType, Expression),
    MapSubscriptCreateIfNeeded(MapType, Expression),
    MapSubscriptMustExist(MapType, Expression),
    SparseSubscript(SparseType, Expression),
    GridSubscript(GridType, Expression, Expression),
}

#[derive(Debug, Clone)]
pub struct LocationAccess {
    pub node: Node,
    pub ty: TypeRef,
    pub kind: LocationAccessKind,
}

#[derive(Debug, Clone)]
pub struct SingleLocationExpression {
    pub kind: MutableReferenceKind,
    pub node: Node,
    pub ty: TypeRef,

    pub starting_variable: VariableRef,
    pub access_chain: Vec<LocationAccess>,
}

#[derive(Debug, Clone)]
pub struct TargetAssignmentLocation(pub SingleLocationExpression);

#[derive(Debug, Clone)]
pub enum MutableReferenceKind {
    MutVariableRef,
    MutStructFieldRef(AnonymousStructType, usize),
}

#[derive(Debug, Clone)]
pub enum ArgumentExpression {
    Expression(Expression),
    BorrowMutableReference(SingleLocationExpression),
}

impl ArgumentExpression {
    #[must_use]
    pub fn ty(&self) -> TypeRef {
        match self {
            Self::Expression(expr) => expr.ty.clone(),
            Self::BorrowMutableReference(location) => location.ty.clone(),
        }
    }

    pub fn expect_immutable(self) -> Result<Expression, SemanticError> {
        match self {
            Self::Expression(expr) => Ok(expr),
            Self::BorrowMutableReference(_) => Err(SemanticError::WasNotImmutable),
        }
    }

    pub const fn expect_immutable_ref(&self) -> Result<&Expression, SemanticError> {
        match &self {
            Self::Expression(expr) => Ok(expr),
            Self::BorrowMutableReference(_) => Err(SemanticError::WasNotImmutable),
        }
    }

    #[must_use]
    pub const fn is_mutable_reference(&self) -> bool {
        matches!(self, Self::BorrowMutableReference(_))
    }

    #[must_use]
    pub const fn node(&self) -> &Node {
        match &self {
            Self::Expression(expr) => &expr.node,
            Self::BorrowMutableReference(loc) => &loc.node,
        }
    }
}

#[derive(Clone)]
pub struct Expression {
    pub ty: TypeRef,
    pub node: Node,
    pub kind: ExpressionKind,
}

impl Expression {
    #[must_use]
    pub fn debug_last_expression(&self) -> &Self {
        match &self.kind {
            ExpressionKind::ConstantAccess(a) => a.expr.debug_last_expression(),
            ExpressionKind::BinaryOp(binary) => binary.right.debug_last_expression(),
            ExpressionKind::UnaryOp(a) => a.left.debug_last_expression(),
            ExpressionKind::ForLoop(_, _, a) => a.debug_last_expression(),
            ExpressionKind::WhileLoop(_, a) => a.debug_last_expression(),
            ExpressionKind::Block(block) => block.last().unwrap_or(self),
            ExpressionKind::Match(a) => a.arms.last().unwrap().expression.debug_last_expression(),
            ExpressionKind::Guard(g) => g.last().unwrap().result.debug_last_expression(),
            ExpressionKind::If(_, a, _) => a.debug_last_expression(),
            ExpressionKind::When(_, b, _a) => b,
            ExpressionKind::TupleDestructuring(_, _, x) => x,
            ExpressionKind::Lambda(_, a) => a.debug_last_expression(),
            _ => self,
        }
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}{},{:?}", self.node, self.ty, self.kind)
    }
}

#[derive(Debug, Clone)]
pub struct WhenBinding {
    pub variable: VariableRef,
    pub expr: Expression,
}

impl WhenBinding {
    #[must_use]
    pub const fn has_expression(&self) -> bool {
        !matches!(self.expr.kind, ExpressionKind::VariableAccess(_))
    }
}

#[derive(Debug, Clone)]
pub enum StartOfChainKind {
    Expression(Box<Expression>),
    Variable(VariableRef),
}

#[derive(Debug, Clone)]
pub struct StartOfChain {
    pub kind: StartOfChainKind,
    pub node: Node,
}

impl StartOfChain {}

impl StartOfChainKind {
    #[must_use]
    pub fn ty(&self) -> TypeRef {
        match self {
            Self::Expression(expr) => expr.ty.clone(),
            Self::Variable(var) => var.resolved_type.clone(),
        }
    }

    #[must_use]
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Expression(_call) => {
                // The language can never return something that is mutable
                false
            }
            Self::Variable(var) => var.is_mutable(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    // Access Lookup values
    ConstantAccess(ConstantRef),
    VariableAccess(VariableRef),

    // ----

    // Operators
    BinaryOp(BinaryOperator),
    UnaryOp(UnaryOperator),
    PostfixChain(StartOfChain, Vec<Postfix>),

    // Coerce
    // the `?` operator. unwraps the value, unless it is none
    CoerceOptionToBool(Box<Expression>),
    CoerceIntToChar(Box<Expression>),
    CoerceIntToByte(Box<Expression>),
    CoerceToAny(Box<Expression>),

    // Calls
    IntrinsicCallEx(IntrinsicFunction, Vec<ArgumentExpression>),
    InternalCall(InternalFunctionDefinitionRef, Vec<ArgumentExpression>),
    HostCall(ExternalFunctionDefinitionRef, Vec<ArgumentExpression>),

    // Constructing
    VariableDefinition(VariableRef, Box<Expression>), // First time assignment
    VariableDefinitionLValue(VariableRef, SingleLocationExpression), // Bind variable to memory location
    VariableReassignment(VariableRef, Box<Expression>),

    Assignment(Box<TargetAssignmentLocation>, Box<Expression>),
    CompoundAssignment(
        TargetAssignmentLocation,
        CompoundOperatorKind,
        Box<Expression>,
    ),

    AnonymousStructLiteral(AnonymousStructLiteral),
    NamedStructLiteral(Box<Expression>),

    FloatLiteral(Fp),
    NoneLiteral,
    IntLiteral(i32),
    ByteLiteral(u8),
    StringLiteral(String),
    BoolLiteral(bool),
    EnumVariantLiteral(EnumVariantType, EnumLiteralExpressions), // TypeRef: EnumType
    TupleLiteral(Vec<Expression>),

    InitializerList(TypeRef, Vec<Expression>),
    InitializerPairList(TypeRef, Vec<(Expression, Expression)>),

    Option(Option<Box<Expression>>), // Wrapping an expression in `Some()`

    // Loops
    ForLoop(ForPattern, Iterable, Box<Expression>),
    WhileLoop(BooleanExpression, Box<Expression>),

    Block(Vec<Expression>),

    // Match and compare
    Match(Match),
    Guard(Vec<Guard>),
    If(BooleanExpression, Box<Expression>, Option<Box<Expression>>),
    When(Vec<WhenBinding>, Box<Expression>, Option<Box<Expression>>),

    TupleDestructuring(Vec<VariableRef>, TypeRef, Box<Expression>),

    Lambda(Vec<VariableRef>, Box<Expression>),
    BorrowMutRef(Box<SingleLocationExpression>),
    Error(ErrorKind),
}

#[derive(Debug, Clone)]
pub struct StringConst(pub Node);

#[derive(Debug, Clone)]
pub struct ArrayInstantiation {
    pub expressions: Vec<Expression>,
    pub item_type: TypeRef,
    pub array_type: TypeRef,
    pub array_type_ref: TypeRef,
}

#[derive(Debug, Clone)]
pub enum ForPattern {
    Single(VariableRef),
    Pair(VariableRef, VariableRef),
}

impl ForPattern {
    #[must_use]
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Single(variable) => variable.is_mutable(),
            Self::Pair(a, b) => a.is_mutable() || b.is_mutable(),
        }
    }
}

impl Display for ForPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "resolved_for_pattern")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ModulePathItem(pub Node);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalTypeIdentifier(pub Node);

#[derive(Debug, Clone)]
pub struct Constant {
    pub symbol_id: TopLevelSymbolId,
    pub name: Node,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: Expression,
    pub resolved_type: TypeRef,
    pub function_scope_state: VariableScopes,
}
pub type ConstantRef = Rc<Constant>;

pub type OptionTypeRef = Rc<OptionType>;

#[derive(Debug, Clone)]
pub struct OptionType {
    pub item_type: TypeRef,
}

/*
pub fn sort_struct_fields(
    unordered_seq_map: &SeqMap<String, StructTypeField>,
) -> SeqMap<String, StructTypeField> {
    let mut sorted_pairs: Vec<(&String, &StructTypeField)> = unordered_seq_map.iter().collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(b.0));
    let mut ordered_seq_map = SeqMap::new();

    for (name, field) in sorted_pairs {
        ordered_seq_map.insert(name, field).unwrap() // We know already that the key fields are unique
    }

    ordered_seq_map
}
*/

#[derive(Debug, Clone)]
pub struct ImplMember {}

#[derive(Debug, Clone)]
pub enum UseItem {
    Identifier(Node),
    TypeIdentifier(Node),
}

#[derive(Debug, Clone)]
pub struct Use {
    pub path: Vec<Node>,
    pub items: Vec<UseItem>,
}

#[derive(Debug, Clone)]
pub struct ImplFunctions {
    pub functions: SeqMap<String, FunctionRef>,
}

impl Default for ImplFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl ImplFunctions {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: SeqMap::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssociatedImpls {
    pub functions: SeqMap<TypeRef, ImplFunctions>,
}

impl AssociatedImpls {}

impl AssociatedImpls {}

impl AssociatedImpls {}

impl Default for AssociatedImpls {
    fn default() -> Self {
        Self::new()
    }
}

impl AssociatedImpls {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: SeqMap::default(),
        }
    }
}

impl AssociatedImpls {
    pub fn prepare(&mut self, ty: &TypeRef) {
        self.functions
            .insert(ty.clone(), ImplFunctions::new())
            .unwrap_or_else(|_| panic!("should work {ty:?}"));
    }

    #[must_use]
    pub fn is_prepared(&self, ty: &TypeRef) -> bool {
        self.functions.contains_key(ty)
    }
    #[must_use]
    pub fn get_member_function(&self, ty: &TypeRef, function_name: &str) -> Option<&FunctionRef> {
        let maybe_found_impl = self.functions.get(ty);
        if let Some(found_impl) = maybe_found_impl
            && let Some(func) = found_impl.functions.get(&function_name.to_string())
        {
            return Some(func);
        }
        None
    }

    fn has_internal_member_function(&self, ty: &TypeRef, function_name: &str) -> bool {
        let maybe_found_impl = self.functions.get(ty);
        if let Some(found_impl) = maybe_found_impl
            && let Some(_func) = found_impl.functions.get(&function_name.to_string())
        {
            return true;
        }
        false
    }

    #[must_use]
    pub fn api_get_external_function(
        &self,
        ty: &TypeRef,
        function_name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name)
            && let Function::External(ext_fn) = &**found
        {
            return Some(ext_fn);
        }
        None
    }

    #[must_use]
    pub fn get_internal_member_function(
        &self,
        ty: &TypeRef,
        function_name: &str,
    ) -> Option<&InternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name)
            && let Function::Internal(int_fn) = &**found
        {
            return Some(int_fn);
        }
        None
    }

    pub fn remove_internal_function_if_exists(
        &mut self,
        ty: &TypeRef,
        function_name: &str,
    ) -> bool {
        if self.has_internal_member_function(ty, function_name) {
            let functions = self.functions.get_mut(ty).unwrap();

            functions.functions.remove(&function_name.to_string());
            true
        } else {
            false
        }
    }

    pub fn add_member_function(
        &mut self,
        ty: &TypeRef,
        name: &str,
        func: FunctionRef,
    ) -> Result<(), SemanticError> {
        let maybe_found_impl = self.functions.get_mut(ty);

        if let Some(found_impl) = maybe_found_impl {
            found_impl
                .functions
                .insert(name.to_string(), func)
                .unwrap_or_else(|_| panic!("already had key {name}"));
            Ok(())
        } else {
            error!(%ty, ?name, "wasn't prepared");
            Err(SemanticError::UnknownImplOnType)
        }
    }

    pub fn add_internal_function(
        &mut self,
        ty: &TypeRef,
        func: InternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        //info!(name=?func.assigned_name, ?ty, "adding member function");
        self.add_member_function(
            ty,
            &func.assigned_name.clone(),
            Function::Internal(func.into()).into(),
        )
    }

    pub fn add_external_member_function(
        &mut self,
        ty: &TypeRef,
        func: ExternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            ty,
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }

    pub fn add_external_struct_member_function(
        &mut self,
        named_struct_type: TypeRef,
        func: Function,
    ) -> Result<(), SemanticError> {
        self.add_member_function(&named_struct_type, &func.name(), func.into())
    }

    pub fn add_external_struct_member_function_external(
        &mut self,
        named_struct_type: TypeRef,
        func: ExternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &named_struct_type,
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }

    pub fn add_external_struct_member_function_external_ref(
        &mut self,
        named_struct_type: TypeRef,
        func: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &named_struct_type,
            &func.assigned_name.clone(),
            Function::External(func).into(),
        )
    }
}

// Mutable part
#[derive(Debug, Clone)]
pub struct ProgramState {
    pub internal_function_id_allocator: InternalFunctionIdAllocator,
    pub symbol_id_allocator: SymbolIdAllocator,
    pub symbols: Symbols,
    pub refs: ReferenceTracker,
    // It is just so we don't have to do another dependency check of the
    // modules, we know that these constants have been
    // evaluated in order already
    pub constants_in_dependency_order: Vec<ConstantRef>,
    pub associated_impls: AssociatedImpls,
    pub types: TypeCache,
    pub errors: Vec<Error>,
    pub hints: Vec<Error>,
    pub infos: Vec<Error>,
}

impl ProgramState {
    #[must_use]
    pub const fn errors(&self) -> &Vec<Error> {
        &self.errors
    }
}

impl Default for ProgramState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct InternalFunctionIdAllocator {
    pub internal_function_number: InternalFunctionId,
}

impl Default for InternalFunctionIdAllocator {
    fn default() -> Self {
        Self::new()
    }
}

impl InternalFunctionIdAllocator {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            internal_function_number: 0,
        }
    }
    pub const fn alloc(&mut self) -> InternalFunctionId {
        self.internal_function_number += 1;
        self.internal_function_number
    }
}

impl ProgramState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            symbol_id_allocator: SymbolIdAllocator::new(),
            symbols: Symbols::new(),
            refs: ReferenceTracker::new(),
            internal_function_id_allocator: InternalFunctionIdAllocator::new(),
            constants_in_dependency_order: Vec::new(),
            associated_impls: AssociatedImpls::new(),
            types: TypeCache::new(),
            errors: vec![],
            hints: vec![],
            infos: vec![],
        }
    }
    pub const fn allocate_internal_function_id(&mut self) -> InternalFunctionId {
        self.internal_function_id_allocator.alloc()
    }
}

#[derive(Clone)]
pub enum EnumLiteralExpressions {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(Vec<(usize, Option<Node>, Expression)>),
}

impl Debug for EnumLiteralExpressions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Nothing => Ok(()),
            Self::Tuple(x) => write!(f, "{x:?}"),
            Self::Struct(s) => write!(f, "{s:?}"),
        }
    }
}
