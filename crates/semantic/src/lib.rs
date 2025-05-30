/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod inst_cache;
pub mod instantiator;
pub mod intr;
pub mod prelude;
pub mod type_var_stack;
use crate::instantiator::Instantiator;
use crate::intr::IntrinsicFunction;
use crate::prelude::IntrinsicFunctionDefinitionRef;
pub use fixed32::Fp;
use seq_map::SeqMap;
use source_map_node::Node;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use swamp_types::GenericAwareSignature;
use swamp_types::StructLikeType;
use swamp_types::prelude::*;
use tracing::error;

#[derive(Debug, Clone)]
pub struct TypeWithMut {
    pub resolved_type: Type,
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
    MismatchedTypes { expected: Type, found: Vec<Type> },
    UnknownImplOnType,
    UnknownTypeVariable,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LocalIdentifier(pub Node);

#[derive(Debug)]
pub struct InternalMainExpression {
    pub expression: Expression,
    pub function_parameters: Vec<VariableRef>,
    pub function_variables: Vec<VariableRef>,
    pub program_unique_id: InternalFunctionId,
}

//#[derive(Debug,Clone)]
pub struct InternalFunctionDefinition {
    pub body: Expression,
    pub name: LocalIdentifier,
    pub assigned_name: String,
    pub associated_with_type: Option<Type>,
    pub defined_in_module_path: Vec<String>,
    pub signature: GenericAwareSignature,
    pub parameters: Vec<VariableRef>,
    pub function_variables: Vec<VariableRef>,
    pub program_unique_id: InternalFunctionId,
    pub attributes: Attributes,
}

impl InternalFunctionDefinition {
    #[must_use]
    pub fn all_parameters_and_variables_are_concrete(&self) -> bool {
        for var in &self.parameters {
            if !var.resolved_type.is_concrete() || var.resolved_type.is_function_type() {
                //warn!(?var.assigned_name, ?var.resolved_type, "skipping function due to not concrete parameters or variables");
                return false;
            }
        }

        for var in &self.function_variables {
            if !var.resolved_type.is_concrete() || var.resolved_type.is_function_type() {
                //warn!(?var.assigned_name, ?var.resolved_type, "skipping function due to not concrete parameters or variables");
                return false;
            }
        }
        true
    }
}

impl Default for InternalFunctionDefinition {
    fn default() -> Self {
        Self {
            body: Expression {
                ty: Type::Never,
                node: Node::default(),
                kind: ExpressionKind::Block(vec![]),
            },
            name: LocalIdentifier(Node::default()),
            assigned_name: String::new(),
            associated_with_type: None,
            defined_in_module_path: vec![],
            signature: GenericAwareSignature {
                signature: Signature {
                    parameters: vec![],
                    return_type: Box::new(Type::Never),
                },
                generic_type_variables: vec![],
            },
            //variable_scopes: FunctionScopeState::new(),
            parameters: Vec::new(),
            function_variables: Vec::new(),
            program_unique_id: 0,
            attributes: Attributes::default(),
        }
    }
}

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
    pub name: Option<Node>,
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
}

#[derive(Debug, Clone)]
pub struct BlockScope {
    pub mode: BlockScopeMode,
    pub variables: SeqMap<String, VariableRef>,
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
        }
    }
}

#[derive(Clone)]
pub struct FunctionScopeState {
    pub block_scope_stack: Vec<BlockScope>,
    pub variable_index: usize,
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
        let index = self.variable_index;
        self.variable_index += 1;
        index
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
    pub name: Node,
    pub assigned_name: String,
    pub resolved_type: Type,
    pub mutable_node: Option<Node>,
    pub variable_type: VariableType,

    pub scope_index: usize,
    pub variable_index: usize,

    pub unique_id_within_function: usize,
    pub is_unused: bool,
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
    pub item_type: Type,
    pub int_expression: Expression,
    pub array_expression: Expression,
    pub array_type: Type,
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

/*
#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(Node, String),
    Interpolation(Expression, Option<FormatSpecifier>),
}

 */

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
    pub fn maybe_node(&self) -> Option<&Node> {
        match self {
            Self::Internal(x) => Some(&x.name.0),
            Self::External(y) => y.name.as_ref(),
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
            Self::Internal(internal) => &internal.signature.signature,
            Self::External(external) => &external.signature,
            Self::Intrinsic(i) => &i.signature,
        }
    }

    #[must_use]
    pub fn signatures(&self) -> (Option<&GenericAwareSignature>, &Signature) {
        match self {
            Self::Internal(internal) => (Some(&internal.signature), &internal.signature.signature),
            Self::External(external) => (None, &external.signature),
            Self::Intrinsic(i) => (None, &i.signature),
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
    pub expression_type: Type,
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
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub enum PatternElement {
    Variable(VariableRef),
    VariableWithFieldIndex(VariableRef, usize),
    Wildcard(Node),
}

#[derive(Debug, Clone)]
pub struct Iterable {
    pub key_type: Option<Type>, // It does not have to support a key type
    pub value_type: Type,

    pub resolved_expression: Box<ArgumentExpression>,
}

#[derive(Debug, Clone)]
pub struct AnonymousStructLiteral {
    pub source_order_expressions: Vec<(usize, Option<Node>, Expression)>,
    pub struct_like_type: StructLikeType,
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

#[must_use]
pub fn create_rust_type(name: &str, external_number: u32) -> ExternalType {
    ExternalType {
        type_name: name.to_string(),
        number: external_number,
    }
}

#[derive(Debug, Clone)]
pub struct Guard {
    pub condition: Option<BooleanExpression>,
    pub result: Expression,
}

#[derive(Debug, Clone)]
pub struct Postfix {
    pub node: Node,
    pub ty: Type,
    pub kind: PostfixKind,
}

#[derive(Debug, Clone)]
pub struct SliceViewType {
    pub element: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct VecType {
    pub element: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct MapType {
    pub key: Box<Type>,
    pub value: Box<Type>,
}

#[derive(Debug, Clone)]
pub enum PostfixKind {
    StructField(AnonymousStructType, usize),
    MemberCall(FunctionRef, Vec<ArgumentExpression>),
    OptionalChainingOperator,           // ? operator
    NoneCoalescingOperator(Expression), // ?? operator
    SliceViewSubscript(SliceViewType, Expression),
    VecSubscript(VecType, Expression),
    MapSubscript(MapType, Expression),
}

#[derive(Debug, Clone)]
pub enum LocationAccessKind {
    FieldIndex(AnonymousStructType, usize),
    IntrinsicSubscript(IntrinsicFunction, Vec<Expression>),
    SliceViewSubscript(SliceViewType, Expression),
    SubscriptVec(Box<Type>, Expression),
}

#[derive(Debug, Clone)]
pub struct LocationAccess {
    pub node: Node,
    pub ty: Type,
    pub kind: LocationAccessKind,
}

#[derive(Debug, Clone)]
pub struct SingleLocationExpression {
    pub kind: MutableReferenceKind,
    pub node: Node,
    pub ty: Type,

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
    pub fn ty(&self) -> Type {
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
    pub ty: Type,
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
            ExpressionKind::When(_, b, a) => b,
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
    pub expr: ArgumentExpression,
}

impl WhenBinding {
    #[must_use]
    pub const fn has_expression(&self) -> bool {
        match &self.expr {
            ArgumentExpression::Expression(expr) => {
                !matches!(expr.kind, ExpressionKind::VariableAccess(_))
            }
            ArgumentExpression::BorrowMutableReference(_) => true,
        }
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
    pub fn ty(&self) -> Type {
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

    // Conversion
    // the `?` operator. unwraps the value, unless it is none
    CoerceOptionToBool(Box<Expression>),

    // Calls
    IntrinsicCallEx(IntrinsicFunction, Vec<ArgumentExpression>),
    InternalCall(InternalFunctionDefinitionRef, Vec<ArgumentExpression>),
    HostCall(ExternalFunctionDefinitionRef, Vec<ArgumentExpression>),

    // For calls from returned function values
    //FunctionValueCall(Signature, Box<Expression>, Vec<MutRefOrImmutableExpression>),

    // Constructing
    VariableDefinition(VariableRef, Box<Expression>), // First time assignment
    VariableReassignment(VariableRef, Box<Expression>),
    VariableBinding(VariableRef, Box<ArgumentExpression>),

    Assignment(Box<TargetAssignmentLocation>, Box<Expression>),
    CompoundAssignment(
        TargetAssignmentLocation,
        CompoundOperatorKind,
        Box<Expression>,
    ),

    AnonymousStructLiteral(AnonymousStructLiteral),
    Literal(Literal),
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

    TupleDestructuring(Vec<VariableRef>, Vec<Type>, Box<Expression>),

    Lambda(Vec<VariableRef>, Box<Expression>),
    BorrowMutRef(Box<SingleLocationExpression>),
}

#[derive(Debug, Clone)]
pub struct StringConst(pub Node);

#[derive(Debug, Clone)]
pub enum Literal {
    FloatLiteral(Fp),
    NoneLiteral,
    IntLiteral(i32),
    StringLiteral(String),
    BoolLiteral(bool),

    EnumVariantLiteral(EnumType, EnumVariantType, EnumLiteralData),
    TupleLiteral(Vec<Type>, Vec<Expression>),

    Slice(Type, Vec<Expression>),
    SlicePair(Type, Vec<(Expression, Expression)>),
}

#[derive(Debug, Clone)]
pub struct ArrayInstantiation {
    pub expressions: Vec<Expression>,
    pub item_type: Type,
    pub array_type: Type,
    pub array_type_ref: Type,
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
    pub name: Node,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: Expression,
    pub resolved_type: Type,
    pub function_scope_state: Vec<VariableRef>,
}
pub type ConstantRef = Rc<Constant>;

pub type OptionTypeRef = Rc<OptionType>;

#[derive(Debug, Clone)]
pub struct OptionType {
    pub item_type: Type,
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
    pub functions: SeqMap<Type, ImplFunctions>,
}

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
    pub fn prepare(&mut self, ty: &Type) {
        self.functions
            .insert(ty.clone(), ImplFunctions::new())
            .expect("should work");
    }
    #[must_use]
    pub fn get_member_function(&self, ty: &Type, function_name: &str) -> Option<&FunctionRef> {
        let maybe_found_impl = self.functions.get(ty);
        if let Some(found_impl) = maybe_found_impl {
            if let Some(func) = found_impl.functions.get(&function_name.to_string()) {
                return Some(func);
            }
        }
        None
    }

    #[must_use]
    pub fn api_get_external_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name) {
            if let Function::External(ext_fn) = &**found {
                return Some(ext_fn);
            }
        }
        None
    }

    #[must_use]
    pub fn api_fetch_external_function_id(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> ExternalFunctionId {
        self.api_get_external_function(ty, function_name)
            .unwrap()
            .id
    }

    #[must_use]
    pub fn get_internal_member_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<&InternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name) {
            if let Function::Internal(int_fn) = &**found {
                return Some(int_fn);
            }
        }
        None
    }

    pub fn add_member_function(
        &mut self,
        ty: &Type,
        name: &str,
        func: FunctionRef,
    ) -> Result<(), SemanticError> {
        let maybe_found_impl = self.functions.get_mut(ty);

        if let Some(found_impl) = maybe_found_impl {
            found_impl
                .functions
                .insert(name.to_string(), func)
                .expect("todo");
            Ok(())
        } else {
            error!(%ty, ?name, "wasn't prepared");
            Err(SemanticError::UnknownImplOnType)
        }
    }

    pub fn add_external_member_function(
        &mut self,
        ty: &Type,
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
        named_struct_type: &NamedStructType,
        func: Function,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type.clone()),
            &func.name(),
            func.into(),
        )
    }

    pub fn add_external_struct_member_function_external(
        &mut self,
        named_struct_type: NamedStructType,
        func: ExternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type),
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }

    pub fn add_external_struct_member_function_external_ref(
        &mut self,
        named_struct_type: NamedStructType,
        func: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type),
            &func.assigned_name.clone(),
            Function::External(func).into(),
        )
    }
}

// Mutable part
#[derive(Debug, Clone)]
pub struct ProgramState {
    pub external_function_number: ExternalFunctionId,
    pub internal_function_id_allocator: InternalFunctionIdAllocator,
    // It is just so we don't have to do another dependency check of the
    // modules, we know that these constants have been
    // evaluated in order already
    pub constants_in_dependency_order: Vec<ConstantRef>,
    pub instantiator: Instantiator,
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
            external_function_number: 0,
            internal_function_id_allocator: InternalFunctionIdAllocator::new(),
            constants_in_dependency_order: Vec::new(),
            instantiator: Instantiator::new(),
        }
    }

    pub const fn allocate_external_function_id(&mut self) -> ExternalFunctionId {
        self.external_function_number += 1;
        self.external_function_number
    }

    pub const fn allocate_internal_function_id(&mut self) -> InternalFunctionId {
        self.internal_function_id_allocator.alloc()
    }
}

#[derive(Clone)]
pub enum EnumLiteralData {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(Vec<(usize, Option<Node>, Expression)>),
}

impl Debug for EnumLiteralData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Nothing => Ok(()),
            Self::Tuple(x) => write!(f, "{x:?}"),
            Self::Struct(s) => write!(f, "{s:?}"),
        }
    }
}
