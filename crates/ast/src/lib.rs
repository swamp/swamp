/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

#[derive(PartialEq, Eq, Hash, Default, Clone)]
pub struct SpanWithoutFileId {
    pub offset: u32,
    pub length: u16,
}

impl Debug for SpanWithoutFileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}:{}>", self.offset, self.length)
    }
}

// Common metadata that can be shared across all AST nodes
#[derive(PartialEq, Eq, Hash, Default, Clone)]
pub struct Node {
    pub span: SpanWithoutFileId,
    // TODO: Add comments and attributes
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.span)
    }
}

/// Identifiers ================
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct QualifiedTypeIdentifier {
    pub name: LocalTypeIdentifier,
    pub module_path: Option<ModulePath>,
    pub generic_params: Vec<GenericParameter>,
}

impl QualifiedTypeIdentifier {
    #[must_use]
    pub fn new(name: LocalTypeIdentifier, module_path: Vec<Node>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params: Vec::new(),
        }
    }

    #[must_use]
    pub fn new_with_generics(
        name: LocalTypeIdentifier,
        module_path: Vec<Node>,
        generic_params: Vec<GenericParameter>,
    ) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct QualifiedIdentifier {
    pub name: Node,
    pub module_path: Option<ModulePath>,
    pub generic_params: Vec<GenericParameter>,
}

impl QualifiedIdentifier {
    #[must_use]
    pub fn new(name: Node, module_path: Vec<Node>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params: vec![],
        }
    }

    #[must_use]
    pub fn new_with_generics(
        name: Node,
        module_path: Vec<Node>,
        generic_params: Vec<GenericParameter>,
    ) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone)]
pub struct LocalTypeIdentifier(pub Node);

impl LocalTypeIdentifier {
    #[must_use]
    pub const fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone)]
pub struct TypeVariable(pub Node);

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone)]
pub struct LocalTypeIdentifierWithOptionalTypeVariables {
    pub name: Node,
    pub type_variables: Vec<TypeVariable>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct LocalIdentifier(pub Node);

impl LocalIdentifier {
    #[must_use]
    pub const fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone)]
pub struct LocalConstantIdentifier(pub Node);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct QualifiedConstantIdentifier {
    pub name: Node,
    pub module_path: Option<ModulePath>,
}

impl QualifiedConstantIdentifier {
    #[must_use]
    pub const fn new(name: Node, module_path: Option<ModulePath>) -> Self {
        Self { name, module_path }
    }
}

#[derive(Debug, Eq, Hash, Clone, PartialEq)]
pub struct FieldName(pub Node);

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct ModulePath(pub Vec<Node>);

impl Default for ModulePath {
    fn default() -> Self {
        Self::new()
    }
}

impl ModulePath {
    #[must_use]
    pub const fn new() -> Self {
        Self(vec![])
    }
}

#[derive(Debug, Clone)]
pub enum ImportItem {
    Identifier(LocalIdentifier),
    Type(LocalTypeIdentifier),
}

#[derive(Debug, Clone)]
pub enum ImportItems {
    Nothing,
    Items(Vec<ImportItem>),
    All,
}

#[derive(Debug, Clone)]
pub struct Mod {
    pub module_path: ModulePath,
    pub items: ImportItems,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub module_path: ModulePath,
    pub items: ImportItems,
}

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct AliasType {
    pub identifier: LocalTypeIdentifier,
    pub referenced_type: Type,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Default)]
pub struct AnonymousStructType {
    pub fields: Vec<StructTypeField>,
}

impl AnonymousStructType {
    #[must_use]
    pub const fn new(fields: Vec<StructTypeField>) -> Self {
        Self { fields }
    }
}

#[derive(Debug, Clone)]
pub struct ConstantInfo {
    pub constant_identifier: LocalConstantIdentifier,
    pub annotation: Option<Type>,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct NamedStructDef {
    pub identifier: LocalTypeIdentifierWithOptionalTypeVariables,
    pub struct_type: AnonymousStructType,
}

#[derive(Debug, Clone)]
pub enum DefinitionKind {
    AliasDef(AliasType),
    NamedStructDef(NamedStructDef),
    EnumDef(
        LocalTypeIdentifierWithOptionalTypeVariables,
        Vec<EnumVariantType>,
    ),
    FunctionDef(Function),
    ImplDef(LocalTypeIdentifierWithOptionalTypeVariables, Vec<Function>),
    Mod(Mod),
    Use(Use),
    // Other
    Constant(ConstantInfo),
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub node: Node,
    pub kind: DefinitionKind,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct ForVar {
    pub identifier: Node,
    pub is_mut: Option<Node>,
}

#[derive(Debug, Clone)]
pub enum ForPattern {
    Single(ForVar),
    Pair(ForVar, ForVar),
}

impl ForPattern {
    #[must_use]
    pub const fn is_key_variable_mut(&self) -> bool {
        match self {
            Self::Single(_a) => false,
            Self::Pair(a, _b) => a.is_mut.is_some(),
        }
    }
    #[must_use]
    pub fn is_value_mut(&self) -> Option<Node> {
        match self {
            Self::Single(a) => a.is_mut.clone(),
            Self::Pair(a, b) => {
                assert!(
                    a.is_mut.is_none(),
                    "key target var is not allowed to be mut"
                );
                b.is_mut.clone()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IterableExpression {
    pub expression: Box<Expression>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

#[derive(Debug, Clone)]
pub struct VariableBinding {
    pub variable: Variable,
    pub expression: Option<Expression>,
}

impl Variable {
    #[must_use]
    pub const fn new(name: Node, is_mutable: Option<Node>) -> Self {
        Self { name, is_mutable }
    }
}

// Since this is a helper struct, we want to implement the debug output for it
// to have it more concise
impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(found) = &self.is_mutable {
            write!(f, "mut {found:?} {:?}", self.name)
        } else {
            write!(f, "{:?}", self.name)
        }
    }
}

#[derive(Debug, Eq, Clone, PartialEq)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Node,
    pub params: Vec<Parameter>,
    pub self_parameter: Option<SelfParameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct FunctionWithBody {
    pub attributes: Vec<Attribute>,
    pub declaration: FunctionDeclaration,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Function {
    Internal(FunctionWithBody),
    External(Node, FunctionDeclaration),
}

impl Function {
    #[must_use]
    pub const fn node(&self) -> &Node {
        match self {
            Self::Internal(func_with_body) => &func_with_body.body.node,
            Self::External(node, _) => node,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SelfParameter {
    pub is_mutable: Option<Node>,
    pub self_node: Node,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssignmentOperatorKind {
    Compound(CompoundOperatorKind),
    Assign, // =
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CompoundOperatorKind {
    Add,    // +=
    Sub,    // -=
    Mul,    // *=
    Div,    // /=
    Modulo, // %=
}

#[derive(Debug, Clone)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[derive(Debug, Clone)]
pub enum RangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub node: Node,
}

impl Debug for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}{:?}", self.node.span, self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum Postfix {
    FieldAccess(Node),
    Subscript(Expression),
    MemberCall(Node, Option<Vec<GenericParameter>>, Vec<Expression>),
    FunctionCall(Node, Option<Vec<GenericParameter>>, Vec<Expression>),
    OptionalChainingOperator(Node),     // ?-postfix
    NoneCoalescingOperator(Expression), // ??-postfix
    SubscriptTuple(Expression, Expression),
}

#[derive(Debug, Clone)]
pub struct PostfixChain {
    pub base: Box<Expression>,
    pub postfixes: Vec<Postfix>,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    // Access
    PostfixChain(PostfixChain),

    // References
    ContextAccess, // Context/Lone/Bare-dot. TODO: Not implemented yet.
    VariableReference(Variable),
    ConstantReference(QualifiedConstantIdentifier),
    StaticMemberFunctionReference(QualifiedTypeIdentifier, Node),
    IdentifierReference(QualifiedIdentifier),

    // Assignments
    VariableDefinition(Variable, Option<Type>, Box<Expression>),
    VariableAssignment(Variable, Box<Expression>),
    Assignment(Box<Expression>, Box<Expression>),
    CompoundAssignment(Box<Expression>, CompoundOperator, Box<Expression>),
    DestructuringAssignment(Vec<Variable>, Box<Expression>),

    // Operators
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Blocks
    Block(Vec<Expression>),
    With(Vec<VariableBinding>, Box<Expression>),
    When(
        Vec<VariableBinding>,
        Box<Expression>,
        Option<Box<Expression>>,
    ),

    // Control flow
    ForLoop(ForPattern, IterableExpression, Box<Expression>),
    WhileLoop(Box<Expression>, Box<Expression>),

    // Compare and Matching
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Match(Box<Expression>, Vec<MatchArm>),
    Guard(Vec<GuardExpr>),

    InterpolatedString(Vec<StringPart>),

    // Literals
    AnonymousStructLiteral(Vec<FieldExpression>, bool),
    NamedStructLiteral(QualifiedTypeIdentifier, Vec<FieldExpression>, bool),
    Range(Box<Expression>, Box<Expression>, RangeMode),
    Literal(LiteralKind),

    Lambda(Vec<Variable>, Box<Expression>),
    Error, // Something was wrong in parsing
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

// TODO: Add literals for Codepoint and Byte
#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int,
    Float,
    String(String),
    Bool,
    EnumVariant(EnumVariantLiteral),
    Tuple(Vec<Expression>),
    InternalInitializerList(Vec<Expression>),
    InternalInitializerPairList(Vec<(Expression, Expression)>),
    None,
}

#[derive(Debug, Clone)]
pub struct FieldExpression {
    pub field_name: FieldName,
    pub expression: Expression,
}

#[derive(Debug, Eq, Hash, Clone, PartialEq)]
pub struct StructTypeField {
    pub field_name: FieldName,
    pub field_type: Type,
}

#[derive(Debug, Clone)]
pub enum EnumVariantLiteral {
    Simple(QualifiedTypeIdentifier, LocalTypeIdentifier),
    Tuple(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        Vec<Expression>,
    ),
    Struct(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        Vec<FieldExpression>,
        bool,
    ),
}

impl EnumVariantLiteral {
    #[must_use]
    pub const fn node(&self) -> &Node {
        match self {
            Self::Tuple(ident, _, _) | Self::Struct(ident, _, _, _) | Self::Simple(ident, _) => {
                &ident.name.0
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum EnumVariantType {
    Simple(Node),
    Direct(Node, Type),
    Tuple(Node, Vec<Type>),
    Struct(Node, AnonymousStructType),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct TypeForParameter {
    pub ast_type: Type,
    pub is_mutable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParameter {
    Type(Type),
    UnsignedInt(Node),
    UnsignedTupleInt(Node, Node),
}

impl GenericParameter {
    #[must_use]
    pub fn get_unsigned_int_node(&self) -> &Node {
        let Self::UnsignedInt(node) = self else {
            panic!("wasn't unsigned int")
        };
        node
    }

    #[must_use]
    pub fn get_unsigned_int_tuple_nodes(&self) -> (&Node, &Node) {
        let Self::UnsignedTupleInt(first, second) = self else {
            panic!("wasn't unsigned int tuple")
        };
        (first, second)
    }
}

impl GenericParameter {
    #[must_use]
    pub fn get_type(&self) -> &Type {
        let Self::Type(ty) = self else {
            panic!("{}", format!("wasn't type {self:?}"))
        };
        ty
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Type {
    // Composite
    FixedCapacityArray(Box<Type>, Node),          // `[T; N]`
    Slice(Box<Type>), // `[T]`. Contiguous memory segments without ownership, Unsized Type (DST) that has inline data
    FixedCapacityMap(Box<Type>, Box<Type>, Node), // `[K:V;N]`
    DynamicLengthMap(Box<Type>, Box<Type>), // `[K:V]`

    AnonymousStruct(AnonymousStructType),
    Unit,
    Tuple(Vec<Type>),
    Function(Vec<TypeForParameter>, Box<Type>),

    Named(QualifiedTypeIdentifier),

    Optional(Box<Type>, Node),
}

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub node: Node,
}

// Takes a left and right side expression
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

// Only takes one expression argument
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not(Node),
    Negate(Node),
    BorrowMutRef(Node),
}

#[derive(Debug, Clone)]
pub struct GuardExpr {
    pub clause: GuardClause,
    pub result: Expression,
}

#[derive(Debug, Clone)]
pub enum GuardClause {
    Wildcard(Node),
    Expression(Expression),
}

// Patterns are used in matching and destructuring
#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard(Node),
    ConcretePattern(Node, ConcretePattern, Option<GuardClause>),
}

#[derive(Debug, Clone)]
pub enum ConcretePattern {
    EnumPattern(Node, DestructuringPattern), // VariantName <destructuring>
    Literal(LiteralKind),                    // 42 or "hello" or 2.42
}

#[derive(Debug, Clone)]
pub enum PatternVariableOrWildcard {
    Variable(Variable),
    Wildcard(Node),
}

// Which destructuring to use, or none
#[derive(Debug, Clone)]
pub enum DestructuringPattern {
    /// A struct-like variant: `Variant { field, .. }`
    Struct { fields: Vec<Variable> },

    /// A tuple-like variant: `Variant(item, ..)`
    Tuple {
        elements: Vec<PatternVariableOrWildcard>,
    },

    /// A single payload variable: `Some(payload)` or `Ok(value)`
    None { variable: Variable },

    /// A unit variant with no payload: `Red`, `Green`, `Blue`
    Unit,
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(Node, String),
    Interpolation(Box<Expression>, Option<FormatSpecifier>),
}

// TODO: Implement them again
// with a to_format() call or similar.
#[derive(Debug, Clone)]
pub enum FormatSpecifier {
    LowerHex(Node),                      // :x
    UpperHex(Node),                      // :X
    Binary(Node),                        // :b
    Float(Node),                         // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float(Node),
    String(Node),
}

#[derive(Debug, Clone)]
pub enum AttributeArg {
    /// A path/identifier, e.g. `Debug` or `unix`
    Path(QualifiedIdentifier),
    /// A literal value, e.g. `"foo"`, `42`, `true`
    Literal(AttributeValue),
    /// A function call, e.g. `any(unix, windows)` or `rename = "foo"`
    Function(QualifiedIdentifier, Vec<AttributeArg>),
}

#[derive(Debug, Clone)]
pub enum AttributeLiteralKind {
    Int,
    String(String),
    Bool,
}

#[derive(Debug, Clone)]
pub enum AttributeValue {
    Literal(Node, AttributeLiteralKind),
    Path(QualifiedIdentifier),
    Function(QualifiedIdentifier, Vec<AttributeArg>),
}

#[derive(Debug, Clone)]
pub struct Attribute {
    pub is_inner: bool,
    pub path: QualifiedIdentifier,
    pub args: Vec<AttributeArg>,
    pub node: Node,
}

#[derive()]
pub struct Module {
    pub expression: Option<Expression>,
    pub definitions: Vec<Definition>,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for definition in &self.definitions {
            writeln!(f, "{definition:?}")?;
        }

        if !self.definitions.is_empty() && self.expression.is_some() {
            writeln!(f, "---")?;
        }

        if let Some(found_expression) = &self.expression {
            match &found_expression.kind {
                ExpressionKind::Block(expressions) => {
                    for expression in expressions {
                        writeln!(f, "{expression:?}")?;
                    }
                }
                _ => writeln!(f, "{found_expression:?}")?,
            }
        }

        Ok(())
    }
}

impl Module {
    #[must_use]
    pub const fn new(definitions: Vec<Definition>, expression: Option<Expression>) -> Self {
        Self {
            expression,
            definitions,
        }
    }

    #[must_use]
    pub const fn expression(&self) -> &Option<Expression> {
        &self.expression
    }

    #[must_use]
    pub const fn definitions(&self) -> &Vec<Definition> {
        &self.definitions
    }

    #[must_use]
    pub fn imports(&self) -> Vec<&Use> {
        let mut use_items = Vec::new();

        for def in &self.definitions {
            if let DefinitionKind::Use(use_info) = &def.kind {
                use_items.push(use_info);
            }
        }

        use_items
    }
}
