/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
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
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct QualifiedTypeIdentifier {
    pub name: LocalTypeIdentifier,
    pub module_path: Option<ModulePath>,
    pub generic_params: Vec<Type>,
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
        generic_params: Vec<Type>,
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
}

impl QualifiedIdentifier {
    #[must_use]
    pub fn new(name: Node, module_path: Vec<Node>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self { name, module_path }
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

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LocalIdentifier(pub Node);

impl LocalIdentifier {
    #[must_use]
    pub const fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ConstantIdentifier(pub Node);

impl ConstantIdentifier {
    #[must_use]
    pub const fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(Debug, Eq, Hash, PartialEq)]
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

#[derive(Debug)]
pub enum UseItem {
    Identifier(LocalIdentifier),
    Type(LocalTypeIdentifier),
}

#[derive(Debug)]
pub struct Use {
    pub module_path: ModulePath,
    //    pub assigned_path: Vec<String>,
    pub items: Vec<UseItem>,
}

#[derive(Debug)]
pub struct Mod {
    pub module_path: ModulePath,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AliasType {
    pub identifier: LocalTypeIdentifier,
    pub referenced_type: Type,
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct StructType {
    pub identifier: LocalTypeIdentifier,
    pub fields: Vec<FieldType>,
}

impl StructType {
    #[must_use]
    pub const fn new(identifier: LocalTypeIdentifier, fields: Vec<FieldType>) -> Self {
        Self { identifier, fields }
    }
}

#[derive(Debug)]
pub struct ConstantInfo {
    pub constant_identifier: ConstantIdentifier,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub enum Definition {
    AliasDef(AliasType),
    StructDef(StructType),
    EnumDef(Node, Vec<EnumVariantType>),
    FunctionDef(Function),
    ImplDef(Node, Vec<Function>),
    Use(Use),
    Mod(Mod),

    // Other
    Comment(Node),
    Constant(ConstantInfo),
}

#[derive(Debug)]
pub struct ForVar {
    pub identifier: Node,
    pub is_mut: Option<Node>,
}

#[derive(Debug)]
pub enum ForPattern {
    Single(ForVar),
    Pair(ForVar, ForVar),
}

impl ForPattern {
    #[must_use]
    pub fn any_mut(&self) -> Option<Node> {
        match self {
            Self::Single(a) => a.is_mut.clone(),
            Self::Pair(a, b) => a.is_mut.clone().or_else(|| b.is_mut.clone()),
        }
    }
}

#[derive(Debug)]
pub struct IterableExpression {
    pub expression: Box<MutableOrImmutableExpression>,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

#[derive(Debug)]
pub struct VariableBinding {
    pub variable: Variable,
    pub expression: MutableOrImmutableExpression,
}

#[derive(Debug)]
pub struct WhenBinding {
    pub variable: Variable,
    pub expression: Option<MutableOrImmutableExpression>,
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

#[derive(Debug, Eq, PartialEq)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Node,
    pub params: Vec<Parameter>,
    pub self_parameter: Option<SelfParameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug)]
pub struct FunctionWithBody {
    pub declaration: FunctionDeclaration,
    pub body: Expression,
}

#[derive(Debug)]
pub enum Function {
    Internal(FunctionWithBody),
    External(FunctionDeclaration),
}

#[derive(Debug)]
pub struct SelfParameter {
    pub is_mutable: Option<Node>,
    pub self_node: Node,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AssignmentOperatorKind {
    Compound(CompoundOperatorKind),
    Assign, // =
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompoundOperatorKind {
    Add,    // +=
    Sub,    // -=
    Mul,    // *=
    Div,    // /=
    Modulo, // %=
}

#[derive(Debug)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[derive(Debug)]
pub enum RangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Debug)]
pub struct MutableOrImmutableExpression {
    pub is_mutable: Option<Node>,
    pub expression: Expression,
}

pub struct Expression {
    pub kind: ExpressionKind,
    pub node: Node,
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}{:?}", self.node.span, self.kind)
    }
}

#[derive(Debug)]
pub enum Postfix {
    FieldAccess(Node),
    Subscript(Expression),
    MemberCall(Node, Vec<MutableOrImmutableExpression>),
    FunctionCall(Node, Vec<MutableOrImmutableExpression>),
    OptionUnwrap(Node),       // ?-postfix
    NoneCoalesce(Expression), // ??-postfix
}

#[derive(Debug)]
pub struct PostfixChain {
    pub base: Box<Expression>,
    pub postfixes: Vec<Postfix>,
}

/// Expressions are things that "converts" to a value when evaluated.
#[derive(Debug)]
pub enum ExpressionKind {
    // Access
    PostfixChain(PostfixChain),

    // References
    IdentifierReference(Variable),
    ConstantReference(ConstantIdentifier),
    FunctionReference(QualifiedIdentifier),
    StaticMemberFunctionReference(QualifiedTypeIdentifier, Node),

    // Assignments
    VariableDefinition(Variable, Option<Type>, Box<MutableOrImmutableExpression>),
    VariableAssignment(Variable, Box<MutableOrImmutableExpression>),
    Assignment(Box<Expression>, Box<Expression>),
    CompoundAssignment(Box<Expression>, CompoundOperator, Box<Expression>),
    DestructuringAssignment(Vec<Variable>, Box<Expression>),

    // Operators
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    //
    Block(Vec<Expression>),
    With(Vec<VariableBinding>, Box<Expression>),
    When(Vec<WhenBinding>, Box<Expression>, Option<Box<Expression>>),

    // Control flow
    ForLoop(ForPattern, IterableExpression, Box<Expression>),
    WhileLoop(Box<Expression>, Box<Expression>),
    Return(Option<Box<Expression>>),
    Break,
    Continue,

    // Compare and Matching
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Match(Box<Expression>, Vec<MatchArm>),
    Guard(Vec<GuardExpr>),

    InterpolatedString(Vec<StringPart>),

    // Literals
    StructLiteral(QualifiedTypeIdentifier, Vec<FieldExpression>, bool),
    Range(Box<Expression>, Box<Expression>, RangeMode),
    Literal(LiteralKind),
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

// Are constructed by themselves
#[derive(Debug)]
pub enum LiteralKind {
    Int,
    Float,
    String(String),
    Bool,
    EnumVariant(EnumVariantLiteral),
    Tuple(Vec<Expression>),
    Array(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    None, // none
}

#[derive(Debug)]
pub struct FieldExpression {
    pub field_name: FieldName,
    pub expression: Expression,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FieldType {
    pub field_name: FieldName,
    pub field_type: Type,
}

#[derive(Debug)]
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
    ),
}

#[derive(Debug, Default)]
pub struct AnonymousStructType {
    pub fields: Vec<FieldType>,
}

#[derive(Debug)]
pub enum EnumVariantType {
    Simple(Node),
    Tuple(Node, Vec<Type>),
    Struct(Node, AnonymousStructType),
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TypeForParameter {
    pub ast_type: Type,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitives
    Unit(Node),
    Int(Node),
    Float(Node),
    String(Node),
    Bool(Node),

    // Composite
    Generic(Box<Type>, Vec<Type>),
    Struct(QualifiedTypeIdentifier),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Enum(QualifiedTypeIdentifier),
    Optional(Box<Type>, Node),
    Function(Vec<TypeForParameter>, Box<Type>),

    Named(QualifiedTypeIdentifier),
}

#[derive(Debug)]
pub struct BinaryOperator {
    pub kind: BinaryOperatorKind,
    pub node: Node,
}

// Takes a left and right side expression
#[derive(Debug)]
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
    RangeExclusive,
}

// Only takes one expression argument
#[derive(Debug)]
pub enum UnaryOperator {
    Not(Node),
    Negate(Node),
}

#[derive(Debug)]
pub struct GuardExpr {
    pub clause: GuardClause,
    pub result: Expression,
}

#[derive(Debug)]
pub enum GuardClause {
    Wildcard(Node),
    Expression(Expression),
}

// Patterns are used in matching and destructuring
#[derive(Debug)]
pub enum Pattern {
    Wildcard(Node),
    NormalPattern(Node, NormalPattern, Option<GuardClause>),
}

// Patterns are used in matching and destructuring
#[derive(Debug)]
pub enum NormalPattern {
    PatternList(Vec<PatternElement>),
    EnumPattern(Node, Option<Vec<PatternElement>>),
    Literal(LiteralKind),
}

#[derive(Debug)]
pub enum PatternElement {
    Variable(Node),
    Expression(Expression),
    Wildcard(Node),
}

#[derive(Debug)]
pub enum StringPart {
    Literal(Node, String),
    Interpolation(Box<Expression>, Option<FormatSpecifier>),
}

#[derive(Debug)]
pub enum FormatSpecifier {
    LowerHex(Node),                      // :x
    UpperHex(Node),                      // :X
    Binary(Node),                        // :b
    Float(Node),                         // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug)]
pub enum PrecisionType {
    Float(Node),
    String(Node),
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
    pub fn new(definitions: Vec<Definition>, expression: Option<Expression>) -> Self {
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
            if let Definition::Use(use_info) = def {
                use_items.push(use_info);
            }
        }

        use_items
    }
}
