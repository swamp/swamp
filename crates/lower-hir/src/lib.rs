use seq_map::SeqMap;
use source_map_node::Node;
use swamp_hir::{
    Atom, AtomKind, Expression, HCallTarget, NodeId, Place, PlaceKind, RValue, RValueKind,
    RuntimeOp, Statement, StatementKind, SymId,
};
use swamp_ir_shared::TypeId;
use swamp_semantic::{
    AnonymousStructLiteral, BinaryOperator, BinaryOperatorKind, Block, ExpressionKind, Postfix,
    PostfixKind, StartOfChain, StartOfChainKind, UnaryOperator, UnaryOperatorKind,
};
use swamp_symbol::{ScopedSymbolId, TopLevelSymbolId};
use swamp_types::prelude::{AnonymousStructType, TypeCache};
use swamp_types::{TypeKind, TypeRef};

pub struct BlockBuilder {
    statements: Vec<swamp_hir::Statement>,
}

impl BlockBuilder {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    /// Append `let <sym>: <ty> = <rhs>;` and return `Atom::Var(sym)`.
    pub fn push_let(
        &mut self,
        sym: swamp_hir::SymId,
        type_id: TypeId,
        rhs: swamp_hir::RValue,
        id: swamp_hir::NodeId,
    ) -> swamp_hir::Atom {
        self.statements.push(swamp_hir::Statement {
            kind: StatementKind::Let {
                name: sym,
                rhs,
                type_id,
            },
            node_id: id,
        });
        swamp_hir::Atom {
            kind: AtomKind::Var { sym },
            id,
        }
    }

    pub fn push_stmt(&mut self, stmt: swamp_hir::Statement) {
        self.statements.push(stmt);
    }

    pub fn into_vec(self) -> Vec<swamp_hir::Statement> {
        self.statements
    }
}

struct SymIdGenerator {
    pub last_id: u32,
}

impl SymIdGenerator {
    pub fn new() -> Self {
        Self { last_id: 0 }
    }
    pub fn next(&mut self) -> SymId {
        self.last_id += 1;
        SymId(self.last_id)
    }
}

struct NodeIdGenerator {
    pub last_id: u32,
}

impl NodeIdGenerator {
    pub fn new() -> Self {
        Self { last_id: 0 }
    }
    pub fn next(&mut self) -> NodeId {
        self.last_id += 1;
        NodeId(self.last_id)
    }
}

pub struct NodeIdToNode {
    pub lookup: SeqMap<NodeId, Node>,
    pub node_id_generator: NodeIdGenerator,
}

impl NodeIdToNode {
    pub fn new() -> Self {
        Self {
            lookup: SeqMap::default(),
            node_id_generator: NodeIdGenerator::new(),
        }
    }

    pub fn create_node_id(&mut self, node: &Node) -> NodeId {
        let node_id = self.node_id_generator.next();
        self.lookup
            .insert(node_id, node.clone())
            .expect("should work");
        node_id
    }
}

pub struct ScopedSymbolIdToSymId {
    pub lookup: SeqMap<ScopedSymbolId, SymId>,
    pub top_lookup: SeqMap<TopLevelSymbolId, SymId>,
    sym_gen: SymIdGenerator,
}

impl ScopedSymbolIdToSymId {
    pub fn new() -> Self {
        Self {
            lookup: SeqMap::default(),
            top_lookup: SeqMap::default(),
            sym_gen: SymIdGenerator::new(),
        }
    }

    pub fn get_or_create_sym_id(&mut self, symbol_id: ScopedSymbolId) -> SymId {
        if let Some(found) = self.lookup.get(&symbol_id) {
            found.clone()
        } else {
            let sym_id = self.sym_gen.next();
            _ = self.lookup.insert(symbol_id, sym_id);
            sym_id
        }
    }

    pub fn get_or_create_top_sym_id(&mut self, symbol_id: TopLevelSymbolId) -> SymId {
        if let Some(found) = self.top_lookup.get(&symbol_id) {
            found.clone()
        } else {
            let sym_id = self.sym_gen.next();
            _ = self.top_lookup.insert(symbol_id, sym_id);
            sym_id
        }
    }
}

pub struct LowerHir<'a> {
    node_gen: NodeIdToNode,
    scoped_symbol_id_to_sym_id: ScopedSymbolIdToSymId,
    type_cache: &'a TypeCache,
    // mappings: HashMap<SemaSymbolId, SymId>,
}

impl<'a> LowerHir<'a> {
    pub fn new(type_cache: &'a TypeCache) -> Self {
        Self {
            node_gen: NodeIdToNode::new(),
            scoped_symbol_id_to_sym_id: ScopedSymbolIdToSymId::new(),
            type_cache,
        }
    }
}

impl LowerHir<'_> {
    pub fn lower_block(&mut self, sema_blk: &Block) -> swamp_hir::Block {
        let mut bb = BlockBuilder::new();

        for s in &sema_blk.statements {
            self.lower_statement(s, &mut bb);
        }

        let tail_atom = self.lower_expression(&sema_blk.tail, &mut bb);

        swamp_hir::Block {
            statements: bb.into_vec(),
            tail: Box::new(tail_atom),
            node_id: self.node_gen.create_node_id(&sema_blk.tail.node),
        }
    }

    pub fn lower_block_expressions(
        &mut self,
        expressions: &Vec<swamp_semantic::Expression>,
    ) -> swamp_hir::Block {
        let mut bb = BlockBuilder::new();

        for s in expressions {
            self.lower_statement(s, &mut bb);
        }

        swamp_hir::Block {
            statements: bb.into_vec(),
            tail: Box::new(Expression::Atom(Atom {
                kind: AtomKind::LitBool { value: false },
                id: NodeId(0),
            })),
            node_id: NodeId(0),
        }
    }

    pub fn lower_statement(
        &mut self,
        expr: &swamp_semantic::Expression,
        block_builder: &mut BlockBuilder,
    ) {
        match &expr.kind {
            ExpressionKind::VariableDefinition(variable, expression) => {
                let node_id = self.node_gen.create_node_id(&variable.name);
                let sym_id = self
                    .scoped_symbol_id_to_sym_id
                    .get_or_create_sym_id(variable.symbol_id);
                let type_id = self.type_id_from_ref(&variable.resolved_type);
                let rhs_atom = self.lower_rvalue(expression, block_builder);
                block_builder.push_let(sym_id, type_id, rhs_atom, node_id);
                /*
                let statement_kind = StatementKind::Let {
                    name: sym_id,
                    type_id: TypeId(0),
                    rhs: rhs_atom,
                };

                Some(Statement {
                    kind: statement_kind,
                    node_id,
                })

                 */
            }

            ExpressionKind::Block(block) => {
                if let Some((last, statements)) = block.split_last() {
                    for x in statements {
                        self.lower_statement(x, block_builder);
                    }

                    self.lower_statement(last, block_builder)
                } else {
                    if block.is_empty() {
                    } else {
                        self.lower_statement(&block.first().unwrap(), block_builder)
                    }
                }
            }

            _ => {
                let converted_expr = self.lower_expression(&expr, block_builder);
                let kind = StatementKind::ExprStmt { rv: converted_expr };

                let node_id = self.node_gen.create_node_id(&expr.node);
                block_builder.push_stmt(Statement { kind, node_id })
            }
        }
    }

    pub fn lower_struct_literal(
        &mut self,
        anon_struct_type: &AnonymousStructType,
        struct_lit: &AnonymousStructLiteral,
        block_builder: &mut BlockBuilder,
    ) -> RValueKind {
        let field_count_in_struct_type = anon_struct_type.field_name_sorted_fields.len();
        let mut fields = vec![None; field_count_in_struct_type];

        for (index, _maybe_node, field_init_value_expression) in
            &struct_lit.source_order_expressions
        {
            let atom = self.lower_atom(&field_init_value_expression, block_builder);
            fields[*index] = Some(atom);
        }

        RValueKind::StructInit {
            ty: self.type_id_from_ref(&struct_lit.struct_like_type),
            fields,
        }
    }

    pub fn lower_rvalue(
        &mut self,
        expr: &swamp_semantic::Expression,
        block_builder: &mut BlockBuilder,
    ) -> swamp_hir::RValue {
        let node_id = self.node_gen.create_node_id(&expr.node);

        let kind = match &expr.kind {
            ExpressionKind::AnonymousStructLiteral(struct_lit) => {
                let TypeKind::AnonymousStruct(anon_struct_type) =
                    &*struct_lit.struct_like_type.kind
                else {
                    panic!("internal error, wrong anon type");
                };
                self.lower_struct_literal(anon_struct_type, struct_lit, block_builder)
            }
            ExpressionKind::NamedStructLiteral(struct_lit_expr) => {
                let ExpressionKind::AnonymousStructLiteral(struct_lit) = &struct_lit_expr.kind
                else {
                    panic!("internal error, wrong anon type");
                };
                let TypeKind::AnonymousStruct(anon_struct_type) =
                    &*struct_lit.struct_like_type.kind
                else {
                    panic!("internal error, wrong anon type");
                };

                self.lower_struct_literal(anon_struct_type, struct_lit, block_builder)
            }
            ExpressionKind::EnumVariantLiteral(_, _) => {
                todo!()
            }
            ExpressionKind::TupleLiteral(_) => {
                todo!()
            }
            _ => {
                let x = self.lower_atom(expr, block_builder);
                RValueKind::Mov { src: x }
            }
        };

        RValue { id: node_id, kind }
    }

    pub fn lower_atom(
        &mut self,
        expr: &swamp_semantic::Expression,
        block_builder: &mut BlockBuilder,
    ) -> swamp_hir::Atom {
        let node_id = self.node_gen.create_node_id(&expr.node);
        match &expr.kind {
            ExpressionKind::ConstantAccess(const_access) => {
                let sym_id = self
                    .scoped_symbol_id_to_sym_id
                    .get_or_create_top_sym_id(const_access.symbol_id);
                let node_id = self.node_gen.create_node_id(&const_access.name);
                Atom {
                    kind: AtomKind::Var { sym: sym_id },
                    id: node_id,
                }
            }
            ExpressionKind::VariableAccess(var_access) => {
                let sym_id = self
                    .scoped_symbol_id_to_sym_id
                    .get_or_create_sym_id(var_access.symbol_id);
                let node_id = self.node_gen.create_node_id(&var_access.name);
                Atom {
                    kind: AtomKind::Var { sym: sym_id },
                    id: node_id,
                }
            }
            ExpressionKind::BinaryOp(binary_operator) => {
                let rvalue = self.lower_binary_operator_to_rvalue(&binary_operator, block_builder);
                let let_variable_sym_id = self.scoped_symbol_id_to_sym_id.sym_gen.next();
                let type_id = TypeId(expr.ty.id.inner());
                let node_id = self.node_gen.create_node_id(&binary_operator.node);

                block_builder.push_let(let_variable_sym_id, type_id, rvalue, node_id);

                Atom {
                    kind: AtomKind::Var {
                        sym: let_variable_sym_id,
                    },
                    id: node_id,
                }
            }
            ExpressionKind::UnaryOp(unary_op) => {
                let rvalue = self.lower_unary_operator_to_rvalue(&unary_op, block_builder);
                let let_variable_sym_id = self.scoped_symbol_id_to_sym_id.sym_gen.next();
                let type_id = TypeId(expr.ty.id.inner());
                let node_id = self.node_gen.create_node_id(&unary_op.node);

                block_builder.push_let(let_variable_sym_id, type_id, rvalue, node_id);

                Atom {
                    kind: AtomKind::Var {
                        sym: let_variable_sym_id,
                    },
                    id: node_id,
                }
            }
            ExpressionKind::PostfixChain(start_of_chain, postfixes) => {
                self.lower_postfix_chain(start_of_chain, postfixes, block_builder)
            }
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::CoerceIntToChar(_) => todo!(),
            ExpressionKind::CoerceIntToByte(_) => todo!(),
            ExpressionKind::CoerceToAny(_) => todo!(),
            ExpressionKind::IntrinsicCallEx(_, _) => todo!(),
            ExpressionKind::InternalCall(_, _) => todo!(),
            ExpressionKind::HostCall(_, _) => todo!(),
            ExpressionKind::VariableDefinition(_, _) => todo!(),
            ExpressionKind::VariableDefinitionLValue(_, _) => todo!(),
            ExpressionKind::VariableReassignment(_, _) => todo!(),
            ExpressionKind::Assignment(_, _) => todo!(),
            ExpressionKind::CompoundAssignment(_, _, _) => todo!(),
            ExpressionKind::FloatLiteral(fixed_point) => Atom {
                kind: AtomKind::LitF32 {
                    value: fixed_point.inner(),
                },
                id: node_id,
            },
            ExpressionKind::NoneLiteral => Atom {
                kind: AtomKind::LitNone,
                id: node_id,
            },
            ExpressionKind::IntLiteral(int_lit) => Atom {
                kind: AtomKind::LitI32 { value: *int_lit },
                id: node_id,
            },
            ExpressionKind::ByteLiteral(byte_literal) => Atom {
                kind: AtomKind::LitU8 {
                    value: *byte_literal,
                },
                id: node_id,
            },
            ExpressionKind::BoolLiteral(bool_literal) => Atom {
                kind: AtomKind::LitBool {
                    value: *bool_literal,
                },
                id: node_id,
            },
            ExpressionKind::StringLiteral(str_literal) => Atom {
                kind: AtomKind::LitString {
                    value: str_literal.clone(),
                },
                id: node_id,
            },
            ExpressionKind::AnonymousStructLiteral(struct_lit) => {
                panic!("struct literal not atom")
            }
            ExpressionKind::NamedStructLiteral(_) => {
                panic!("named struct literal not atom")
            }
            ExpressionKind::EnumVariantLiteral(_, _) => {
                panic!("enum variant literal not atom")
            }
            ExpressionKind::TupleLiteral(_) => {
                panic!("tuple literal not atom")
            }
            ExpressionKind::InitializerList(_, _) => todo!(),
            ExpressionKind::InitializerPairList(_, _) => todo!(),
            ExpressionKind::Option(_) => todo!(),
            ExpressionKind::ForLoop(_, _, _) => todo!(),
            ExpressionKind::WhileLoop(_, _) => todo!(),
            ExpressionKind::Block(_) => todo!(),
            ExpressionKind::Match(_) => todo!(),
            ExpressionKind::Guard(_) => todo!(),
            ExpressionKind::If(_, _, _) => todo!(),
            ExpressionKind::When(_, _, _) => todo!(),
            ExpressionKind::TupleDestructuring(_, _, _) => todo!(),
            ExpressionKind::Lambda(_, _) => todo!(),
            ExpressionKind::BorrowMutRef(_) => todo!(),
            ExpressionKind::Error(_) => panic!("should not be lowering if there are errors"),
        }
    }

    pub fn lower_unary_operator_to_rvalue(
        &mut self,
        unary_operator: &UnaryOperator,
        block_builder: &mut BlockBuilder,
    ) -> swamp_hir::RValue {
        let left = self.lower_atom(&unary_operator.left, block_builder);
        let node_id = self.node_gen.create_node_id(&unary_operator.node);

        match &unary_operator.kind {
            UnaryOperatorKind::Not => RValue {
                kind: RValueKind::Not { x: left },
                id: node_id,
            },
            UnaryOperatorKind::Negate => RValue {
                kind: RValueKind::Neg { x: left },
                id: node_id,
            },
        }
    }

    pub fn lower_binary_operator_to_rvalue(
        &mut self,
        binary_operator: &BinaryOperator,
        block_builder: &mut BlockBuilder,
    ) -> swamp_hir::RValue {
        let left = self.lower_atom(&binary_operator.left, block_builder);
        let right = self.lower_atom(&binary_operator.right, block_builder);
        let node_id = self.node_gen.create_node_id(&binary_operator.node);

        let kind = match &binary_operator.kind {
            BinaryOperatorKind::Add => RValueKind::Add { a: left, b: right },
            BinaryOperatorKind::Subtract => RValueKind::Sub { a: left, b: right },
            BinaryOperatorKind::Multiply => RValueKind::Mul { a: left, b: right },
            BinaryOperatorKind::Divide => RValueKind::SDiv { a: left, b: right },
            BinaryOperatorKind::Modulo => RValueKind::SMod { a: left, b: right },
            BinaryOperatorKind::LogicalOr => RValueKind::Or { a: left, b: right },
            BinaryOperatorKind::LogicalAnd => RValueKind::And { a: left, b: right },
            BinaryOperatorKind::Equal => RValueKind::CmpEq { a: left, b: right },
            BinaryOperatorKind::NotEqual => RValueKind::CmpNe { a: left, b: right },
            BinaryOperatorKind::LessThan => RValueKind::CmpLt { a: left, b: right },
            BinaryOperatorKind::LessEqual => RValueKind::CmpLe { a: left, b: right },
            BinaryOperatorKind::GreaterThan => RValueKind::CmpGt { a: left, b: right },
            BinaryOperatorKind::GreaterEqual => RValueKind::CmpGe { a: left, b: right },
            BinaryOperatorKind::NoneCoalesce => RValueKind::NoneCoalesce { a: left, b: right },
        };

        RValue { kind, id: node_id }
    }

    pub fn lower_expression(
        &mut self,
        expr: &swamp_semantic::Expression,
        block_builder: &mut BlockBuilder,
    ) -> swamp_hir::Expression {
        match &expr.kind {
            ExpressionKind::PostfixChain(_, _) => todo!(),
            ExpressionKind::CoerceOptionToBool(_) => todo!(),
            ExpressionKind::CoerceIntToChar(_) => todo!(),
            ExpressionKind::CoerceIntToByte(_) => todo!(),
            ExpressionKind::CoerceToAny(_) => todo!(),
            ExpressionKind::IntrinsicCallEx(_, _) => todo!(),
            ExpressionKind::InternalCall(_, _) => todo!(),
            ExpressionKind::HostCall(_, _) => todo!(),
            ExpressionKind::VariableDefinitionLValue(_, _) => todo!(),
            ExpressionKind::VariableReassignment(_, _) => todo!(),
            ExpressionKind::Assignment(_, _) => todo!(),
            ExpressionKind::CompoundAssignment(_, _, _) => todo!(),
            ExpressionKind::AnonymousStructLiteral(_) => todo!(),
            ExpressionKind::NamedStructLiteral(_) => todo!(),
            ExpressionKind::FloatLiteral(_) => todo!(),
            ExpressionKind::NoneLiteral => todo!(),
            ExpressionKind::IntLiteral(_) => todo!(),
            ExpressionKind::ByteLiteral(_) => todo!(),
            ExpressionKind::StringLiteral(_) => todo!(),
            ExpressionKind::BoolLiteral(_) => todo!(),
            ExpressionKind::EnumVariantLiteral(_, _) => todo!(),
            ExpressionKind::TupleLiteral(_) => todo!(),
            ExpressionKind::InitializerList(_, _) => todo!(),
            ExpressionKind::InitializerPairList(_, _) => todo!(),
            ExpressionKind::Option(_) => todo!(),
            ExpressionKind::ForLoop(_, _, _) => todo!(),
            ExpressionKind::WhileLoop(_, _) => todo!(),
            ExpressionKind::Block(block) => {
                if let Some((last, statements)) = block.split_last() {
                    for x in statements {
                        self.lower_statement(x, block_builder);
                    }

                    self.lower_expression(last, block_builder)
                } else {
                    self.lower_expression(&block.first().unwrap(), block_builder)
                }
            }
            ExpressionKind::Match(_) => todo!(),
            ExpressionKind::Guard(_) => todo!(),
            ExpressionKind::If(condition, truth_expr, false_expr) => {
                let condition_atom = self.lower_atom(&condition.expression, block_builder);
                let truth_block = self.lower_block_expressions(&vec![*truth_expr.clone()]);
                let false_block =
                    self.lower_block_expressions(&vec![*false_expr.as_ref().unwrap().clone()]);

                Expression::IfExpr {
                    cond: condition_atom,
                    then_: truth_block,
                    else_: false_block,
                }
            }
            ExpressionKind::When(_, _, _) => todo!(),
            ExpressionKind::TupleDestructuring(_, _, _) => todo!(),
            ExpressionKind::Lambda(_, _) => todo!(),
            ExpressionKind::BorrowMutRef(_) => todo!(),
            ExpressionKind::Error(_) => todo!(),
            ExpressionKind::ConstantAccess(_) => todo!(),
            ExpressionKind::VariableAccess(_) => todo!(),
            ExpressionKind::BinaryOp(_) => todo!(),
            ExpressionKind::UnaryOp(_) => todo!(),
            ExpressionKind::VariableDefinition(_, _) => todo!(),
        }
    }

    fn lower_postfix_chain(
        &mut self,
        start: &StartOfChain,
        postfixes: &Vec<Postfix>,
        block_builder: &mut BlockBuilder,
    ) -> Atom {
        let node_id = self.node_gen.create_node_id(&start.node);
        let (atom, type_ref) = match &start.kind {
            StartOfChainKind::Expression(start_expression) => {
                let atom = self.lower_atom(start_expression, block_builder);
                (atom, start_expression.ty.clone())
            }
            StartOfChainKind::Variable(var) => {
                let sym_id = self
                    .scoped_symbol_id_to_sym_id
                    .get_or_create_sym_id(var.symbol_id);
                let atom = swamp_hir::Atom {
                    kind: swamp_hir::AtomKind::Var { sym: sym_id },
                    id: node_id,
                };
                (atom, var.resolved_type.clone())
            }
        };

        let mut cur_ty = TypeId(type_ref.id.inner());
        let let_variable_sym_id = self.scoped_symbol_id_to_sym_id.sym_gen.next();
        let start_rvalue = RValue {
            id: node_id,
            kind: RValueKind::Mov { src: atom },
        };

        let cur_atom =
            block_builder.push_let(let_variable_sym_id, cur_ty, start_rvalue.clone(), node_id);

        // start the cursor
        let mut cur_place = swamp_hir::Place {
            kind: PlaceKind::Var {
                // TODO: clean this up, so the match is not needed here
                sym: match cur_atom.kind {
                    swamp_hir::AtomKind::Var { sym, .. } => sym,
                    _ => let_variable_sym_id,
                },
            },
            id: node_id,
        };

        for postfix in postfixes {
            match &postfix.kind {
                PostfixKind::StructField(type_ref, field_index) => {
                    let TypeKind::AnonymousStruct(found_struct) = &*type_ref.kind else {
                        panic!("internal error anon struct hir");
                    };
                    let struct_type_field = found_struct
                        .field_name_sorted_fields
                        .values()
                        .collect::<Vec<_>>()[*field_index];
                    cur_place = swamp_hir::Place {
                        kind: PlaceKind::Field {
                            base: Box::new(cur_place),
                            name: self
                                .scoped_symbol_id_to_sym_id
                                .get_or_create_top_sym_id(struct_type_field.symbol_id),
                        },
                        id: self.node_gen.create_node_id(&postfix.node),
                    };
                    cur_ty = TypeId(type_ref.id.inner());
                }
                PostfixKind::MemberCall(function_ref, argument_expressions) => todo!(),
                PostfixKind::OptionalChainingOperator => todo!(),
                PostfixKind::VecSubscript(vector_type_ref, index_usize_expression) => {
                    let idx_atom = self.lower_atom(index_usize_expression, block_builder);
                    let node_id = self.node_gen.create_node_id(&index_usize_expression.node);

                    cur_place = Place {
                        kind: swamp_hir::PlaceKind::Index {
                            base: Box::new(cur_place.clone()),
                            index: idx_atom,
                        },
                        id: node_id,
                    };
                    cur_ty = TypeId(vector_type_ref.vec_type.id.inner());
                }
                PostfixKind::MapSubscript(map_type_ref, key_expression) => {
                    let key = self.lower_atom(key_expression, block_builder);
                    // get base map value to pass to runtime
                    let base_val = {
                        let s = self.scoped_symbol_id_to_sym_id.sym_gen.next();
                        block_builder.push_let(
                            s,
                            cur_ty,
                            RValue {
                                kind: swamp_hir::RValueKind::Use {
                                    place: cur_place.clone(),
                                },
                                id: node_id,
                            },
                            node_id,
                        )
                    };
                    // call runtime: MapGetAddr(map, key) -> *T
                    let ptr_ty = self.type_id_from_ref(&map_type_ref.value);
                    let addr_sym = self.scoped_symbol_id_to_sym_id.sym_gen.next();
                    let addr_atom = block_builder.push_let(
                        addr_sym,
                        ptr_ty,
                        RValue {
                            kind: RValueKind::Call {
                                target: HCallTarget::Runtime(RuntimeOp::MapGetAddr),
                                args: vec![base_val, key],
                            },
                            id: node_id,
                        },
                        node_id,
                    );
                    // Deref to quickly go back to a Place cursor
                    cur_place = Place {
                        kind: swamp_hir::PlaceKind::Deref { ptr: addr_atom },
                        id: node_id,
                    };
                    cur_ty = ptr_ty;
                }

                PostfixKind::VecSubscriptRange(vector_type, range_expression) => todo!(),
                PostfixKind::SparseSubscript(sparse_map_type, key_expression) => todo!(),

                PostfixKind::GridSubscript(grid_type_ref, x_expression, y_expression) => todo!(),
            }
        }

        // TODO: Hack
        Atom {
            kind: AtomKind::LitBool { value: false },
            id: NodeId(0),
        }
    }

    fn type_id_from_ref(&self, type_ref: &TypeRef) -> TypeId {
        TypeId(type_ref.id.inner())
    }
}
