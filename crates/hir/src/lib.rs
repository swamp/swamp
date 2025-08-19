use swamp_lir::FunctionId;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SymId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct TypeId(pub u32);

#[cfg(feature = "debug-info")]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct NodeId(pub u32);

#[cfg(not(feature = "debug-info"))]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct NodeId; // zero-sized in non-debug builds


#[derive(Debug)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub id: FunctionId,
    pub name: SymId,
    pub params: Vec<Param>,
    pub ret: Option<TypeId>,
    pub body: Block,
}

#[derive(Debug)]
pub struct Param {
    pub name: SymId,
    pub ty: TypeId,
}


#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Box<Expression>,
    pub node_id: NodeId,
}

#[derive(Clone, Debug)]
pub enum RuntimeOp {
    MapGetAddr,

}

#[derive(Clone, Debug)]
pub struct ExternalFnId(u32);

#[derive(Clone, Debug)]
pub enum HCallTarget {
    Function(FunctionId),
    Runtime(RuntimeOp),
    Extern(ExternalFnId, SymId),
    Indirect(Atom),
}

#[derive(Debug, Clone)]
pub struct RValue {
    pub id: NodeId,
    pub kind: RValueKind,
}

#[derive(Clone, Debug)]
pub enum RValueKind {
    // Loads / moves
    Use { place: Place },               // read an lvalue as a value
    Mov { src: Atom },                  // just forward an atom (often optimized away)

    Neg { x: Atom },                    // arithmetic negate
    Not { x: Atom },                    // bitwise/logical not (typed)

    // Binary ops (operands are Atoms; signedness/width come from types)

    // Arithmetic
    Add { a: Atom, b: Atom },
    Sub { a: Atom, b: Atom },
    Mul { a: Atom, b: Atom },
    SDiv { a: Atom, b: Atom },
    SMod { a: Atom, b: Atom },

    // Logical
    And { a: Atom, b: Atom },
    Or { a: Atom, b: Atom },

    // Bit
    Xor { a: Atom, b: Atom },
    Shl { a: Atom, b: Atom },           // shift amount is Atom (usually small imm)
    LShr { a: Atom, b: Atom },           // logical right shift
    AShr { a: Atom, b: Atom },           // arithmetic right shift

    // Comparisons (result is Bool)
    CmpEq { a: Atom, b: Atom },
    CmpNe { a: Atom, b: Atom },
    CmpLt { a: Atom, b: Atom },
    CmpLe { a: Atom, b: Atom },
    CmpGt { a: Atom, b: Atom },
    CmpGe { a: Atom, b: Atom },

    // Conversion
    NoneCoalesce { a: Atom, b: Atom },

    // Addressing / casts (no memory effects here)
    AddrOf { place: Place },
    Cast { expr: Atom, to: TypeId },     // implicit casts should be explicit

    // Calls (may have effects)
    Call { target: HCallTarget, args: Vec<Atom> },

    // If as an expression (both branches same type; only one executes)
    IfExpr { cond: Atom, then_: Block, else_: Block },

    ArrayInit {
        elem_ty: TypeId,
        elems: Vec<Atom>,
    },

    StructInit {
        ty: TypeId,
        fields: Vec<(SymId, Atom)>,
    },

    MapInit {
        key_ty: TypeId,
        val_ty: TypeId,
        entries: Vec<(Atom, Atom)>,
    },
}

// Handy loop bindings
#[derive(Copy, Clone, Debug)]
pub struct Bind {
    pub name: SymId,
    pub ty: TypeId,
    pub mutable: bool, // true if the loop var is declared `mut`
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum RangeKind { HalfOpen, Closed }

#[derive(Debug, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub node_id: NodeId,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Let { name: SymId, type_id: TypeId, rhs: RValue },
    Assign { dst: Place, src: Atom },      // single store
    While { cond: Atom, body: Block },    // no break/return in Swamp

    /// Numeric ranges: for i in start .. end //[step k]
    ForRange {
        ivar: Bind,                 // loop variable (usually u32)
        start: Atom,                // inclusive start
        end: Atom,                // end bound (see `kind`)
        //step: Option<Atom>,
        kind: RangeKind,           // HalfOpen (..), Closed (..=) // inclusive exclusive
        body: Block,
        id: NodeId,
    },

    /// Arrays/slices/vectors: for (idx?, val) in array
    ForArray {
        idx: Option<Bind>,        // Some(i) to bind index; None if you don't need it
        val: Bind,                // value binding (by value or by ref; see `val_by_ref`)
        array: Atom,                // the array/slice value (already evaluated)
        val_by_ref: bool,           // true => bind &elem instead of copying elem
        body: Block,
        id: NodeId,
    },

    /// Maps/dicts: for (key, val) in map
    /// Lowered to runtime iterator protocol (or specialized bucket walk).
    ForMap {
        key: Option<Bind>,
        val: Bind,                // value binding
        map: Atom,                // the map value (already evaluated)
        key_by_ref: bool,           // bind &key?
        val_by_ref: bool,           // bind &val?
        body: Block,
        id: NodeId,
    },

    ExprStmt { rv: RValue },               // side-effect-only expression
}

#[derive(Clone, Debug)]
pub struct MatchArm {
    pub pat: Pattern,           // pattern to test
    pub guard: Option<Atom>,    // optional guard expression (Atom)
    pub body: Block,            // body that yields the arm's value
    pub id: NodeId,
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub id: NodeId,
    pub kind: PatternKind,
}

#[derive(Clone, Debug)]
pub enum PatternKind {
    Wild { id: NodeId },                      // _
    LitI32 { value: i32, id: NodeId },        // 0, 1, 2, ...
    Or { alts: Vec<Pattern>, id: NodeId },    // p1 | p2 | ...
    // (future: range, enum, struct, by-ref, bindings, etc.)
}

#[derive(Debug, Clone)]
pub enum Expression {
    Atom(Atom),                                      // Var / literal
    Use(Place),                                      // read a lvalue
    IfExpr { cond: Atom, then_: Block, else_: Block }, // if is an expression
    Call { callee: Atom, args: Vec<Atom> },          // ANF: args are atoms

    MatchExpr { scrutinee: Atom, arms: Vec<MatchArm>, id: NodeId },
}

#[derive(Clone, Debug)]
pub struct Place {
    pub id: NodeId,
    pub kind: PlaceKind,
}

#[derive(Clone, Debug)]
pub enum PlaceKind {
    Var { sym: SymId },
    Field { base: Box<Place>, name: SymId },
    Index { base: Box<Place>, index: Atom },
    Deref { ptr: Atom }, // *ptr
}

#[derive(Clone, Debug)]
pub struct Atom {
    pub kind: AtomKind,
    pub id: NodeId,
}

#[derive(Clone, Debug)]
pub enum AtomKind {
    Var { sym: SymId },
    LitI32 { value: i32 },
    LitF32 { value: i32 },
    LitBool { value: bool },
}



/*

TODO:

fn sum_pos(arr: [Int]) -> Int {
  mut s: Int = 0

  for v in arr {
    s = if v > 0 { s + v } else { s }
  }

  s
}


should be lowered to:

let s: i32 = LitI32(0)

ForArray {
  idx: None, val: Bind(v:i32), val_by_ref: false,
  array: Var(arr),
  body: {
    let c  = CmpGt { a: Var(v), b: LitI32(0) }
    let t  = Add   { a: Var(s), b: Var(v) }
    let e  = Mov   { src: Var(s) }
    let nv = IfExpr { cond: Var(c), then_: Block{ tail: Mov(t) }, else_: Block{ tail: Mov(e) } }
    s = Mov(Var(nv))
    tail: Mov(Var(s))
  }
}

tail: Mov(Var(s))

*/


/*


fn bucket(x: Int) -> Int {
  match x {
    0        => 100,
    1 | 2    => 200,
    n if n < 0 => -1,
    _        => 300,
  }
}


Function {
  name: SymId("bucket"), params: [Param{x: i32}], ret: Some(i32),
  body: Block {
    statements: [],
    tail: Box::new(Expression::MatchExpr {
      scrutinee: Atom::Var { sym: SymId("x"), id },
      arms: vec![
        MatchArm {
          pat: Pattern::LitI32 { value: 0, id },
          guard: None,
          body: Block { statements: [], tail: Box::new(Expression::Atom(Atom::LitI32 { value: 100, id })), node_id: id },
          id
        },
        MatchArm {
          pat: Pattern::Or { alts: vec![
            Pattern::LitI32 { value: 1, id },
            Pattern::LitI32 { value: 2, id },
          ], id },
          guard: None,
          body: Block { statements: [], tail: Box::new(Expression::Atom(Atom::LitI32 { value: 200, id })), node_id: id },
          id
        },
        MatchArm {
          pat: Pattern::Wild { id },
          guard: Some(Atom::CmpLtLike? ),
          body: Block { statements: [], tail: Box::new(Expression::Atom(Atom::LitI32 { value: -1, id })), node_id: id },
          id
        },
        MatchArm {
          pat: Pattern::Wild { id },
          guard: None,
          body: Block { statements: [], tail: Box::new(Expression::Atom(Atom::LitI32 { value: 300, id })), node_id: id },
          id
        },
      ],
      id
    }),
    node_id: id
  }
}

bb0(entry):
    // tmp_neg = (x < 0)
    tmp_neg = CmpLt { a: x, b: 0 }
    if tmp_neg { goto bb_neg } else { goto bb_sw }

bb_neg:
    ret = -1
    goto bb_ret

bb_sw:
    // Switch on x: 0->bb0v, 1->bb12, 2->bb12, default->bb_def
    switch x {
      0: bb0v
      1: bb12
      2: bb12
      _ : bb_def
    }

bb0v:
    ret = 100
    goto bb_ret

bb12:
    ret = 200
    goto bb_ret

bb_def:
    ret = 300
    goto bb_ret

bb_ret:
    RETURN ret
 */
