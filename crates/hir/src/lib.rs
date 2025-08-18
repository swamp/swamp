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

#[derive(Clone, Debug)]
pub enum RValue {
    // Loads / moves
    Use { place: Place, id: NodeId },               // read an lvalue as a value
    Mov { src: Atom, id: NodeId },                  // just forward an atom (often optimized away)

    Neg { x: Atom, id: NodeId },                    // arithmetic negate
    Not { x: Atom, id: NodeId },                    // bitwise/logical not (typed)

    // Binary ops (operands are Atoms; signedness/width come from types)

    // Arithmetic
    Add { a: Atom, b: Atom, id: NodeId },
    Sub { a: Atom, b: Atom, id: NodeId },
    Mul { a: Atom, b: Atom, id: NodeId },
    SDiv { a: Atom, b: Atom, id: NodeId },
    SMod { a: Atom, b: Atom, id: NodeId },

    // Logical
    And { a: Atom, b: Atom, id: NodeId },
    Or { a: Atom, b: Atom, id: NodeId },

    // Bit
    Xor { a: Atom, b: Atom, id: NodeId },
    Shl { a: Atom, b: Atom, id: NodeId },           // shift amount is Atom (usually small imm)
    LShr { a: Atom, b: Atom, id: NodeId },           // logical right shift
    AShr { a: Atom, b: Atom, id: NodeId },           // arithmetic right shift

    // Comparisons (result is Bool)
    CmpEq { a: Atom, b: Atom, id: NodeId },
    CmpNe { a: Atom, b: Atom, id: NodeId },
    CmpLt { a: Atom, b: Atom, id: NodeId },
    CmpLe { a: Atom, b: Atom, id: NodeId },
    CmpGt { a: Atom, b: Atom, id: NodeId },
    CmpGe { a: Atom, b: Atom, id: NodeId },

    // Addressing / casts (no memory effects here)
    AddrOf { place: Place, id: NodeId },
    Cast { expr: Atom, to: TypeId, id: NodeId },     // implicit casts should be explicit

    // Calls (may have effects)
    Call { target: HCallTarget, args: Vec<Atom>, id: NodeId },

    // If as an expression (both branches same type; only one executes)
    IfExpr { cond: Atom, then_: Block, else_: Block, id: NodeId },
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
pub enum Statement {
    Let { name: SymId, ty: TypeId, rhs: RValue, id: NodeId },
    Assign { dst: Place, src: Atom, id: NodeId },      // single store
    While { cond: Atom, body: Block, id: NodeId },    // no break/return in Swamp

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

    ExprStmt { rv: RValue, id: NodeId },               // side-effect-only expression
}

#[derive(Debug, Clone)]
pub enum Expression {
    Atom(Atom),                                      // Var / literal
    Use(Place),                                      // read a lvalue
    IfExpr { cond: Atom, then_: Block, else_: Block }, // if is an expression
    Call { callee: Atom, args: Vec<Atom> },          // ANF: args are atoms
}

#[derive(Clone, Debug)]
pub enum Place {
    Var { sym: SymId, id: NodeId },
    Field { base: Box<Place>, name: SymId, id: NodeId },
    Index { base: Box<Place>, index: Atom, id: NodeId },
    Deref { ptr: Atom, id: NodeId }, // *ptr
}

#[derive(Clone, Debug)]
pub enum Atom {
    Var { sym: SymId, id: NodeId },
    LitI32 { value: i32, id: NodeId },
    LitF32 { value: i32, id: NodeId },
    LitBool { value: bool, id: NodeId },
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
