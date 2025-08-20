use seq_map::SeqMap;
use swamp_hir::StatementKind::{Assign, ExprStmt, Let, While};
use swamp_hir::{Atom, AtomKind, Place, PlaceKind, SymId};
use swamp_ir_shared::{BlockId, FunctionId, TypeId, VReg};
use swamp_mir::{
    FnSig, Function, Inst, LoadTy, Operand, SlotKind, StackSlot, StackSlotId, Terminator, VRegInfo,
    ValueKind, Width,
};

pub struct LowerCtx {
    // output being built
    pub mir_func: swamp_mir::Function,

    // allocators
    next_vreg: u32,
    next_block: u32,

    // symbol -> stack slot (one per source local/param)
    sym_to_slot: SeqMap<SymId, StackSlotId>, // maybe FxHashMap in the future?

    // type & layout helpers (if you have them)
    ty_i32: TypeId, // convenience
}

impl LowerCtx {
    pub fn new() -> Self {
        Self {
            mir_func: Function {
                id: FunctionId(0),
                sig: FnSig {
                    parameters: vec![],
                    ret: None,
                },
                entry: BlockId(0),
                blocks: vec![],
                vregs: vec![],
                slots: vec![],
                stack_size: 0,
                slot_offsets: vec![],
            },
            next_vreg: 0,
            next_block: 0,
            sym_to_slot: Default::default(),
            ty_i32: TypeId(0),
        }
    }

    fn new_vreg(&mut self, kind: ValueKind) -> VReg {
        let v = VReg(self.next_vreg);
        self.next_vreg += 1;
        self.mir_func.vregs.push(VRegInfo { kind });
        v
    }
    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.mir_func.blocks.push(swamp_mir::Block {
            id,
            instructions: vec![],
            terminator: Terminator::Hlt,
        });
        id
    }

    fn cur_block_mut(&mut self, id: BlockId) -> &mut swamp_mir::Block {
        self.mir_func
            .blocks
            .iter_mut()
            .find(|b| b.id == id)
            .unwrap()
    }

    fn ensure_slot_for_sym(&mut self, sym: SymId, ty: TypeId) -> StackSlotId {
        if let Some(x) = self.sym_to_slot.get(&sym) {
            x.clone()
        } else {
            let size = 4;
            let align = 4; // stub; use real layout
            let id = StackSlotId(self.mir_func.slots.len() as u32);
            self.mir_func.slots.push(StackSlot {
                ty,
                size,
                align,
                kind: SlotKind::Local,
            });
            id
        }
    }

    #[inline]
    pub fn new_imm_i32(&mut self, bb: BlockId, k: i32) -> VReg {
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(Inst::Mov {
            dst,
            src: Operand::ImmI32(k),
        });
        dst
    }

    #[inline]
    pub fn new_imm_u32(&mut self, bb: BlockId, k: u32) -> VReg {
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(Inst::Mov {
            dst,
            src: Operand::ImmU32(k),
        });
        dst
    }

    /// Load a source-level variable from its stack slot into a fresh vreg.
    pub fn load_var_i32(&mut self, bb: BlockId, sym: SymId, ty: TypeId) -> VReg {
        let _slot = self.ensure_slot_for_sym(sym, ty);
        // If you model slots as (base, offset), fill that later in frame layout.
        // For now we emit a symbolic Load; base/offset patched post layout.
        let base = self.new_vreg(ValueKind::Ptr(ty));
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(Inst::Load {
            dst,
            base,
            offset: 0,
            ty: LoadTy::I32,
        });
        dst
    }

    /// Turn an Atom into an Operand, folding small literals directly.
    pub fn atom_to_operand(&mut self, bb: BlockId, a: &Atom) -> Operand {
        match a.kind {
            AtomKind::LitI32 { value } => Operand::ImmI32(value),
            AtomKind::LitU8 { value } => Operand::ImmU32(value as u32),
            AtomKind::LitBool { value } => Operand::ImmU32(if value { 1 } else { 0 }),
            _ => Operand::Reg(self.atom_to_vreg(bb, a)),
        }
    }

    /// Turn an Atom into a VReg (loads vars, materializes immediates when needed).
    pub fn atom_to_vreg(&mut self, bb: BlockId, a: &Atom) -> VReg {
        match a.kind {
            AtomKind::Var { sym } => self.load_var_i32(bb, sym, self.ty_i32),
            AtomKind::LitI32 { value } => self.new_imm_i32(bb, value),
            AtomKind::LitU8 { value } => self.new_imm_u32(bb, value as u32),
            AtomKind::LitBool { value } => self.new_imm_u32(bb, u32::from(value)),
            AtomKind::LitNone | AtomKind::LitF32 { .. } | AtomKind::LitString { .. } => {
                unimplemented!("atom_to_vreg: {:?}", a.kind)
            }
        }
    }

    /// Address of a Place (pointer vreg). Keep it simple; refine once you have layout.
    pub fn place_addr(&mut self, bb: BlockId, p: &Place) -> VReg {
        match &p.kind {
            PlaceKind::Var { sym } => {
                let _slot = self.ensure_slot_for_sym(*sym, self.ty_i32);
                self.new_vreg(ValueKind::Ptr(self.ty_i32)) // symbolic; patched later
            }
            PlaceKind::Field { base, .. } => {
                let base_ptr = self.place_addr(bb, base);
                let dst = self.new_vreg(ValueKind::Ptr(self.ty_i32));
                self.cur_block_mut(bb).instructions.push(Inst::AddPtrImm {
                    dst,
                    base: base_ptr,
                    offset: 0, // fill actual offset after layout
                });
                dst
            }
            PlaceKind::Index { base, index } => {
                let base_ptr = self.place_addr(bb, base);
                let idx = self.atom_to_vreg(bb, index);
                // TODO: scale by elem size. For now, leave as unimplemented or emit shl+add.
                let _ = idx;
                unimplemented!("place_addr(Index)")
            }
            PlaceKind::Deref { ptr } => self.atom_to_vreg(bb, ptr),
        }
    }

    /// Load/store helpers for i32 places.
    pub fn load_place_i32(&mut self, bb: BlockId, p: &Place) -> VReg {
        let ptr = self.place_addr(bb, p);
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(Inst::Load {
            dst,
            base: ptr,
            offset: 0,
            ty: LoadTy::I32,
        });
        dst
    }
    pub fn store_place_i32(&mut self, bb: BlockId, p: &Place, src: VReg) {
        let ptr = self.place_addr(bb, p);
        self.cur_block_mut(bb).instructions.push(Inst::Store {
            base: ptr,
            offset: 0,
            src: Operand::Reg(src),
            width: Width::Word,
        });
    }

    pub fn lower_fn_body(&mut self, hir_body: &swamp_hir::Block, ret_ty: Option<TypeId>) {
        let entry = self.new_block();
        self.mir_func.entry = entry;

        let (_end_bb, maybe_val) = self.lower_block_value(entry, hir_body);

        // Return if the function expects a value; otherwise return void.
        let term = match (ret_ty, maybe_val) {
            (Some(_), Some(v)) => Terminator::Ret { value: Some(v) },
            (Some(_), None) => Terminator::Hlt, // or diagnose: missing value in tail position
            (None, _) => Terminator::Ret { value: None },
        };
        self.cur_block_mut(_end_bb).terminator = term;
    }

    pub fn lower_statement(&mut self, bb: BlockId, s: &swamp_hir::Statement) -> BlockId {
        use swamp_hir::StatementKind::*;
        match &s.kind {
            Let { name, type_id, rhs } => {
                let v = self.lower_rvalue(bb, rhs);
                let p = swamp_hir::Place {
                    id: s.node_id,
                    kind: swamp_hir::PlaceKind::Var { sym: *name },
                };
                self.store_place_i32(bb, &p, v);
                bb
            }
            Assign { dst, src } => {
                let v = self.atom_to_vreg(bb, src);
                self.store_place_i32(bb, dst, v);
                bb
            }
            ExprStmt { rv } => {
                let _ = self.lower_expression(bb, rv);
                bb
            }
            While { cond, body } => {
                //   br head
                // head: if cond br body else br exit
                // body: ...; br head
                let head = self.new_block();
                let body_bb = self.new_block();
                let exit = self.new_block();

                self.cur_block_mut(bb).terminator = Terminator::Br { target: head };

                let vcond = self.atom_to_vreg(head, cond);
                self.cur_block_mut(head).terminator = Terminator::BrNZ {
                    v: vcond,
                    target: body_bb,
                };

                let body_end = self.lower_block(body_bb, body);
                self.cur_block_mut(body_end).terminator = Terminator::Br { target: head };

                exit
            }
            ForRange {
                ivar,
                start,
                end,
                kind,
                body,
                ..
            } => {
                // i = start; while (i </<= end) { body; i += 1; }
                let slot = self.ensure_slot_for_sym(ivar.name, ivar.ty);
                let _ = slot;

                let vstart = self.atom_to_vreg(bb, start);
                let ip = swamp_hir::Place {
                    id: s.node_id,
                    kind: swamp_hir::PlaceKind::Var { sym: ivar.name },
                };
                self.store_place_i32(bb, &ip, vstart);

                let head = self.new_block();
                let loop_bb = self.new_block();
                let latch = self.new_block();
                let exit = self.new_block();

                self.cur_block_mut(bb).terminator = Terminator::Br { target: head };

                // head
                let vi = self.load_place_i32(head, &ip);
                let vend = self.atom_to_vreg(head, end);
                let vcmp = self.new_vreg(ValueKind::Bool);
                match kind {
                    swamp_hir::RangeKind::HalfOpen => {
                        self.cur_block_mut(head).instructions.push(Inst::CmpSLt {
                            dst: vcmp,
                            a: Operand::Reg(vi),
                            b: Operand::Reg(vend),
                        })
                    }
                    swamp_hir::RangeKind::Closed => {
                        self.cur_block_mut(head).instructions.push(Inst::CmpSLe {
                            dst: vcmp,
                            a: Operand::Reg(vi),
                            b: Operand::Reg(vend),
                        })
                    }
                }
                self.cur_block_mut(head).terminator = Terminator::BrNZ {
                    v: vcmp,
                    target: loop_bb,
                };

                // body
                let end_bb = self.lower_block(loop_bb, body);
                self.cur_block_mut(end_bb).terminator = Terminator::Br { target: latch };

                // latch
                let vi2 = self.load_place_i32(latch, &ip);
                let one = self.new_imm_i32(latch, 1);
                let vnext = self.new_vreg(ValueKind::I32);
                self.cur_block_mut(latch).instructions.push(Inst::Add {
                    dst: vnext,
                    a: Operand::Reg(vi2),
                    b: Operand::Reg(one),
                });
                self.store_place_i32(latch, &ip, vnext);
                self.cur_block_mut(latch).terminator = Terminator::Br { target: head };

                exit
            }
            ForArray { .. } | ForMap { .. } => {
                // Defer to runtime iterator protocol or specialized lowering later.
                unimplemented!("ForArray/ForMap")
            }
        }
    }

    /// Lower a block that you know you will discard the tail value for (used in loops).
    fn lower_block(&mut self, bb: BlockId, blk: &swamp_hir::Block) -> BlockId {
        let (end, _maybe) = self.lower_block_value(bb, blk);
        end
    }
    pub fn lower_block_value(
        &mut self,
        start_bb: BlockId,
        blk: &swamp_hir::Block,
    ) -> (BlockId, Option<VReg>) {
        let mut bb = start_bb;

        for stmt in &blk.statements {
            bb = self.lower_statement(bb, stmt);
        }

        // Tail expression: may or may not yield a value depending on your usage.
        let v = self.lower_expression(bb, &blk.tail);
        (bb, v)
    }

    pub fn lower_rvalue(&mut self, bb: BlockId, rv: &swamp_hir::RValue) -> VReg {
        use swamp_hir::RValueKind::*;
        match &rv.kind {
            Use { place } => self.load_place_i32(bb, place),
            Mov { src } => self.atom_to_vreg(bb, src),

            Add { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::Add { dst, a, b }),
            Sub { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::Sub { dst, a, b }),
            Mul { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::Mul { dst, a, b }),
            SDiv { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::SDiv { dst, a, b }),
            SMod { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::SMod { dst, a, b }),

            And { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::And { dst, a, b }),
            Or { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::Or { dst, a, b }),
            Xor { a, b } => self.bin_ii(bb, a, b, |dst, a, b| Inst::Xor { dst, a, b }),

            Shl { a, b } => self.shift(bb, a, b, |dst, a, b| Inst::Shl {
                dst,
                a,
                b_or_imm: b,
            }),
            LShr { a, b } => self.shift(bb, a, b, |dst, a, b| Inst::LShr {
                dst,
                a,
                b_or_imm: b,
            }),
            AShr { a, b } => self.shift(bb, a, b, |dst, a, b| Inst::AShr {
                dst,
                a,
                b_or_imm: b,
            }),

            CmpEq { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpEq { dst, a, b }),
            CmpNe { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpNe { dst, a, b }),
            CmpLt { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpSLt { dst, a, b }),
            CmpLe { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpSLe { dst, a, b }),
            CmpGt { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpSGt { dst, a, b }),
            CmpGe { a, b } => self.cmp(bb, a, b, |dst, a, b| Inst::CmpSGe { dst, a, b }),

            AddrOf { place } => self.place_addr(bb, place),

            Cast { expr, .. } => self.atom_to_vreg(bb, expr),

            IfExpr { cond, then_, else_ } => {
                let vcond = self.atom_to_vreg(bb, cond);
                let b_then = self.new_block();
                let b_else = self.new_block();
                let b_join = self.new_block();

                // Split control from current bb:
                self.cur_block_mut(bb).terminator = Terminator::BrNZ {
                    v: vcond,
                    target: b_then,
                };
                // “else” is reachable by fallthrough via a Br to b_else
                // (or you can emit BrZ from bb instead; either is fine)

                // Then branch
                let v_then = {
                    let (end, v) = self.lower_block_value(b_then, then_);
                    self.cur_block_mut(end).terminator = Terminator::Br { target: b_join };
                    v.expect("then_ must yield value in IfExpr")
                };

                // Else branch
                let v_else = {
                    let (end, v) = self.lower_block_value(b_else, else_);
                    self.cur_block_mut(end).terminator = Terminator::Br { target: b_join };
                    v.expect("else_ must yield value in IfExpr")
                };

                // Join value via Select (keeps MIR simple)
                let v_out = self.new_vreg(ValueKind::I32);
                self.cur_block_mut(b_join).instructions.push(Inst::Select {
                    dst: v_out,
                    cond: vcond,
                    if_true: v_then,
                    if_false: v_else,
                });
                v_out
            }

            ArrayInit { .. }
            | StructInit { .. }
            | MapInit { .. }
            | NoneCoalesce { .. }
            | Call { .. } => unimplemented!("lower_rvalue: {:?}", rv.kind),
            Neg { x } => {
                let vx = self.atom_to_vreg(bb, x);
                let zero = self.new_imm_i32(bb, 0);
                let dst = self.new_vreg(ValueKind::I32);
                self.cur_block_mut(bb).instructions.push(Inst::Sub {
                    dst,
                    a: Operand::Reg(zero),
                    b: Operand::Reg(vx),
                });
                dst
            }
            Not { x } => {
                let vx = self.atom_to_vreg(bb, x);
                let dst = self.new_vreg(ValueKind::I32);
                self.cur_block_mut(bb)
                    .instructions
                    .push(Inst::Not { dst, a: vx });
                dst
            }
        }
    }

    pub fn lower_expression(&mut self, bb: BlockId, e: &swamp_hir::Expression) -> Option<VReg> {
        use swamp_hir::Expression::*;
        Some(match e {
            Atom(a) => self.atom_to_vreg(bb, a),
            Use(p) => self.load_place_i32(bb, p),

            // delegate to the RValue lowering where shapes match:
            IfExpr { cond, then_, else_ } => {
                let fake = swamp_hir::RValue {
                    id: swamp_hir::NodeId::default(),
                    kind: swamp_hir::RValueKind::IfExpr {
                        cond: cond.clone(),
                        then_: then_.clone(),
                        else_: else_.clone(),
                    },
                };
                self.lower_rvalue(bb, &fake)
            }

            Call { .. } => unimplemented!("Expression::Call"),
            MatchExpr { .. } => unimplemented!("Expression::MatchExpr"),
        })
    }

    #[inline]
    pub fn bin_ii<F>(&mut self, bb: BlockId, a: &swamp_hir::Atom, b: &swamp_hir::Atom, f: F) -> VReg
    where
        F: Fn(VReg, Operand, Operand) -> Inst,
    {
        let oa = self.atom_to_operand(bb, a);
        let ob = self.atom_to_operand(bb, b);
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(f(dst, oa, ob));
        dst
    }
    #[inline]
    pub fn shift<F>(&mut self, bb: BlockId, a: &swamp_hir::Atom, b: &swamp_hir::Atom, f: F) -> VReg
    where
        F: Fn(VReg, VReg, Operand) -> Inst,
    {
        let va = self.atom_to_vreg(bb, a);
        let ob = self.atom_to_operand(bb, b);
        let dst = self.new_vreg(ValueKind::I32);
        self.cur_block_mut(bb).instructions.push(f(dst, va, ob));
        dst
    }
    #[inline]
    pub fn cmp<F>(&mut self, bb: BlockId, a: &swamp_hir::Atom, b: &swamp_hir::Atom, f: F) -> VReg
    where
        F: Fn(VReg, Operand, Operand) -> Inst,
    {
        let oa = self.atom_to_operand(bb, a);
        let ob = self.atom_to_operand(bb, b);
        let dst = self.new_vreg(ValueKind::Bool);
        self.cur_block_mut(bb).instructions.push(f(dst, oa, ob));
        dst
    }
}
