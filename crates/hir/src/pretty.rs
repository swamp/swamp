use crate::{
    Atom, AtomKind, Block, Expression, Place, PlaceKind, RValue, RValueKind, Statement,
    StatementKind, SymId,
};
use ansi_pretty_print::{PrettyPrint, Printer};
use std::fmt::{Display, Formatter};

impl PrettyPrint for Block {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        p.block("", |printer| {
            for statement in &self.statements {
                printer.line("")?;
                statement.pretty(printer)?;
            }

            Ok(())
        })
    }
}

impl PrettyPrint for RValue {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        self.kind.pretty(p)
    }
}

impl PrettyPrint for RValueKind {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Self::Use { .. } => todo!(),
            Self::Mov { src } => {
                p.punctuation("mov(")?;
                src.pretty(p)?;
                p.punctuation(")")?;
            }
            Self::Neg { .. } => todo!(),
            Self::Not { .. } => todo!(),
            Self::Add { a, b } => {
                a.pretty(p)?;
                p.punctuation(" + ")?;
                b.pretty(p)?;
            }
            Self::Sub { .. } => todo!(),
            Self::Mul { .. } => todo!(),
            Self::SDiv { .. } => todo!(),
            Self::SMod { .. } => todo!(),
            Self::And { .. } => todo!(),
            Self::Or { .. } => todo!(),
            Self::Xor { .. } => todo!(),
            Self::Shl { .. } => todo!(),
            Self::LShr { .. } => todo!(),
            Self::AShr { .. } => todo!(),
            Self::CmpEq { .. } => todo!(),
            Self::CmpNe { .. } => todo!(),
            Self::CmpLt { .. } => todo!(),
            Self::CmpLe { .. } => todo!(),
            Self::CmpGt { a, b } => {
                a.pretty(p)?;
                p.punctuation(" > ")?;
                b.pretty(p)?;
            }
            Self::CmpGe { .. } => todo!(),
            Self::NoneCoalesce { .. } => todo!(),
            Self::AddrOf { .. } => todo!(),
            Self::Cast { .. } => todo!(),
            Self::Call { .. } => todo!(),
            Self::IfExpr { .. } => todo!(),
            Self::ArrayInit { .. } => todo!(),
            Self::StructInit { .. } => todo!(),
            Self::MapInit { .. } => todo!(),
        }
        Ok(())
    }
}

impl PrettyPrint for Statement {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match &self.kind {
            StatementKind::Let { name, type_id, rhs } => {
                p.symbol(&format!("{}", name))?;
                p.punctuation("=")?;

                rhs.pretty(p)
            }
            StatementKind::Assign { dst, src } => {
                dst.pretty(p)?;
                p.punctuation("<-")?;
                src.pretty(p)
            }
            StatementKind::While { .. } => todo!(),
            StatementKind::ForRange { .. } => todo!(),
            StatementKind::ForArray { .. } => todo!(),
            StatementKind::ForMap { .. } => todo!(),
            StatementKind::ExprStmt { rv } => rv.pretty(p),
        }
    }
}

impl PrettyPrint for Expression {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Expression::Atom(_) => todo!(),
            Expression::Use(_) => todo!(),
            Expression::IfExpr { cond, then_, else_ } => {
                p.punctuation("if(")?;
                p.with_indent(|inner_p| {
                    cond.pretty(inner_p)?;
                    then_.pretty(inner_p)?;
                    else_.pretty(inner_p)
                })?;

                p.punctuation(")")
            }
            Expression::Call { .. } => todo!(),
            Expression::MatchExpr { .. } => todo!(),
        }
    }
}

impl PrettyPrint for Place {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        self.kind.pretty(p)
    }
}

impl PrettyPrint for PlaceKind {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            PlaceKind::Var { sym } => {
                p.punctuation("var(")?;
                p.symbol(&format!("{}", sym.0))?;
                p.punctuation(")")?;
            }
            PlaceKind::Field { .. } => {}
            PlaceKind::Index { .. } => {}
            PlaceKind::Deref { .. } => {}
        }
        Ok(())
    }
}

impl PrettyPrint for Atom {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        self.kind.pretty(p)
    }
}

impl PrettyPrint for AtomKind {
    fn pretty(&self, p: &mut Printer) -> std::fmt::Result {
        match self {
            Self::Var { sym } => {
                p.punctuation("sym(")?;
                p.symbol(&format!("{sym}"))?;
                p.punctuation(")")
            }
            Self::LitI32 { value } => p.literal(&format!("{}", value)),
            Self::LitF32 { .. } => todo!(),
            Self::LitBool { .. } => todo!(),
            Self::LitNone => todo!(),
            Self::LitU8 { .. } => todo!(),
            Self::LitString { .. } => todo!(),
        }
    }
}

impl Display for SymId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "s{}", self.0)
    }
}
