use std::{collections::{HashMap, VecDeque}, fmt::Write as _};

use interop::{CDump, Size};

use templated::templated;

use crate::{rel, sym};

mod sigs;

impl interop::CRead<sym::SymAddr> for ppcdis::Instruction<sym::SymAddr> {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: interop::CReader<sym::SymAddr> + ?Sized
    {
        let mut reader2 = reader.clone();

        let insn32 = match reader.read_rel_u32()? {
            rel::Symbol::Value(v) => Some(ppcdis::RawInsn::Concrete(v)),
            rel::Symbol::Partial => None,
            rel::Symbol::Unknown => None,
            rel::Symbol::Rel(r) => Some(ppcdis::RawInsn::Rel {
                sym: r.get_address(),
                rtype: r.rtype,
                value: r.orig.map(u32::from).unwrap_or_default(),
            }),
        };

        let raw = match insn32 {
            Some(v) => v,
            None => {
                let upper16 = (reader2.read_u16()? as u32) << 16;
                match reader2.read_rel_u16()? {
                    rel::Symbol::Value(v) => {
                        ppcdis::RawInsn::Concrete(v as u32 | upper16)
                    }
                    rel::Symbol::Rel(r) => {
                        use std::ops::BitOr;
                        ppcdis::RawInsn::Rel {
                            sym: r.get_address(),
                            rtype: r.rtype,
                            value: r.orig
                                .map(u32::from)
                                .unwrap_or_default()
                                .bitor(upper16)
                        }
                    }
                    rel::Symbol::Partial
                    | rel::Symbol::Unknown => {
                        return Err(reader.error_complex_symbol());
                    }
                }
            }
        };

        ppcdis::Instruction::parse(&raw)
            .map_err(|e| {
                let (bt, e) = e.unwrap();
                reader.error_custom_with(format!("{e}"), bt)
            })
    }
}

#[derive(Debug)]
pub struct Code {
    insns: Vec<ppcdis::Instruction<sym::SymAddr>>,
}

impl IntoIterator for Code {
    type Item = ppcdis::Instruction<sym::SymAddr>;

    type IntoIter = std::vec::IntoIter<ppcdis::Instruction<sym::SymAddr>>;

    fn into_iter(self) -> Self::IntoIter {
        self.insns.into_iter()
    }
}

impl<'a> IntoIterator for &'a Code {
    type Item = &'a ppcdis::Instruction<sym::SymAddr>;

    type IntoIter = std::slice::Iter<'a, ppcdis::Instruction<sym::SymAddr>>;

    fn into_iter(self) -> Self::IntoIter {
        self.insns.iter()
    }
}

impl<'a> IntoIterator for &'a mut Code {
    type Item = &'a mut ppcdis::Instruction<sym::SymAddr>;

    type IntoIter = std::slice::IterMut<'a, ppcdis::Instruction<sym::SymAddr>>;

    fn into_iter(self) -> Self::IntoIter {
        self.insns.iter_mut()
    }
}

impl interop::CRead<sym::SymAddr> for Code {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: interop::CReader<sym::SymAddr> + ?Sized
    {
        let mut insns = Vec::new();
        while !reader.eof() {
            insns.push(reader.read_val()?);
        }
        Ok(Self { insns })
    }
}

impl CDump<sym::AddrDumpCtx<'_>> for Code {
    type Error = std::fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
    ) -> Result<(), Self::Error> {
        let ctx = ctx.set_refs(false);
        for (i, insn) in self.insns.iter().enumerate() {
            if i > 0 {
                write!(out, "\n")?;
            }
            write!(out, "  ")?;
            CDump::dump(insn, out, &ctx)?;
        }
        Ok(())
    }
}

impl interop::Size for Code {
    fn len(&self) -> usize {
        self.insns.len() * 4
    }
}

#[derive(Debug)]
pub struct CCode {
    snippets: Vec<(String, Option<sym::SymAddr>)>,
    size: usize,
    pub args: Vec<String>,
    pub return_type: String,
}

// TODO: vars with global values
#[derive(Clone, Copy, Debug)]
pub struct CCodeTemplateFragment<'a> {
    snippets: &'a [(&'a str, Option<usize>)],
    asm: &'a [InsnTempl<'a>],
}

#[derive(Clone, Copy)]
pub enum TemplateRegExp<'a> {
    Epsilon,
    Fragment(CCodeTemplateFragment<'a>),
    Rep(&'a TemplateRegExp<'a>),
    Concat(&'a [TemplateRegExp<'a>]),
    Or(&'a [TemplateRegExp<'a>]),
}

pub struct CCodeTemplate<'a> {
    templ: TemplateRegExp<'a>,
    args: &'a [&'a str],
    return_type: &'a str,
}

#[derive(Debug)]
pub struct InsnTempl<'a> {
    pub name: &'a str,
    pub operands: &'a [ppcdis::Operand<usize>],
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SymTag {
    Reg, Lo, Hi, Ha
}

pub enum SimpleOperand<S> {
    Number(u32),
    Parts(Vec<SimpleOperand<S>>),
    Symbol(SymTag, S),
    RelSymbol(S),
    Unknown,
}

impl<S> SimpleOperand<S> {
    fn unify<T, F: FnMut(&S, Result<u32, &T>) -> bool>(
        self,
        other: SimpleOperand<T>,
        unifier: &mut F,
    ) -> bool {
        match (self, other) {
            (SimpleOperand::Number(n1), SimpleOperand::Number(n2)) => n1 == n2,
            (SimpleOperand::Parts(p1), SimpleOperand::Parts(p2)) => {
                if p1.len() != p2.len() {
                    return false;
                }
                p1.into_iter().zip(p2).all(|(a, b)| a.unify(b, unifier))
            },
            (SimpleOperand::Symbol(t1, s1), SimpleOperand::Symbol(t2, s2)) => {
                t1 == t2 && unifier(&s1, Err(&s2))
            }
            (SimpleOperand::RelSymbol(s1), SimpleOperand::Number(n2)) => {
                unifier(&s1, Ok(n2))
            }
            (SimpleOperand::RelSymbol(s1), SimpleOperand::RelSymbol(s2)) => {
                unifier(&s1, Err(&s2))
            }
            _ => false,
        }
    }
}

impl<S> From<ppcdis::Operand<S>> for SimpleOperand<S> {
    fn from(op: ppcdis::Operand<S>) -> Self {
        match op {
            ppcdis::Operand::Reg(n) => Self::Number(n.get_u32()),
            ppcdis::Operand::FReg(n) => Self::Number(n.get_u32()),
            ppcdis::Operand::CReg(n) => Self::Number(n.get_u32()),
            ppcdis::Operand::QReg(b, n) => Self::Parts(vec! [
                Self::Number(b as u32),
                Self::Number(n.get_u32()),
            ]),
            ppcdis::Operand::Mem(sym, r) => Self::Parts(vec! [
                ppcdis::Operand::Sym(sym.map_num(|s, i| i.extend(s))).into(),
                Self::Number(r.get_u32()),
            ]),
            ppcdis::Operand::Num(val) => Self::Number(val.as_u32()),
            ppcdis::Operand::Rel(rsym) => match rsym {
                ppcdis::RelValue::Value(_, v) => Self::Number(v.get_u32()),
                ppcdis::RelValue::Symbol(s)
                | ppcdis::RelValue::SymbolRel(s) => Self::RelSymbol(s),
                ppcdis::RelValue::SymbolLo(s) => Self::Symbol(SymTag::Lo, s),
                ppcdis::RelValue::SymbolHi(s) => Self::Symbol(SymTag::Hi, s),
                ppcdis::RelValue::SymbolHa(s) => Self::Symbol(SymTag::Ha, s),
                ppcdis::RelValue::Unknown => Self::Unknown,
            },
            ppcdis::Operand::Sym(sym) => match sym {
                ppcdis::RelValue::Value(_, v) => Self::Number(v.get_u32()),
                ppcdis::RelValue::SymbolRel(s) => Self::RelSymbol(s),
                ppcdis::RelValue::Symbol(s) => Self::Symbol(SymTag::Reg, s),
                ppcdis::RelValue::SymbolLo(s) => Self::Symbol(SymTag::Lo, s),
                ppcdis::RelValue::SymbolHi(s) => Self::Symbol(SymTag::Hi, s),
                ppcdis::RelValue::SymbolHa(s) => Self::Symbol(SymTag::Ha, s),
                ppcdis::RelValue::Unknown => Self::Unknown,
            },
        }
    }
}

impl Size for CCode {
    fn len(&self) -> usize {
        self.size
    }
}

fn match_fragment(
    insns: &[ppcdis::Instruction<sym::SymAddr>],
    templ: CCodeTemplateFragment,
    base: sym::SymAddr,
    output: &mut Vec<(String, Option<sym::SymAddr>)>,
) -> bool {
    if insns.len() < templ.asm.len() {
        return false;
    }

    let mut symmap = HashMap::new();
    for (i, (templ, insn)) in templ.asm.iter().zip(insns.iter()).enumerate() {
        if templ.name != &insn.name {
            return false;
        }

        if templ.operands.len() != insn.operands.len() {
            return false;
        }

        for (op_templ, op) in templ.operands.iter().zip(insn.operands.iter()) {
            let templ_simp = SimpleOperand::from(op_templ.clone());
            let actual_simp = SimpleOperand::from(op.clone());
            let unified = templ_simp.unify(actual_simp, &mut |symid, actual| {
                let addr = match actual {
                    Ok(off) => base + (i as u32 * 4 + off),
                    Err(addr) => *addr,
                };

                if let Some(old_addr) = symmap.insert(*symid, addr) {
                    if old_addr != addr {
                        return false;
                    }
                }

                true
            });

            if !unified {
                return false;
            }
        }
    }

    for &(code, symid) in templ.snippets {
        let addr = symid
            .map(|symid| {
                *symmap.get(&symid).expect("Symbol id in code template not in asm template")
            });
        output.push((code.to_string(), addr))
    }


    true
}

#[derive(Clone, Debug)]
enum Regex<'a> {
    Epsilon,
    Fragment(CCodeTemplateFragment<'a>),
    Rep(Box<Regex<'a>>),
    Concat(Vec<Regex<'a>>),
    Or(Vec<Regex<'a>>),
}

impl<'a> From<TemplateRegExp<'a>> for Regex<'a> {
    fn from(value: TemplateRegExp<'a>) -> Self {
        match value {
            TemplateRegExp::Epsilon => Self::Epsilon,
            TemplateRegExp::Fragment(f) => Self::Fragment(f),
            TemplateRegExp::Rep(inner) => Self::Rep(Box::new((*inner).into())),
            TemplateRegExp::Concat(parts) => {
                Self::Concat(parts.iter().map(|p| (*p).into()).collect())
            }
            TemplateRegExp::Or(choices) => {
                Self::Or(choices.iter().map(|p| (*p).into()).collect())
            }
        }
    }
}

impl<'a> Regex<'a> {
    pub fn matches_nil(&self) -> bool {
        match self {
            Regex::Epsilon => true,
            Regex::Fragment(_) => false,
            Regex::Rep(_) => true,
            Regex::Concat(rexps) => {
                rexps.iter().all(|r| r.matches_nil())
            }
            Regex::Or(choices) => {
                choices.iter().all(|r| r.matches_nil())
            }
        }
    }
}

#[derive(Clone)]
struct RegexState<'a> {
    insns: &'a[ppcdis::Instruction<sym::SymAddr>],
    cur_addr: sym::SymAddr,
    output: Vec<(String, Option<sym::SymAddr>)>,
}

impl<'a> RegexState<'a> {
    fn new(
        insns: &'a[ppcdis::Instruction<sym::SymAddr>],
        cur_addr: sym::SymAddr,
    ) -> Self {
        Self {
            insns,
            cur_addr,
            output: Vec::new(),
        }
    }

    fn done(&self, regex: &Regex<'a>) -> bool {
        self.insns.is_empty() && regex.matches_nil()
    }

    fn step(mut self, regex: &Regex<'a>) -> Vec<(Self, Regex<'a>)> {
        match regex {
            Regex::Epsilon => Vec::new(),
            Regex::Fragment(frag) => {
                let matches = frag.asm.len() <= self.insns.len() && match_fragment(
                    &self.insns[..frag.asm.len()],
                    *frag,
                    self.cur_addr,
                    &mut self.output,
                );
                if matches {
                    self.cur_addr += frag.asm.len() as u32 * 4;
                    self.insns = &self.insns[frag.asm.len()..];
                    vec![(self, Regex::Epsilon)]
                } else {
                    Vec::new()
                }
            }
            Regex::Rep(inner) => {
                let star = Regex::Rep(inner.clone());
                self.step(inner)
                    .into_iter()
                    .map(|(st, reg)| (st, Regex::Concat(vec![reg, star.clone()])))
                    .collect()
            }
            Regex::Concat(parts) => {
                let mut ret = Vec::new();
                for (i, part) in parts.iter().enumerate() {
                    let part_mod = self.clone().step(part);
                    if i + 1 == parts.len() {
                        ret.extend(part_mod);
                    } else {
                        for (st, regex_part) in part_mod {
                            let mut concat = Vec::with_capacity(parts.len());
                            concat.push(regex_part);
                            concat.extend_from_slice(&parts[i + 1..]);
                            ret.push((st, Regex::Concat(concat)));
                        }
                    }

                    if !part.matches_nil() {
                        break;
                    }
                }
                ret
            }
            Regex::Or(choices) => {
                let mut ret = Vec::with_capacity(choices.len());
                for choice in choices {
                    ret.extend(self.clone().step(choice));
                }
                ret
            }
        }
    }
}

impl Code {
    fn match_template(
        &self,
        templ: &CCodeTemplate,
        base: sym::SymAddr,
    ) -> Option<CCode> {
        let mut queue = VecDeque::new();
        queue.push_back((RegexState::new(&self.insns, base), templ.templ.into()));
        while let Some((st, regex)) = queue.pop_front() {
            if st.done(&regex) {
                return Some(CCode {
                    snippets: st.output,
                    size: self.len(),
                    args: templ.args.iter().map(|s| s.to_string()).collect(),
                    return_type: templ.return_type.into(),
                });
            }
            queue.extend(st.step(&regex));
        }

        return None;
    }
}

impl CDump<sym::AddrDumpCtx<'_>> for CCode {
    type Error = std::fmt::Error;

    fn dump(&self, out: &mut interop::Dumper, ctx: &sym::AddrDumpCtx<'_>) -> Result<(), Self::Error> {
        let ctx = ctx.set_refs(false);
        for (code, addr) in &self.snippets {
            write!(out, "{code}")?;
            if let Some(addr) = addr {
                addr.dump(out, &ctx)?;
            }
        }

        Ok(())
    }
}

