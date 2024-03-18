use std::fmt::Write as _;

use interop::CDump;

use crate::{rel, sym};

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

impl interop::CDump<sym::AddrDumpCtx<'_>> for Code {
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
