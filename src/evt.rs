use std::{
    cell::RefCell,
    collections::BTreeMap,
    fmt::Write as _,
    sync::LazyLock,
};

use convert_case::{Case, Casing};
use interop::CReader;
use num::FromPrimitive;
use num_derive::FromPrimitive;
use thiserror::Error;

use crate::{reader, rel, sym};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Expected symbol in database: {0}")]
    MissingSymbol(String),
    #[error("Bad script opcode: {0}")]
    BadOpcode(u16),
    #[error("Bad script opcode due to unexpected relocation entry")]
    UnresolvedOpcode,
    #[error("Bad script argument due to unexpected relocation entry")]
    UnresolvedArg,
    #[error("Bad script argument due to unexpected relocation entry ({0:?})")]
    UnresolvedArgRel(ppcdis::RelocType),
    #[error("Unexpected number of args: expected ({0}) but got ({1})")]
    UnexpectedArgs(usize, u16),
}

macro_rules! error {
    ($reader:expr, $err:expr) => {{
        use ErrorType::*;
        $reader.error_custom($err.to_string())
    }};
}

macro_rules! bail {
    ($reader:expr, $err:expr) => {{
        use ErrorType::*;
        return Err($reader.error_custom($err.to_string()));
    }};
}

#[derive(Clone, Copy)]
pub enum ExpressionType {
    Address,
    Float,
    UF,
    UW,
    GSW,
    LSW,
    GSWF,
    LSWF,
    GF,
    LF,
    GW,
    LW,
    Immediate,
}

const ZONE_EXTENT: i32 = 10000000;
const UF_BASE: i32 = -210000000;
const ADDR_BASE: i32 = if cfg!(feature = "spm") {
    -270000000
} else {
    -250000000
};
const FLOAT_BASE: i32 = if cfg!(feature = "spm") {
    -250000000
} else {
    -230000000
};
static ZONES_DESC: LazyLock<BTreeMap<i32, ExpressionType>> =
    LazyLock::new(|| {
        use ExpressionType::*;
        let mut ret = BTreeMap::new();
        ret.insert(i32::MIN, Address);
        ret.insert(ADDR_BASE + 1, Float);

        let mut bound = UF_BASE;
        for et in [UF, UW, GSW, LSW, GSWF, LSWF, GF, LF, GW, LW] {
            ret.insert(bound, et);
            ret.insert(bound + ZONE_EXTENT + 1, Immediate);
            bound += ZONE_EXTENT * 2;
        }

        ret
    });

#[derive(Clone, Copy)]
pub enum ExprOverride {
    Normal,
    Hex,
    Raw,
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    Address(u32),
    AddressSym(rel::SectionAddr),
    Float(f32),
    UF(u32),
    UW(u32),
    GSW(u32),
    LSW(u32),
    GSWF(u32),
    LSWF(u32),
    GF(u32),
    LF(u32),
    GW(u32),
    LW(u32),
    Immediate(i32, bool),
}

impl Expr {
    pub fn new(value: i32, exp_override: ExprOverride) -> Self {
        use ExpressionType as ET;
        let hex = match exp_override {
            ExprOverride::Normal => false,
            ExprOverride::Hex => true,
            ExprOverride::Raw => return Self::Immediate(value, true),
        };

        let (base, et) = ZONES_DESC
            .range(..=value)
            .last()
            .expect("There should be a zone at the min i32 value");
        let adj_value = value.wrapping_sub(*base) as u32;
        match et {
            ET::Address => Self::Address(value as u32),
            ET::Float => Self::Float((value - FLOAT_BASE) as f32 / 1024.0),
            ET::UF => Self::UF(adj_value),
            ET::UW => Self::UW(adj_value),
            ET::GSW => Self::GSW(adj_value),
            ET::LSW => Self::LSW(adj_value),
            ET::GSWF => Self::GSWF(adj_value),
            ET::LSWF => Self::LSWF(adj_value),
            ET::GF => Self::GF(adj_value),
            ET::LF => Self::LF(adj_value),
            ET::GW => Self::GW(adj_value),
            ET::LW => Self::LW(adj_value),
            ET::Immediate => Self::Immediate(value, hex),
        }
    }
}

impl interop::CDump<sym::AddrDumpCtx<'_>> for Expr {
    type Error = std::fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
    ) -> Result<(), Self::Error> {
        match self {
            Expr::Address(addr) => {
                write!(out, "PTR(")?;
                sym::SymAddr::Dol(*addr).dump(out, ctx)?;
                write!(out, ")")
            }
            Expr::AddressSym(local) => {
                write!(out, "PTR(")?;
                sym::SymAddr::Rel(ctx.area(), *local).dump(out, ctx)?;
                write!(out, ")")
            }
            Expr::Float(fl) => write!(out, "FLOAT({fl:.5})"),
            Expr::UF(id) => write!(out, "UF({id})"),
            Expr::UW(id) => write!(out, "UW({id})"),
            Expr::GSW(id) => write!(out, "GSW({id})"),
            Expr::LSW(id) => write!(out, "LSW({id})"),
            Expr::GSWF(id) => write!(out, "GSWF({id})"),
            Expr::LSWF(id) => write!(out, "LSWF({id})"),
            Expr::GF(id) => write!(out, "GF({id})"),
            Expr::LF(id) => write!(out, "LF({id})"),
            Expr::GW(id) => write!(out, "GW({id})"),
            Expr::LW(id) => write!(out, "LW({id})"),
            Expr::Immediate(val, is_hex) => {
                if *is_hex {
                    write!(out, "0x{:08x}", *val as u32)
                } else {
                    write!(out, "{val}")
                }
            }
        }
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Self::new(value, ExprOverride::Normal)
    }
}

impl From<u32> for Expr {
    fn from(value: u32) -> Self {
        (value as i32).into()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum OpcodeIndent {
    Passthrough,
    PassthroughRaw,
    PassthroughHex,
    IndentIn,
    IndentOutIn,
    IndentOut,
    DblIndentIn,
    DblIndentOut,
}

#[derive(FromPrimitive, Clone, Copy, Debug)]
#[repr(u8)]
pub enum ScriptOpcode {
    InternalFetch = 0,
    EndScript,
    Return,
    Label,
    Goto,
    Loop,
    EndLoop,
    BreakLoop,
    ContinueLoop,
    WaitFrames,
    WaitMS,
    WaitUntil,
    IfStringEq,
    IfStringNe,
    IfStringLt,
    IfStringGt,
    IfStringLe,
    IfStringGe,
    IfFloatEq,
    IfFloatNe,
    IfFloatLt,
    IfFloatGt,
    IfFloatLe,
    IfFloatGe,
    IfIntEq,
    IfIntNe,
    IfIntLt,
    IfIntGt,
    IfIntLe,
    IfIntGe,
    IfBitsSet,
    IfBitsClear,
    Else,
    EndIf,
    Switch,
    SwitchR,
    CaseIntEq,
    CaseIntNe,
    CaseIntLt,
    CaseIntGt,
    CaseIntLe,
    CaseIntGe,
    CaseDefault,
    CaseOrEq,
    CaseAndEq,
    CaseFlag,
    CaseEnd,
    CaseBetween,
    BreakSwitch,
    EndSwitch,
    Set,
    SetRaw,
    SetFloat,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Addf,
    Subf,
    Mulf,
    Divf,
    MoSetBaseInt,
    MoReadInt,
    MoReadInt2,
    MoReadInt3,
    MoReadInt4,
    MoReadIntIndexed,
    MoSetBaseFloat,
    MoReadFloat,
    MoReadFloat2,
    MoReadFloat3,
    MoReadFloat4,
    MoReadFloatIndexed,
    #[cfg(feature = "spm")]
    ClampInt,
    SetUwBase,
    SetUfBase,
    AllocateUwBase,
    AndExpr,
    AndRaw,
    OrExpr,
    OrRaw,
    CvtMsF,
    CvtFMs,
    StoreInt,
    StoreFloat,
    LoadInt,
    LoadFloat,
    StoreIntInd,
    StoreFloatInd,
    LoadIntInd,
    LoadFloatInd,
    CallCppSync,
    CallScriptAsync,
    CallScriptAsyncTID,
    CallScriptSync,
    TerminateThread,
    Jump,
    SetThreadPriority,
    SetThreadTimeQuantum,
    SetThreadMask,
    ThreadSuspendTypes,
    ThreadResumeTypes,
    ThreadSuspendTypesOther,
    ThreadResumeTypesOther,
    ThreadSuspendTID,
    ThreadResumeTID,
    CheckThreadRunning,
    BeginThread,
    BeginThreadTid,
    EndThread,
    BeginChildThread,
    BeginChildThreadTid,
    EndChildThread,
    DebugOutputString,
    DebugUnk1,
    DebugExprToString,
    DebugUnk2,
    DebugUnk3,
}

impl std::fmt::Display for ScriptOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", format!("{self:?}").to_case(Case::ScreamingSnake))
    }
}

macro_rules! indent_match {
    (
        match ($mexp:expr) {
            $({$($opc:pat),* $(,)?}: $oi:expr),*,
        }
    ) => {
        match $mexp {
            $($($opc => $oi,)*)*
            #[cfg(feature = "spm")]
            ClampInt => OpcodeIndent::Passthrough,
        }
    };
}

impl ScriptOpcode {
    pub fn indent(&self) -> OpcodeIndent {
        use ScriptOpcode::*;
        indent_match!(match (self) {
            {
                SetRaw, AndRaw, OrRaw,
            }: OpcodeIndent::PassthroughRaw,
            { AndExpr, OrExpr }: OpcodeIndent::PassthroughHex,
            {
                InternalFetch, EndScript, Return, Label, Goto, BreakLoop,
                ContinueLoop, WaitFrames, WaitMS, WaitUntil,

                CaseEnd, BreakSwitch,

                Set, SetFloat, Add, Sub, Mul, Div, Mod, Addf, Subf, Mulf, Divf,

                MoSetBaseInt, MoReadInt, MoReadInt2, MoReadInt3, MoReadInt4,
                MoReadIntIndexed, MoSetBaseFloat, MoReadFloat, MoReadFloat2,
                MoReadFloat3, MoReadFloat4, MoReadFloatIndexed,

                SetUwBase, SetUfBase, AllocateUwBase,
                CvtMsF, CvtFMs, StoreInt, StoreFloat, LoadInt, LoadFloat,
                StoreIntInd, StoreFloatInd, LoadIntInd, LoadFloatInd,

                CallCppSync, CallScriptAsync, CallScriptAsyncTID,
                CallScriptSync, Jump,

                TerminateThread, SetThreadPriority, SetThreadTimeQuantum,
                SetThreadMask, ThreadSuspendTypes, ThreadResumeTypes,
                ThreadSuspendTypesOther, ThreadResumeTypesOther,
                ThreadSuspendTID, ThreadResumeTID, CheckThreadRunning,

                DebugOutputString, DebugUnk1, DebugExprToString,
                DebugUnk2, DebugUnk3,
            }: OpcodeIndent::Passthrough,
            {
                Loop,

                IfStringEq, IfStringNe, IfStringLt, IfStringGt, IfStringLe,
                IfStringGe, IfFloatEq, IfFloatNe, IfFloatLt, IfFloatGt,
                IfFloatLe, IfFloatGe, IfIntEq, IfIntNe, IfIntLt, IfIntGt,
                IfIntLe, IfIntGe, IfBitsSet, IfBitsClear,

                BeginThread, BeginThreadTid, BeginChildThread, BeginChildThreadTid,
            }: OpcodeIndent::IndentIn,
            {
                EndLoop, EndIf,

                EndThread, EndChildThread,
            }: OpcodeIndent::IndentOut,
            {
                Else,

                CaseIntEq, CaseIntNe, CaseIntLt, CaseIntGt, CaseIntLe,
                CaseIntGe, CaseDefault, CaseOrEq, CaseAndEq,
                CaseFlag, CaseBetween,
            }: OpcodeIndent::IndentOutIn,

            { Switch, SwitchR }: OpcodeIndent::DblIndentIn,
            { EndSwitch }: OpcodeIndent::DblIndentOut,
        })
    }

    pub fn overrides(&self) -> Option<Vec<ExprOverride>> {
        match self.indent() {
            OpcodeIndent::Passthrough
            | OpcodeIndent::IndentIn
            | OpcodeIndent::IndentOutIn
            | OpcodeIndent::IndentOut
            | OpcodeIndent::DblIndentOut => None,
            OpcodeIndent::PassthroughRaw => {
                Some([ExprOverride::Normal, ExprOverride::Raw].to_vec())
            }
            OpcodeIndent::PassthroughHex => {
                Some([ExprOverride::Normal, ExprOverride::Hex].to_vec())
            }
            OpcodeIndent::DblIndentIn => {
                if let Self::SwitchR = self {
                    // TODO: verify this is correct!
                    Some([ExprOverride::Raw].to_vec())
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Instruction {
    pub opc: ScriptOpcode,
    pub args: Vec<Expr>,
}

impl interop::CDump<sym::AddrDumpCtx<'_>> for Instruction {
    type Error = std::fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
    ) -> Result<(), Self::Error> {
        write!(out, "{}(", self.opc)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(out, ", ")?;
            }
            arg.dump(out, ctx)?;
        }
        write!(out, ")")
    }
}

impl Instruction {
    fn parse_one<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: interop::CReader<sym::SymAddr> + ?Sized,
    {
        let header = reader
            .read_u32()
            .map_err(|_| error!(reader, UnresolvedOpcode))?;

        let opc = ScriptOpcode::from_u16(header as u16)
            .ok_or_else(|| error!(reader, BadOpcode(header as u16)))?;
        let arg_count = (header >> 16) as u16;
        let overrides = match opc.overrides() {
            None => vec![ExprOverride::Normal; arg_count as usize],
            Some(overrides) => {
                if overrides.len() != arg_count.into() {
                    bail!(reader, UnexpectedArgs(overrides.len(), arg_count,));
                }
                overrides
            }
        };

        let mut args = Vec::new();
        for ovr in overrides {
            args.push(match reader.read_rel_u32()? {
                rel::Symbol::Value(v) => Expr::new(v as i32, ovr),
                rel::Symbol::Rel(r) => match r.rtype {
                    ppcdis::RelocType::PPCAddr32 => {
                        // TODO: check that file is the correct value
                        Expr::AddressSym(r.target)
                    }
                    rt => bail!(reader, UnresolvedArgRel(rt)),
                },
                rel::Symbol::Partial | rel::Symbol::Unknown => {
                    bail!(reader, UnresolvedArg)
                }
            })
        }

        Ok(Self { opc, args })
    }
}

#[derive(Debug)]
pub struct Script {
    insns: Vec<Instruction>,
    len: usize,
}

impl interop::Size for Script {
    fn len(&self) -> usize {
        self.len
    }
}

impl interop::CTypeable for Script {
    type Canonical = Script;
    fn compute_type() -> interop::CTypeKind {
        interop::CTypeKind::Array(None, interop::CTypePrim::U32)
    }
}

impl IntoIterator for Script {
    type Item = Instruction;

    type IntoIter = std::vec::IntoIter<Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.insns.into_iter()
    }
}

impl<'a> IntoIterator for &'a Script {
    type Item = &'a Instruction;

    type IntoIter = std::slice::Iter<'a, Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.insns.iter()
    }
}

impl interop::CDump<sym::AddrDumpCtx<'_>> for Script {
    type Error = std::fmt::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &sym::AddrDumpCtx,
    ) -> Result<(), Self::Error> {
        let mut indent = 0;
        write!(out, "{{\n")?;
        for insn in self {
            write!(out, "    ")?;
            let (ind_out, ind_in) = match insn.opc.indent() {
                OpcodeIndent::Passthrough
                | OpcodeIndent::PassthroughRaw
                | OpcodeIndent::PassthroughHex => (0, 0),
                OpcodeIndent::IndentIn => (0, 1),
                OpcodeIndent::IndentOutIn => (1, 1),
                OpcodeIndent::IndentOut => (1, 0),
                OpcodeIndent::DblIndentIn => (0, 2),
                OpcodeIndent::DblIndentOut => (2, 0),
            };

            indent -= ind_out;
            for _ in 0..indent {
                write!(out, "    ")?;
            }

            insn.dump(out, ctx)?;
            indent += ind_in;

            write!(out, "\n")?;
        }

        write!(out, "}}")
    }
}

impl interop::CRead<sym::SymAddr> for Script {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<sym::SymAddr> + ?Sized,
    {
        let mut insns = Vec::new();
        let len = reader.remaining();
        loop {
            let insn = Instruction::parse_one(reader)?;
            let opc = insn.opc;
            insns.push(insn);
            if let ScriptOpcode::EndScript = opc {
                break;
            }
        }

        reader.expect_fully_read()?;

        Ok(Self { insns, len })
    }
}

pub struct EvtParser<'r, 'b> {
    overlay: &'r rel::RelocOverlay<'b, 'b>,
    evt_scripts: RefCell<BTreeMap<rel::SectionAddr, Script>>,
}

impl<'r, 'b> EvtParser<'r, 'b> {
    pub fn new(overlay: &'r rel::RelocOverlay<'b, 'b>) -> Self {
        Self {
            overlay,
            evt_scripts: RefCell::new(BTreeMap::new()),
        }
    }

    pub fn add_from_symdb<T>(
        &self,
        symdb: &sym::SymbolDatabase<T>,
    ) -> reader::Res<()> {
        let mut evt_scripts = self.evt_scripts.borrow_mut();

        for sym in symdb.rel_iter(self.overlay.backing().header().id.get()) {
            if sym.value_type == sym::DataType::Simple(sym::SimpleType::Evt) {
                let mut reader = reader::Reader(reader::RelocOverlayReader::new(
                    &self.overlay,
                    sym,
                ));
                let script: Script = reader.read_val()?;
                evt_scripts.insert(sym.section_addr(), script);
            }
        }

        Ok(())
    }

    pub fn search_evt_scripts<T>(
        &self,
        symdb: &sym::SymbolDatabase<T>,
    ) -> reader::Res<()> {
        let fn_evt_addr = symdb.get_addr("relSetEvtAddr").ok_or_else(|| {
            reader::Error::new(reader::ErrorType::Custom(
                ErrorType::MissingSymbol("relSetEvtAddr".into()).to_string(),
            ))
        })?;

        // Find the root event script by using the heuristics of knowing that
        // it will call relSetEvtAddr() in the prolog function.
        let mut prolog_off = rel::SectionAddr::new(
            self.overlay.backing().header().prolog_section,
            self.overlay.backing().header().prolog.get(),
        );
        let mut queue = Vec::new();
        loop {
            let insn1 = self.overlay.read_32(prolog_off)?;
            if let rel::Symbol::Value(0x4e800020u32) = insn1 {
                break;
            }

            let insn1_1 = self.overlay.read_16(prolog_off)?;
            let insn1_2 = self.overlay.read_16(prolog_off + 2)?;
            let insn2 = self.overlay.read_32(prolog_off + 4)?;
            match (insn1_1, insn1_2, insn2) {
                (
                    // add r4, r4, evt_init@lo
                    rel::Symbol::Value(0x3884),
                    rel::Symbol::Rel(rel::RelocSymbol {
                        file: _,
                        target,
                        rtype: ppcdis::RelocType::PPCAddr16Lo,
                        orig: _,
                    }),
                    // jmp callee
                    rel::Symbol::Rel(rel::RelocSymbol {
                        file: 0,
                        target:
                            rel::SectionAddr {
                                sect: _,
                                offset: callee,
                            },
                        rtype: ppcdis::RelocType::PPCRel24,
                        orig: _,
                    }),
                ) if sym::SymAddr::Dol(callee) == fn_evt_addr => {
                    queue.push(target);
                }
                _ => {}
            }
            prolog_off += 4;
        }

        let mut evt_scripts = self.evt_scripts.borrow_mut();
        while let Some(addr) = queue.pop() {
            if evt_scripts.contains_key(&addr) {
                continue;
            }

            // Parse this current script.
            let mut reader = reader::Reader(reader::RelocOverlayReader::new_unsized(
                self.overlay,
                addr,
            ));
            let script: Script = reader.read_val()?;

            // Search for nested event scripts.
            // TODO: does other calls take a script reference?
            for insn in &script {
                match insn.opc {
                    ScriptOpcode::CallScriptSync
                    | ScriptOpcode::CallScriptAsync
                    | ScriptOpcode::CallScriptAsyncTID => {
                        match &insn.args[0] {
                            Expr::AddressSym(sym) => queue.push(*sym),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }

            evt_scripts.insert(addr, script);
        }

        Ok(())
    }

    pub fn dump_scripts(
        &self,
        symdb: &sym::SymbolDatabase<sym::SectionType>,
        strings: &sym::StringsMap,
    ) {
        let evt_scripts = self.evt_scripts.borrow();
        for (addr, script) in evt_scripts.iter() {
            let area = self.overlay.backing().header().id.get();
            println!(
                "{}: {}",
                symdb.symbol_name(
                    sym::SymAddr::Rel(
                        self.overlay.backing().header().id.get(),
                        *addr
                    ),
                    false,
                ),
                sym::AddrDumpCtx::new(
                    area,
                    &interop::ctype!(mut [i32]),
                    symdb,
                    strings,
                ).to_string(script).expect("should format correctly")
            );
        }
    }
}
