use std::{
    cell::RefCell,
    collections::BTreeMap,
    sync::LazyLock,
};

use num::FromPrimitive;
use num_derive::FromPrimitive;
use thiserror::Error;

use crate::{mk_err_wrapper, reader, rel, sym};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Error in parsing rel file: {0}")]
    RelFileError(rel::ErrorType),
    #[error("{0}")]
    ReaderError(reader::ErrorType),
    #[error("Expected symbol in database: {0}")]
    MissingSymbol(String),
    #[error("Bad script opcode: {0}")]
    BadOpcode(u16),
    #[error("Bad script opcode due to unexpected relocation entry")]
    UnresolvedOpcode,
    #[error("Bad script argument due to unexpected relocation entry")]
    UnresolvedArg,
    #[error("Bad script argument due to unexpected relocation entry ({0:?})")]
    UnresolvedArgRel(rel::RelocType),
    #[error("Unexpected number of args: expected ({0}) but got ({1})")]
    UnexpectedArgs(usize, u16),
}

mk_err_wrapper! {
    ErrorType {
        RelFileError => rel::ErrorType,
        ReaderError => reader::ErrorType,
    }
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
static ZONES_DESC: LazyLock<BTreeMap<i32, ExpressionType>> = LazyLock::new(|| {
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

        let (base, et) = ZONES_DESC.range(..=value)
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

impl sym::SymDisplay for Expr {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symdb: &sym::SymbolDatabase,
        area: u32,
    ) -> std::fmt::Result {
        match self {
            Expr::Address(addr) => write!(f, "{}", symdb.symbol_name(sym::SymAddr::Dol(*addr))),
            Expr::AddressSym(local) => write!(f, "{}", symdb.symbol_name(sym::SymAddr::Rel(area, *local))),
            Expr::Float(fl) => write!(f, "{fl:.5}"),
            Expr::UF(id) => write!(f, "UF({id})"),
            Expr::UW(id) => write!(f, "UW({id})"),
            Expr::GSW(id) => write!(f, "GSW({id})"),
            Expr::LSW(id) => write!(f, "LSW({id})"),
            Expr::GSWF(id) => write!(f, "GSWF({id})"),
            Expr::LSWF(id) => write!(f, "LSWF({id})"),
            Expr::GF(id) => write!(f, "GF({id})"),
            Expr::LF(id) => write!(f, "LF({id})"),
            Expr::GW(id) => write!(f, "GW({id})"),
            Expr::LW(id) => write!(f, "LW({id})"),
            Expr::Immediate(val, is_hex) => if *is_hex {
                write!(f, "0x{:08x}", *val as u32)
            } else {
                write!(f, "{val}")
            },
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
	ScriptEnd,
	Return,
	Label,
	Goto,
	LoopBegin,
	LoopIterate,
	LoopBreak,
	LoopContinue,
	WaitFrames,
	WaitMS,
	WaitUntil,
	IfStringEqual,
	IfStringNotEqual,
	IfStringLess,
	IfStringGreater,
	IfStringLessEqual,
	IfStringGreaterEqual,
	IfFloatEqual,
	IfFloatNotEqual,
	IfFloatLess,
	IfFloatGreater,
	IfFloatLessEqual,
	IfFloatGreaterEqual,
	IfIntEqual,
	IfIntNotEqual,
	IfIntLess,
	IfIntGreater,
	IfIntLessEqual,
	IfIntGreaterEqual,
	IfBitsSet,
	IfBitsClear,
	Else,
	EndIf,
	SwitchExpr,
	SwitchRaw,
	CaseIntEqual,
	CaseIntNotEqual,
	CaseIntLess,
	CaseIntGreater,
	CaseIntLessEqual,
	CaseIntGreaterEqual,
	CaseDefault,
	CaseIntEqualAny,
	CaseIntNotEqualAll,
	CaseBitsSet,
	EndMultiCase,
	CaseIntRange,
	SwitchBreak,
	EndSwitch,
	SetExprIntToExprInt,
	SetExprIntToRaw,
	SetExprFloatToExprFloat,
	AddInt,
	SubtractInt,
	MultiplyInt,
	DivideInt,
	ModuloInt,
	AddFloat,
	SubtractFloat,
	MultiplyFloat,
	DivideFloat,
	MemOpSetBaseInt,
	MemOpReadInt,
	MemOpReadInt2,
	MemOpReadInt3,
	MemOpReadInt4,
	MemOpReadIntIndexed,
	MemOpSetBaseFloat,
	MemOpReadFloat,
	MemOpReadFloat2,
	MemOpReadFloat3,
	MemOpReadFloat4,
	MemOpReadFloatIndexed,
  #[cfg(feature = "spm")]
	ClampInt,
	SetUserWordBase,
	SetUserFlagBase,
	AllocateUserWordBase,
	AndExpr,
	AndRaw,
	OrExpr,
	OrRaw,
	ConvertMSToFrames,
	ConvertFramesToMS,
	StoreIntToPtr,
	StoreFloatToPtr,
	LoadIntFromPtr,
	LoadFloatFromPtr,
	StoreIntToPtrExpr,
	StoreFloatToPtrExpr,
	LoadIntFromPtrExpr,
	LoadFloatFromPtrExpr,
	CallCppSync,
	CallScriptAsync,
	CallScriptAsyncSaveTID,
	CallScriptSync,
	TerminateThread,
	Jump,
	SetThreadPriority,
	SetThreadTimeQuantum,
	SetThreadTypeMask,
	ThreadSuspendTypes,
	ThreadResumeTypes,
	ThreadSuspendTypesOther,
	ThreadResumeTypesOther,
	ThreadSuspendTID,
	ThreadResumeTID,
	CheckThreadRunning,
	ThreadStart,
	ThreadStartSaveTID,
	ThreadEnd,
	ThreadChildStart,
	ThreadChildStartSaveTID,
	ThreadChildEnd,
	DebugOutputString,
	DebugUnk1,
	DebugExprToString,
	DebugUnk2,
	DebugUnk3,
}

impl std::fmt::Display for ScriptOpcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Evt_")?;
        std::fmt::Debug::fmt(self, f)
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
                SetExprIntToRaw, AndRaw, OrRaw,
            }: OpcodeIndent::PassthroughRaw,
            { AndExpr, OrExpr }: OpcodeIndent::PassthroughHex,
            {
                InternalFetch, ScriptEnd, Return, Label, Goto, LoopBreak,
                LoopContinue, WaitFrames, WaitMS, WaitUntil,

                EndMultiCase, SwitchBreak,

                SetExprIntToExprInt, SetExprFloatToExprFloat, AddInt,
                SubtractInt, MultiplyInt, DivideInt, ModuloInt, AddFloat,
                SubtractFloat, MultiplyFloat, DivideFloat,

                MemOpSetBaseInt, MemOpReadInt, MemOpReadInt2, MemOpReadInt3,
                MemOpReadInt4, MemOpReadIntIndexed, MemOpSetBaseFloat,
                MemOpReadFloat, MemOpReadFloat2, MemOpReadFloat3,
                MemOpReadFloat4, MemOpReadFloatIndexed,

                SetUserWordBase, SetUserFlagBase, AllocateUserWordBase,
                ConvertMSToFrames, ConvertFramesToMS, StoreIntToPtr,
                StoreFloatToPtr, LoadIntFromPtr, LoadFloatFromPtr,
                StoreIntToPtrExpr, StoreFloatToPtrExpr, LoadIntFromPtrExpr,
                LoadFloatFromPtrExpr,

                CallCppSync, CallScriptAsync, CallScriptAsyncSaveTID,
                CallScriptSync, Jump,

                TerminateThread, SetThreadPriority, SetThreadTimeQuantum,
                SetThreadTypeMask, ThreadSuspendTypes, ThreadResumeTypes,
                ThreadSuspendTypesOther, ThreadResumeTypesOther,
                ThreadSuspendTID, ThreadResumeTID, CheckThreadRunning,

                DebugOutputString, DebugUnk1, DebugExprToString,
                DebugUnk2, DebugUnk3,
            }: OpcodeIndent::Passthrough,
            {
                LoopBegin,

                IfStringEqual, IfStringNotEqual, IfStringLess, IfStringGreater,
                IfStringLessEqual, IfStringGreaterEqual, IfFloatEqual,
                IfFloatNotEqual, IfFloatLess, IfFloatGreater, IfFloatLessEqual,
                IfFloatGreaterEqual, IfIntEqual, IfIntNotEqual, IfIntLess,
                IfIntGreater, IfIntLessEqual, IfIntGreaterEqual, IfBitsSet,
                IfBitsClear,

                ThreadStart, ThreadStartSaveTID, ThreadChildStart,
                ThreadChildStartSaveTID,
            }: OpcodeIndent::IndentIn,
            {
                LoopIterate, EndIf,

                ThreadEnd, ThreadChildEnd,
            }: OpcodeIndent::IndentOut,
            {
                Else,

                CaseIntEqual, CaseIntNotEqual, CaseIntLess, CaseIntGreater,
                CaseIntLessEqual, CaseIntGreaterEqual, CaseDefault,
                CaseIntEqualAny, CaseIntNotEqualAll, CaseBitsSet, CaseIntRange,
            }: OpcodeIndent::IndentOutIn,

            { SwitchExpr, SwitchRaw }: OpcodeIndent::DblIndentIn,
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
            OpcodeIndent::PassthroughRaw => Some([
                ExprOverride::Normal,
                ExprOverride::Raw,
            ].to_vec()),
            OpcodeIndent::PassthroughHex => Some([
                ExprOverride::Normal,
                ExprOverride::Hex,
            ].to_vec()),
            OpcodeIndent::DblIndentIn => {
                if let Self::SwitchRaw = self {
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

impl sym::SymDisplay for Instruction {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symdb: &sym::SymbolDatabase,
        area: u32,
    ) -> std::fmt::Result {
        write!(f, "{}(", self.opc)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            arg.fmt(f, symdb, area)?;
        }
        write!(f, ")")
    }
}

impl Instruction {
    fn parse_one(reader: &mut reader::Reader) -> Res<Self> {
        let header = match reader.read_32()? {
            rel::Symbol::Value(v) => v,
            _ => return Err(error!(UnresolvedOpcode)),
        };

        let opc = ScriptOpcode::from_u16(header as u16)
            .ok_or_else(|| error!(BadOpcode(header as u16)))?;
        let arg_count = (header >> 16) as u16;
        let overrides = match opc.overrides() {
            None => vec![ExprOverride::Normal; arg_count as usize],
            Some(overrides) => {
                if overrides.len() != arg_count.into() {
                    return Err(error!(UnexpectedArgs(
                        overrides.len(), 
                        arg_count,
                    )));
                }
                overrides
            }
        };

        let mut args = Vec::new();
        for ovr in overrides {
            args.push(match reader.read_32()? {
                rel::Symbol::Value(v) => Expr::new(v as i32, ovr),
                rel::Symbol::Rel(r) => match r.rtype {
                    rel::RelocType::PPCAddr32 => {
                        // TODO: check that file is the correct value
                        Expr::AddressSym(r.target)
                    },
                    rt => bail!(UnresolvedArgRel(rt))
                },
                rel::Symbol::Partial
                | rel::Symbol::Unknown => bail!(UnresolvedArg),
            })
        }

        Ok(Self { opc, args })
    }

    fn size(&self) -> u32 {
        (1 + self.args.len() as u32) * 4
    }
}

#[derive(Debug)]
pub struct Script {
    insns: Vec<Instruction>,
    start: rel::SectionAddr,
    len: u32,
}

impl reader::Size for Script {
    fn len(&self) -> u32 {
        self.len
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

impl sym::SymDisplay for Script {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symdb: &sym::SymbolDatabase,
        area: u32,
    ) -> std::fmt::Result {
        let mut indent = 0;
        let mut addr = self.start;
        write!(f, "EVENT_SCRIPT(\n")?;
        for insn in self {
            write!(f, "/* {addr} */ ")?;

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
                write!(f, "  ")?;
            }

            insn.fmt(f, symdb, area)?;
            indent += ind_in;

            write!(f, ",\n")?;
            addr += insn.size();
        }

        write!(f, ")")
    }
}

impl Script {
    pub fn parse(
        reader: &mut reader::Reader,
    ) -> Res<Self> {
        let mut insns = Vec::new();
        loop {
            let insn = Instruction::parse_one(reader)?;
            let opc = insn.opc;
            insns.push(insn);
            if let ScriptOpcode::ScriptEnd = opc {
                break;
            }
        }

        reader.expect_fully_read()?;

        Ok(Self {
            insns,
            start: reader.start(),
            len: reader.rel_pos(),
        })
    }
}

pub struct EvtParser<'r, 'b>{
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

    pub fn add_from_symdb(&self, symdb: &sym::SymbolDatabase) -> Result<(), Error> {
        let mut evt_scripts = self.evt_scripts.borrow_mut();

        for sym in symdb.rel_iter(self.overlay.backing().header().id.get()) {
            if sym.value_type == sym::DataType::Simple(sym::SimpleType::Evt) {
                let mut reader = reader::Reader::new(&self.overlay, sym);
                let script = Script::parse(&mut reader)?;
                let addr = script.start;
                evt_scripts.insert(addr, script);
            }
        }

        Ok(())
    }

    pub fn search_evt_scripts(
        &self,
        symdb: &sym::SymbolDatabase,
    ) -> Result<(), Error> {
        let fn_evt_addr = symdb.get_addr("relSetEvtAddr")
            .ok_or_else(|| error!(MissingSymbol("relSetEvtAddr".into())))?;

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
                        rtype: rel::RelocType::PPCAddr16Lo,
                        orig: _,
                    }),
                    // jmp callee
                    rel::Symbol::Rel(rel::RelocSymbol {
                        file: 0,
                        target: rel::SectionAddr {
                            sect: _,
                            offset: callee,
                        },
                        rtype: rel::RelocType::PPCRel24,
                        orig: _,
                    })

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
            let mut reader = reader::Reader::new_unsized(self.overlay, addr);
            let script = Script::parse(&mut reader)?;

            // Search for nested event scripts.
            // TODO: does other calls take a script reference?
            for insn in &script {
                match insn.opc {
                    ScriptOpcode::CallScriptSync
                    | ScriptOpcode::CallScriptAsync
                    | ScriptOpcode::CallScriptAsyncSaveTID => {
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

    pub fn dump_scripts(&self, symdb: &sym::SymbolDatabase) {
        let evt_scripts = self.evt_scripts.borrow();
        for (addr, script) in evt_scripts.iter() {
            println!("{}: {}",
                symdb.symbol_name(sym::SymAddr::Rel(self.overlay.backing().header().id.get(), *addr)),
                sym::SymContext::new(
                    symdb,
                    self.overlay.backing().header().id.get(),
                    script,
                ),
            );
        }
    }
}


