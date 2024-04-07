use std::{
    collections::HashMap,
    fmt::{Display, Write as _},
    sync::LazyLock,
};
use error::mk_err_wrapper;

use crate::rel;

pub struct B<const RES: bool>(());
#[const_trait]
pub trait Assert {}
impl const Assert for B<true> {}

#[derive(thiserror::Error, Debug)]
pub enum ErrorType {
    #[error("Invalid instruction({0:#?})")]
    InvalidInstruction(RawInsn<()>),
    #[error("Expected relative relocation value but got absolute instead")]
    BadAbsolute,
    #[error("Expected absolute relocation value but got relative instead")]
    BadRelative,
    #[error("#lo, #hi, #ha tagging requires immediate operand to be 16 bits")]
    NotBit16,
    #[error("Found relocatable value while expect constant")]
    ExpectValue,
}

mk_err_wrapper! {
    ErrorType
}

impl Num<1> {
    pub const fn get_bool(self) -> bool {
        self.0 != 0
    }
}

impl<const BITS: u8> Display for Num<BITS> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Num<const BITS: u8>(u32);
impl<const BITS: u8> Num<BITS> {
    pub const fn new<const VAL: u32>() -> Self where
        B<{VAL < (1<<BITS)}>: Assert
    {
        Self(VAL)
    }

    pub const fn downcast<const NEW_BITS: u8>(self) -> Num<NEW_BITS> where
        B<{NEW_BITS <= BITS}>: Assert
    {
        Num(self.0 & ((1<<NEW_BITS) - 1))
    }

    pub const fn extend<const NEW_BITS: u8>(self, signed: ImmOpType) -> Num<NEW_BITS> where
        B<{NEW_BITS >= BITS}>: Assert,
        B<{NEW_BITS <= 32}>: Assert,
    {
        Num(self.get_num(signed).as_u32() & ((1<<(NEW_BITS % 32)) - 1))
    }

    pub const fn get_u8(self) -> u8 where
        B<{BITS <= 8}>: Assert
    {
        self.0 as u8
    }

    pub const fn get_u16(self) -> u16 where
        B<{BITS <= 16}>: Assert
    {
        self.0 as u16
    }

    pub const fn get_u32(self) -> u32 {
        self.0
    }

    pub const fn get_i32(self) -> i32 {
        ((self.0 << (32 - BITS)) as i32) >> (32 - BITS)
    }

    pub const fn get_num(self, signed: ImmOpType) -> Number {
        match signed {
            ImmOpType::U => Number::U(self.get_u32()),
            ImmOpType::S => Number::S(self.get_i32()),
        }
    }

    pub const fn get_usize(self) -> usize {
        self.0 as usize
    }

    pub const fn bit<const IND: u8>(self) -> Num<1> where
        B<{IND < BITS}>: Assert
    {
        Num((self.0 >> IND) & 1)
    }

    pub const fn bits<const FROM: u8, const TO: u8>(self) -> Num<{TO-FROM}> where
        B<{FROM <= TO}>: Assert,
        B<{FROM < BITS}>: Assert,
        B<{TO <= BITS}>: Assert,
    {
        Num((self.0 >> FROM) & ((1<<(TO - FROM)) - 1))
    }

    const fn combine<const OTHER: u8>(self, other: Num<OTHER>) -> Num<{BITS + OTHER}> where
        B<{BITS + OTHER <= 32}>: Assert,
    {
        Num((self.0 << OTHER) | other.0)
    }
}

#[derive(Clone, Debug)]
pub enum RelValue<S, const BITS: u8> {
    Value(ImmOpType, Num<BITS>),
    Symbol(S),
    SymbolLo(S),
    SymbolHi(S),
    SymbolHa(S),
    SymbolRel(S),
    Unknown,
}

impl<Ctx, S, const BITS: u8> interop::CDump<Ctx> for RelValue<S, BITS> where
    S: interop::CDump<Ctx>
{
    type Error = S::Error;

    fn dump(&self, f: &mut interop::Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
        let (s, suff) = match self {
            RelValue::Value(signed, v) => {
                write!(
                    f,
                    "{}",
                    v.get_num(*signed)
                )?;
                return Ok(())
            },
            RelValue::Symbol(s) => (s, ""),
            RelValue::SymbolLo(s) => (s, "#lo"),
            RelValue::SymbolHi(s) => (s, "#hi"),
            RelValue::SymbolHa(s) => (s, "#ha"),
            RelValue::SymbolRel(s) => (s, ""),
            RelValue::Unknown => {
                write!(f, "#UNK")?;
                return Ok(());
            }
        };

        s.dump(f, ctx)?;
        write!(f, "{suff}")?;
        Ok(())
    }
}

impl<S, const BITS: u8> RelValue<S, BITS> {
    pub fn map_num<const BITS_NEW: u8, F>(self, mapper: F) -> RelValue<S, BITS_NEW>
    where
        F: Fn(ImmOpType, Num<BITS>) -> Num<BITS_NEW>,
    {
        match self {
            RelValue::Value(signed, v) => RelValue::Value(signed, mapper(signed, v)),
            RelValue::Symbol(s) => RelValue::Symbol(s),
            RelValue::SymbolLo(s) => RelValue::SymbolLo(s),
            RelValue::SymbolHi(s) => RelValue::SymbolHi(s),
            RelValue::SymbolHa(s) => RelValue::SymbolHa(s),
            RelValue::SymbolRel(s) => RelValue::SymbolRel(s),
            RelValue::Unknown => RelValue::Unknown,
        }
    }

    pub fn val(&self) -> Option<Num<BITS>> {
        if let Self::Value(_, v) = self {
            Some(*v)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
pub enum RegOpType {
    /// CR reg, i.e. upper 3 bits,
    C,
    /// CR reg upper 3 bits, with L bit at the bottom
    CL,
    /// QR reg is mid 3 bits, with W bit on the top, and U bit on the bottom
    QW,
    /// general purpose register,
    R,
    /// float register,
    F,
    /// raw integer
    N,
    /// Overflow flag
    Oe,
    /// Shift value that uses bit 30
    H,
    /// Mask value that uses 6 bits instead of 5
    M64,
    /// SPR index (11..20)
    Spr,
    /// float mask index {op + (1..9)}
    Fm,
    /// 4-bit immediate field to place into FPSCR
    U_,
}

#[derive(Clone, Copy, Debug)]
pub enum ImmOpType {
    /// Unsigned
    U,
    /// Signed
    S,
}

#[derive(Clone, Copy)]
enum InsnFlag {
    Rc, Lk,
}

#[derive(Clone, Copy)]
enum InsnDesc {
    /// Regular 3-operand D-form
    ///
    /// if true then shift op1 and op2
    D(RegOpType, RegOpType, ImmOpType, bool),
    /// Memory 2-operand D-form
    /// 3rd operand is the memory offset value
    DMem(RegOpType),
    /// Memory 2-operand DS-form
    /// 3rd operand is the memory offset value
    DSMem(RegOpType),
    /// Memory 4-operand DQ-form
    DQMem,
    /// B-form
    ///
    /// TODO: there's a lot of extended mnemonics stuff here
    B,
    /// I-form
    I,
    /// M-form, MD-form, MDS-form
    /// 4 to 5 operands
    ///
    /// because of a PowerPC quirk, 1st/2nd operands are always swapped
    M(RegOpType, RegOpType, RegOpType, RegOpType, Option<RegOpType>),
    /// SC-form
    SC,
    /// A-form, X-form, XL-form, XS-form, XO-form
    /// 2-4-reg operand,
    ///
    /// Note because of a quirk in PowerPC, if both the 3rd and 4th operand are
    /// present, they are swapped in operand order.
    ///
    /// If last boolean is true, swap 1st and 2nd operand too.
    AX([Option<RegOpType>; 4], Option<InsnFlag>, bool),
}

#[derive(Clone, Copy)]
enum InsnClass {
    Ent(&'static str, InsnDesc),
    Unk,
    Dir1(u8, &'static [InsnClass; 2]),
    Dir2(u8, &'static [InsnClass; 4]),
    Dir4(u8, &'static [InsnClass; 16]),
    Dir5(u8, &'static [InsnClass; 32]),
    Dir6(u8,  &'static [InsnClass; 64]),
    Dir10(u8, &'static LazyLock<HashMap<u16, InsnClass>>),
}

static INSN_ROOT: InsnClass = {
    use InsnClass::*;
    use InsnDesc::*;
    use InsnFlag::*;
    use RegOpType::*;
    use ImmOpType::*;
    use Some as P;
    use None as X;

    macro_rules! X_ent {
        ($name:ident $p1:ident $p2:ident $p3:ident $p4:ident: $fl:ident) => {
            Ent(stringify!($name), AX(
                [X_ent!{{$p1}}, X_ent!{{$p2}}, X_ent!{{$p3}}, X_ent!{{$p4}}],
                X_ent!{{$fl}},
                false,
            ))
        };
        ($name:ident $p1:ident $p2:ident $p3:ident $p4:ident: $fl:ident !) => {
            Ent(stringify!($name), AX(
                [X_ent!{{$p1}}, X_ent!{{$p2}}, X_ent!{{$p3}}, X_ent!{{$p4}}],
                X_ent!{{$fl}},
                true,
            ))
        };
        ({C}) => { Some(RegOpType::C) };
        ({L}) => { Some(RegOpType::CL) };
        ({Q}) => { Some(RegOpType::QW) };
        ({H}) => { Some(RegOpType::H) };
        ({R}) => { Some(RegOpType::R) };
        ({F}) => { Some(RegOpType::F) };
        ({M}) => { Some(RegOpType::Fm) };
        ({N}) => { Some(RegOpType::N) };
        ({O}) => { Some(RegOpType::Oe) };
        ({P}) => { Some(RegOpType::Spr) };
        ({U}) => { Some(RegOpType::U_) };
        ({Lk}) => { Some(InsnFlag::Lk) };
        ({Rc}) => { Some(InsnFlag::Rc) };
        ({X}) => { None };
    }

    static INSN_OP4_PS_EXT: LazyLock<HashMap<u16, InsnClass>> = LazyLock::new(|| {
        // TODO: check
        HashMap::from_iter([
            (0,   X_ent!(ps_cmpu0   C F F X: X)),
            (18,  X_ent!(ps_div     F F F X: Rc)),
            (20,  X_ent!(ps_sub     F F F X: Rc)),
            (21,  X_ent!(ps_add     F F F X: Rc)),
            (24,  X_ent!(ps_res     F X F X: Rc)),
            (26,  X_ent!(ps_rsqrte  F X F X: Rc)),
            (32,  X_ent!(ps_cmpo0   C F F X: X)),
            (40,  X_ent!(ps_neg     F X F X: Rc)),
            (64,  X_ent!(ps_cmpu1   C F F X: X)),
            (72,  X_ent!(ps_mr      F X F X: Rc)),
            (96,  X_ent!(ps_cmpo1   C F F X: X)),
            (136, X_ent!(ps_nabs    F X F X: Rc)),
            (264, X_ent!(ps_abs     F X F X: Rc)),
            (528, X_ent!(ps_merge00 F F F X: Rc)),
            (560, X_ent!(ps_merge01 F F F X: Rc)),
            (592, X_ent!(ps_merge10 F F F X: Rc)),
            (624, X_ent!(ps_merge11 F F F X: Rc)),
            (1014, X_ent!(dcbz_l    X R R X: X)),
        ])
    });

    // 209: R X R X
    // 210: R X 
    // 248 F R R (:3, :1)
    // paired-single operators
    static INSN_OP4_PS: [InsnClass; 32] = [
        /* 00 */ Dir10(21, &INSN_OP4_PS_EXT), // PS ARITH EXT
        /* 01 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 02 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 03 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 04 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 05 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 06 */ X_ent!(psq_lx     F R R Q: X),
        /* 07 */ X_ent!(psq_stx    F R R Q: X),
        /* 08 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 09 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 10 */ X_ent!(ps_sum0    F F F F: Rc),
        /* 11 */ X_ent!(ps_sum1    F F F F: Rc),
        /* 12 */ X_ent!(ps_muls0   F F X F: Rc),
        /* 13 */ X_ent!(ps_muls1   F F X F: Rc),
        /* 14 */ X_ent!(ps_madds0  F F F F: Rc),
        /* 15 */ X_ent!(ps_madds1  F F F F: Rc),
        /* 16 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 17 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 18 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 19 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 20 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 21 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 22 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 23 */ X_ent!(ps_sel     F F F F: Rc),
        /* 24 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 25 */ X_ent!(ps_mul     F F X F: Rc),
        /* 26 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 27 */ Dir10(21, &INSN_OP4_PS_EXT), // PS EXT
        /* 28 */ X_ent!(ps_msub    F F F F: Rc),
        /* 29 */ X_ent!(ps_madd    F F F F: Rc),
        /* 30 */ X_ent!(ps_nmsub   F F F F: Rc),
        /* 31 */ X_ent!(ps_nmadd   F F F F: Rc),
    ];

    // CR Ops + misc. All XL types
    static INSN_OP19: LazyLock<HashMap<u16, InsnClass>> = LazyLock::new(|| {
        const CCXX: InsnDesc = AX([P(C), P(C), X, X], X, false);
        const NNNX: InsnDesc = AX([P(N), P(N), P(N), X], X, false);
        const XXXX: InsnDesc = AX([X, X, X, X], X, false);
        const BRN: InsnDesc = AX([P(N), P(N), X, X], P(Lk), false);
        HashMap::from_iter([
            (0,   Ent("mcrf",  CCXX)),
            (16,  Ent("bclr",  BRN)), // [l] TODO: verify BH
            (18,  Ent("rfid",  XXXX)),
            (33,  Ent("crnor", NNNX)),
            (50,  Ent("rfi",   XXXX)),
            (82,  Ent("rfsvc", XXXX)), // TODO: verify
            (129, Ent("crandc", NNNX)),
            (150, Ent("isync", XXXX)),
            (193, Ent("crxor", NNNX)),
            (225, Ent("crnand", NNNX)),
            (257, Ent("crand", NNNX)),
            (274, Ent("hrfid", XXXX)), // TODO: verify
            (289, Ent("creqv", NNNX)),
            (417, Ent("crorc", NNNX)),
            (449, Ent("cror",  NNNX)),
            (528, Ent("bcctr", BRN)), // [l]
        ])
    });

    // All either MD or MDS type, use MDS-esque opcodes though
    static INSN_FX_ROTS: [InsnClass; 16] = [
        /* 00 */ Ent("rldicl", M(R, R, H, M64, X)),
        /* 01 */ Ent("rldicl", M(R, R, H, M64, X)),
        /* 02 */ Ent("rldicr", M(R, R, H, M64, X)),
        /* 03 */ Ent("rldicr", M(R, R, H, M64, X)),
        /* 04 */ Ent("rldic",  M(R, R, H, M64, X)),
        /* 05 */ Ent("rldic",  M(R, R, H, M64, X)),
        /* 06 */ Ent("rldimi", M(R, R, H, M64, X)),
        /* 07 */ Ent("rldimi", M(R, R, H, M64, X)),
        /* 08 */ Ent("rldcl", M(R, R, R, M64, X)),
        /* 09 */ Ent("rldcr", M(R, R, R, M64, X)),
        /* 10 */ Unk,
        /* 11 */ Unk,
        /* 12 */ Unk,
        /* 13 */ Unk,
        /* 14 */ Unk,
        /* 15 */ Unk,
    ];

    static INSN_OP31: LazyLock<HashMap<u16, InsnClass>> = LazyLock::new(|| {
        let dups = HashMap::from([
            (8,   X_ent!(subfc    R R R O: Rc)),
            (9,   X_ent!(mullhdu  R R R X: Rc)),
            (10,  X_ent!(addc     R R R O: Rc)),
            (11,  X_ent!(mullhwu  R R R X: Rc)),
            (40,  X_ent!(subf     R R R O: Rc)),
            (73,  X_ent!(mullhd   R R R X: Rc)),
            (75,  X_ent!(mullhw   R R R X: Rc)),
            (104, X_ent!(neg      R R X O: Rc)),
            (136, X_ent!(subfe    R R R O: Rc)),
            (138, X_ent!(adde     R R R O: Rc)),
            (200, X_ent!(subfze   R R X O: Rc)),
            (202, X_ent!(addze    R R X O: Rc)),
            (232, X_ent!(subfme   R R R O: Rc)),
            (233, X_ent!(mulld    R R R O: Rc)),
            (234, X_ent!(addme    R R R O: Rc)),
            (235, X_ent!(mullw    R R R O: Rc)),
            (266, X_ent!(add      R R R O: Rc)),
            (457, X_ent!(divdu    R R R O: Rc)),
            (459, X_ent!(divwu    R R R O: Rc)),
            (489, X_ent!(divd     R R R O: Rc)),
            (491, X_ent!(divw     R R R O: Rc)),
        ]);
        let mut ret = HashMap::from([
            (0,   X_ent!(cmp      L R R X: X)),
            (4,   X_ent!(tw       N R R X: X)),
            (19,  Dir1(11, &[
                /* 0 */ X_ent!(mtcr R X X X: X),
                /* 1 */ Unk,
            ])),
            (20,  X_ent!(lwarx    R R R X: X)),
            (21,  X_ent!(ldx      R R R X: X)),
            (23,  X_ent!(lwzx     R R R X: X)),
            (24,  X_ent!(slw      R R R X: Rc !)),
            (26,  X_ent!(cntlzw   R R X X: Rc !)),
            (27,  X_ent!(sld      R R R X: Rc !)),
            (28,  X_ent!(and      R R R X: Rc !)),
            (32,  X_ent!(cmpl     L R R X: X)),
            (53,  X_ent!(ldux     R R R X: X)),
            (54,  X_ent!(dcbst    X R R X: X)),
            (55,  X_ent!(lwzux    R R R X: X)),
            (58,  X_ent!(cntlzd   R R X X: Rc !)),
            (60,  X_ent!(andc     R R R X: Rc !)),
            (68,  X_ent!(td       N R R X: X)),
            (83,  X_ent!(mfmsr    R X X X: X)),
            (84,  X_ent!(ldarx    R R R X: X)),
            (86,  Dir1(10, &[
                /* 0 */ X_ent!(dcbf  X R R X: X),
                /* 1 */ X_ent!(dcbfl X R R X: X),
            ])),
            (87,  X_ent!(lbzx     R R R X: X)),
            (119, X_ent!(lbzux    R R R X: X)),
            (122, X_ent!(popcntb  R R X X: Rc !)),
            (124, X_ent!(nor      R R R X: Rc !)),
            (144, Dir1(11, &[
                /* 0 */ X_ent!(mtcrf  R M X X: X !),
                /* 1 */ X_ent!(mtocrf R M X X: X !),
            ])),
            (146, X_ent!(mtmsr    R X X X: X)),
            (149, X_ent!(stdx     R R R X: X)),
            (150, X_ent!(stwcx    R R R X: X)),
            (151, X_ent!(stwx     R R R X: X)),
            (181, X_ent!(stdux    R R R X: X)),
            (183, X_ent!(stwux    R R R X: X)),
            (210, X_ent!(mtsr     R N X X: X !)),
            (214, X_ent!(stdcx    R R R X: X)),
            (215, X_ent!(stbx     R R R X: X)),
            (246, X_ent!(dcbtst   X R R X: X)),
            (247, X_ent!(stbux    R R R X: X)),
            (278, X_ent!(dcbt     N R R X: X)),
            (279, X_ent!(lhzx     R R R X: X)),
            (284, X_ent!(eqv      R R R X: Rc !)),
            (310, X_ent!(eciwx    R R R X: X)),
            (311, X_ent!(lhzux    R R R X: X)),
            (316, X_ent!(xor      R R R X: Rc !)),
            (339, X_ent!(mfspr    R P X X: X)),
            (341, X_ent!(lwax     R R R X: X)),
            (343, X_ent!(lhax     R R R X: X)),
            (371, X_ent!(mftb     R M X X: X)),
            (373, X_ent!(lwaux    R R R X: X)),
            (375, X_ent!(lhaux    R R R X: X)),
            (407, X_ent!(sthx     R R R X: X)),
            (412, X_ent!(orc      R R R X: Rc !)),
            (438, X_ent!(ecowx    R R R X: X)),
            (439, X_ent!(sthux    R R R X: X)),
            (444, X_ent!(or       R R R X: Rc !)),
            (467, X_ent!(mtspr    R P X X: X !)),
            (470, X_ent!(dcbi     X R R X: X)),
            (476, X_ent!(nand     R R R X: Rc !)),
            (512, X_ent!(mcrxr    C X X X: X)),
            (533, X_ent!(lswx     R R R X: X)),
            (534, X_ent!(lwbrx    R R R X: X)),
            (535, X_ent!(lfsx     F R R X: X)),
            (536, X_ent!(srw      R R R X: Rc !)),
            (539, X_ent!(srd      R R R X: Rc !)),
            (567, X_ent!(lfsux    F R R X: X)),
            (595, X_ent!(mfsr     R N X X: X)),
            (597, X_ent!(lswi     R R R X: X)),
            (598, Dir2(9, &[
                /* 0 */ X_ent!(sync    X X X X: X),
                /* 1 */ X_ent!(lwsync  X X X X: X),
                /* 2 */ X_ent!(ptesync X X X X: X),
                /* 3 */ Unk,
            ])),
            (599, X_ent!(lfdx     F R R X: X)),
            (631, X_ent!(lfdux    F R R X: X)),
            (661, X_ent!(stswx    R R R X: X)),
            (662, X_ent!(stwbrx   R R R X: X)),
            (663, X_ent!(stfsx    F R R X: X)),
            (695, X_ent!(stfsux   F R R X: X)),
            (725, X_ent!(stswi    R R R X: X)),
            (727, X_ent!(stfdx    F R R X: X)),
            (759, X_ent!(stfdux   F R R X: X)),
            (792, X_ent!(sraw     R R R X: Rc !)),
            (794, X_ent!(srad     R R R X: Rc !)),
            (824, X_ent!(srawi    R R N X: Rc !)),
            (826, X_ent!(sradi    R R H X: Rc !)),
            (827, X_ent!(sradi    R R H X: Rc !)),
            (854, X_ent!(eieio    X X X X: X)),
            (922, X_ent!(extsh    R R X X: Rc !)),
            (954, X_ent!(extsb    R R X X: Rc !)),
            (982, X_ent!(icbi     X R R X: X)),
            (983, X_ent!(stfiwx   F R R X: X)),
            (986, X_ent!(extsw    R R X X: Rc !)),
            (1014, X_ent!(dcbz    X R R X: X)),
        ]);

        for (k, v) in dups {
            ret.insert(k, v.clone());
            ret.insert(k + 512, v);
        }

        ret
    });

    static INSN_DS_LOADS: [InsnClass; 4] = [
        Ent("ld", DSMem(R)),
        Ent("ldu", DSMem(R)),
        Ent("lwa", DSMem(R)),
        Unk,
    ];

    static INSN_DS_STORES: [InsnClass; 4] = [
        Ent("std", DSMem(R)),
        Ent("stdu", DSMem(R)),
        Unk,
        Unk,
    ];

    const FXFX: InsnDesc = AX([P(F), X, P(F), X], P(Rc), false);
    const FFFX: InsnDesc = AX([P(F), P(F), P(F), X], P(Rc), false);
    const FFXF: InsnDesc = AX([P(F), P(F), X, P(F)], P(Rc), false);
    const FFFF: InsnDesc = AX([P(F), P(F), P(F), P(F)], P(Rc), false);
    static INSN_OP59_A: [InsnClass; 16] = [
        /* 16 + 00 */ Unk,
        /* 16 + 01 */ Unk,
        /* 16 + 02 */ Ent("fdivs", FFFX),
        /* 16 + 03 */ Unk,
        /* 16 + 04 */ Ent("fsubs", FFFX),
        /* 16 + 05 */ Ent("fadds", FFFX),
        /* 16 + 06 */ Ent("fsqrts", FXFX),
        /* 16 + 07 */ Unk,
        /* 16 + 08 */ Ent("fres", FXFX),
        /* 16 + 09 */ Ent("fmuls", FFXF),
        /* 16 + 10 */ Ent("frsqrtes", FFFX),
        /* 16 + 11 */ Unk,
        /* 16 + 12 */ Ent("fmsubs", FFFF),
        /* 16 + 13 */ Ent("fmadds", FFFF),
        /* 16 + 14 */ Ent("fnmsubs", FFFF),
        /* 16 + 15 */ Ent("fnmadds", FFFF),
    ];

    static INSN_OP59: [InsnClass; 2] = [
        /* 0 */ Unk,
        /* 1 */ Dir4(27, &INSN_OP59_A),
    ];

    static INSN_OP63_X: LazyLock<HashMap<u16, InsnClass>> = LazyLock::new(|| {
        HashMap::from([
            (0,   X_ent!(fcmp     C F F X: Rc)),
            (12,  X_ent!(frsp     F X F X: Rc)),
            (14,  X_ent!(fctiw    F X F X: Rc)),
            (15,  X_ent!(fctiwz   F X F X: Rc)),
            (32,  X_ent!(fcmpo    C F F X: Rc)),
            (38,  X_ent!(mtfsb1   N X X X: Rc)),
            (40,  X_ent!(fneg     F X F X: Rc)),
            (64,  X_ent!(mcrfs    C C X X: Rc)),
            (70,  X_ent!(mtfsb0   N X X X: Rc)),
            (72,  X_ent!(fmr      F X F X: Rc)),
            (134, X_ent!(mtfsfi   C X U X: Rc)),
            (136, X_ent!(fnabs    F X F X: Rc)),
            (264, X_ent!(fabs     F X F X: Rc)),
            (583, X_ent!(mffs     F X X X: Rc)),
            (711, X_ent!(mtfsf    M X F X: Rc)),
            (814, X_ent!(fctid    F X F X: Rc)),
            (815, X_ent!(fctidz   F X F X: Rc)),
            (846, X_ent!(fcfid    F X F X: Rc)),
        ])
    });

    static INSN_OP63_A: [InsnClass; 16] = [
        /* 16 + 00 */ Unk,
        /* 16 + 01 */ Unk,
        /* 16 + 02 */ Ent("fdiv", FFFX),
        /* 16 + 03 */ Unk,
        /* 16 + 04 */ Ent("fsub", FFFX),
        /* 16 + 05 */ Ent("fadd", FFFX),
        /* 16 + 06 */ Ent("fsqrt", FXFX),
        /* 16 + 07 */ Ent("fsel", FFFF),
        /* 16 + 08 */ Ent("fre", FXFX),
        /* 16 + 09 */ Ent("fmul", FFXF),
        /* 16 + 10 */ Ent("frsqrte", FFFX),
        /* 16 + 11 */ Unk,
        /* 16 + 12 */ Ent("fmsub", FFFF),
        /* 16 + 13 */ Ent("fmadd", FFFF),
        /* 16 + 14 */ Ent("fnmsub", FFFF),
        /* 16 + 15 */ Ent("fnmadd", FFFF),
    ];

    static INSN_OP63: [InsnClass; 2] = [
        /* 0 */ Dir10(21, &INSN_OP63_X),
        /* 1 */ Dir4(27, &INSN_OP63_A),
    ];

    static INSN_TYPE_TABLE: [InsnClass; 64] = [
        /* 00 */ Unk, // TODO some of these are reserved
        /* 01 */ Unk,
        /* 02 */ Ent("tdi", D(N, R, S, false)), // trap doubleword immediate
        /* 03 */ Ent("twi", D(N, R, S, false)), // trap word immediate
        /* 04 */ Dir5(26, &INSN_OP4_PS), // PS operations
        /* 05 */ Unk,
        /* 06 */ Unk,
        /* 07 */ Ent("mulli", D(R, R, S, false)), // multiply low immediate
        /* 08 */ Ent("subfic", D(R, R, S, false)), // subtract from immediate carrying
        /* 09 */ Ent("dozi", D(R, R, S, false)), // difference or zero immediate
        /* 10 */ Ent("cmpli", D(CL, R, U, false)), // compare logical immediate
        /* 11 */ Ent("cmpi", D(CL, R, S, false)), // compare immediate
        /* 12 */ Ent("addic", D(R, R, S, false)), // add immediate carrying
        /* 13 */ Ent("addic.", D(R, R, S, false)), // add immediate carrying and record
        /* 14 */ Ent("addi", D(R, R, S, false)), // add immediate
        /* 15 */ Ent("addis", D(R, R, S, false)), // add immediate shifted
        /* 16 */ Ent("bc", B), // branch conditional [l][a]
        /* 17 */ Dir1(30, &[
            /* 0 */ Unk,
            /* 1 */ Ent("sc", InsnDesc::SC), // system call
        ]),
        /* 18 */ Ent("b", I), // branch [l][a]
        /* 19 */ Dir10(21, &INSN_OP19),
        /* 20 */ Ent("rlwimi", M(R, R, N, N, P(N))), // rotate left word imm. then mask insert
        /* 21 */ Ent("rlwinm", M(R, R, N, N, P(N))), // rotate left word imm. then AND with mask
        /* 22 */ Ent("rlmi", M(R, R, N, N, P(N))), // rotate left then mask insert
        /* 23 */ Ent("rlwnm", M(R, R, N, N, P(N))), // rotate left word then AND with mask
        /* 24 */ Ent("ori", D(R, R, U, true)), // OR immediate
        /* 25 */ Ent("oris", D(R, R, U, true)), // OR immediate shifted
        /* 26 */ Ent("xori", D(R, R, U, true)),  // XOR immediate
        /* 27 */ Ent("xoris", D(R, R, U, true)), // XOR immediate shifted
        /* 28 */ Ent("andi.", D(R, R, U, true)),  // AND immediate
        /* 29 */ Ent("andis.", D(R, R, U, true)), // AND immediate shifted
        /* 30 */ Dir4(27, &INSN_FX_ROTS), // FX dwd rot
        /* 31 */ Dir10(21, &INSN_OP31), // FX extended ops
        /* 32 */ Ent("lwz", DMem(R)), // load word and zero
        /* 33 */ Ent("lwzu", DMem(R)), // load word and zero with update
        /* 34 */ Ent("lbz", DMem(R)), // load byte and zero
        /* 35 */ Ent("lbzu", DMem(R)), // load byte and zero with update
        /* 36 */ Ent("stw", DMem(R)), // store word and zero
        /* 37 */ Ent("stwu", DMem(R)), // store word and zero with update
        /* 38 */ Ent("stb", DMem(R)), // store byte and zero
        /* 39 */ Ent("stbu", DMem(R)), // store byte and zero with update
        /* 40 */ Ent("lhz", DMem(R)), // load half and zero
        /* 41 */ Ent("lhzu", DMem(R)), // load half and zero with update
        /* 42 */ Ent("lha", DMem(R)), // load half algebraic
        /* 43 */ Ent("lhau", DMem(R)), // load half algebraic with update
        /* 44 */ Ent("sth", DMem(R)), // store half and zero
        /* 45 */ Ent("sthu", DMem(R)), // store half and zero with update
        /* 46 */ Ent("lmw", DMem(R)), // load multiple word
        /* 47 */ Ent("stmw", DMem(R)), // store multiple word
        /* 48 */ Ent("lfs", DMem(R)), // load floating-point single
        /* 49 */ Ent("lfsu", DMem(R)), // load floating-point single with update
        /* 50 */ Ent("lfd", DMem(R)), // load floating-point double
        /* 51 */ Ent("lfdu", DMem(R)), // load floating-point double with update
        /* 52 */ Ent("stfs", DMem(R)), // store floating-point single
        /* 53 */ Ent("stfsu", DMem(R)), // store floating-point single with update
        /* 54 */ Ent("stfd", DMem(R)), // store floating-point double
        /* 55 */ Ent("stfdu", DMem(R)), // store floating-point double with update
        /* 56 */ Ent("psq_l", DQMem),
        /* 57 */ Ent("psq_lu", DQMem),
        /* 58 */ Dir2(30, &INSN_DS_LOADS),
        /* 59 */ Dir1(26, &INSN_OP59),
        /* 60 */ Ent("psq_st", DQMem),
        /* 61 */ Ent("psq_stu", DQMem),
        /* 62 */ Dir2(30, &INSN_DS_STORES),
        /* 63 */ Dir1(26, &INSN_OP63),
    ];

    Dir6(0, &INSN_TYPE_TABLE)
};

pub enum RawInsn<S> {
    Concrete(u32),
    Rel {
        sym: S,
        rtype: rel::RelocType,
        value: u32,
    },
}

impl<S: std::fmt::Debug> std::fmt::Debug for RawInsn<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                Self::Concrete(val) => write!(f, "{val:08x}"),
                Self::Rel { sym: _, rtype, value } => {
                    let mut tmp = format!("{value:08x}").as_bytes().to_vec();
                    let sym_mask = rtype.mask();
                    for i in 0..8 {
                        if (sym_mask >> (i * 4)) & 0xf != 0 {
                            tmp[7 - i] = b'?';
                        }
                    }
                    write!(f, "{}", unsafe {
                        std::str::from_utf8_unchecked(&tmp)
                    })
                }
            }
        } else {
            match self {
                Self::Concrete(arg0) => f.debug_tuple("Concrete")
                    .field(arg0)
                    .finish(),
                Self::Rel { sym, rtype, value } => f
                    .debug_struct("Rel")
                    .field("sym", sym)
                    .field("rtype", rtype)
                    .field("value", value)
                    .finish(),
            }

        }
    }
}

impl<S: Clone> RawInsn<S> {
    fn desym(&self) -> RawInsn<()> {
        match self {
            RawInsn::Concrete(v) => RawInsn::Concrete(*v),
            &RawInsn::Rel { sym: _, rtype, value } => RawInsn::Rel {
                sym: (),
                rtype,
                value,
            },
        }
    }

    fn expect_val<const BITS: u8>(&self, from: u8) -> Res<Num<BITS>> {
        self.get(ImmOpType::U, from)
            .val()
            .ok_or_else(|| error!(ExpectValue))
    }

    fn get<const BITS: u8>(&self, signed: ImmOpType, from: u8) -> RelValue<S, BITS> {
        const MASK_14_32: u32 = 0x0000fffc;
        const BIT_TAKEN: u32 = 1 << 21;

        let shf = 32 - (from + BITS);
        let mask = (1 << BITS) - 1;
        match self {
            RawInsn::Concrete(v) => RelValue::Value(signed, Num((v >> shf) & mask)),
            RawInsn::Rel {
                sym,
                rtype,
                mut value,
            } => {
                let rel_mask = match rtype.eval(0, Some(0)).expect("should be some") {
                    rel::RelocAction::None
                    | rel::RelocAction::Sect
                    | rel::RelocAction::End => return RelValue::Unknown,
                    rel::RelocAction::Write16 { val: _, mask } => mask.into(),
                    rel::RelocAction::Write32 { val, mask } => {
                        if mask == BIT_TAKEN | MASK_14_32 {
                            value = (value & !BIT_TAKEN) | (val & BIT_TAKEN);
                            mask & !BIT_TAKEN
                        } else {
                            mask
                        }
                    },
                };

                let shf_rel_mask = (rel_mask >> shf) & mask;
                if shf_rel_mask == 0 {
                    // Completly concrete value
                    RelValue::Value(signed, Num((value >> shf) & mask))
                } else if shf_rel_mask == mask && BITS == rtype.reloc_bits() {
                    // Completly symbolic value
                    (match rtype {
                        rel::RelocType::PPCNone
                        | rel::RelocType::RvlNone
                        | rel::RelocType::RvlSect
                        | rel::RelocType::RvlEnd => |_| RelValue::Unknown,
                        rel::RelocType::PPCAddr32
                        | rel::RelocType::PPCAddr24
                        | rel::RelocType::PPCAddr16
                        | rel::RelocType::PPCAddr14
                        | rel::RelocType::PPCAddr14BrTaken
                        | rel::RelocType::PPCAddr14BrNotTaken => RelValue::Symbol,
                        rel::RelocType::PPCAddr16Lo => RelValue::SymbolLo,
                        rel::RelocType::PPCAddr16Hi => RelValue::SymbolHi,
                        rel::RelocType::PPCAddr16Ha => RelValue::SymbolHa,
                        rel::RelocType::PPCRel24
                        | rel::RelocType::PPCRel14
                        | rel::RelocType::PPCRel14BrTaken
                        | rel::RelocType::PPCRel14BrNotTaken => RelValue::SymbolRel,
                    })(sym.clone())
                } else {
                    RelValue::Unknown
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct Instruction<S> {
    pub base_name: &'static str,
    pub name: String,
    pub operands: Vec<Operand<S>>,
}

fn lookup_insn<S: Clone>(raw: &RawInsn<S>) -> Res<(&'static str, InsnDesc)> {
    let mut class_tbl = INSN_ROOT;
    loop {
        class_tbl = match class_tbl {
            InsnClass::Ent(name, desc) => return Ok((name, desc)),
            InsnClass::Unk => bail!(InvalidInstruction(raw.desym())),
            InsnClass::Dir1(ind, tbl) => tbl[raw.expect_val::<1>(ind)?.get_usize()],
            InsnClass::Dir2(ind, tbl) => tbl[raw.expect_val::<2>(ind)?.get_usize()],
            InsnClass::Dir4(ind, tbl) => tbl[raw.expect_val::<4>(ind)?.get_usize()],
            InsnClass::Dir5(ind, tbl) => tbl[raw.expect_val::<5>(ind)?.get_usize()],
            InsnClass::Dir6(ind, tbl) => tbl[raw.expect_val::<6>(ind)?.get_usize()],
            InsnClass::Dir10(ind, tbl) => {
                *tbl.get(&raw.expect_val::<10>(ind)?.get_u16())
                    .unwrap_or(&InsnClass::Unk)
            }
        };
    }
}

#[derive(Clone, Copy, Default)]
pub struct Suffix {
    u: bool,
    x: bool,
    d: bool,
    w: bool,
    i: bool,
    oe: bool,
    lk: bool,
    aa: bool,
    rc: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Number {
    S(i32),
    U(u32),
}

impl Number {
    pub const fn as_u32(self) -> u32 {
        match self {
            Self::S(i) => i as u32,
            Self::U(u) => u,
        }
    }
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::S(n) => write!(f, "{n}"),
            Self::U(n) => write!(f, "{n}"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Operand<S> {
    Reg(Num<5>),
    FReg(Num<5>),
    CReg(Num<3>),
    QReg(bool, Num<3>),
    Mem(RelValue<S, 16>, Num<5>),
    Num(Number),
    Rel(RelValue<S, 32>),
    Sym(RelValue<S, 32>),
}

impl<Ctx, S> interop::CDump<Ctx> for Operand<S> where
    S: interop::CDump<Ctx>
{
    type Error = S::Error;

    fn dump(&self, f: &mut interop::Dumper, ctx: &Ctx) -> Result<(), Self::Error> {
        match self {
            Operand::Reg(r) => write!(f, "r{r}")?,
            Operand::FReg(r) => write!(f, "{r}")?,
            Operand::CReg(r) => write!(f, "{r}")?,
            Operand::QReg(w, r) => write!(f, "{}, {r}", *w as u32)?,
            Operand::Mem(off, r) => {
                off.dump(f, ctx)?;
                write!(f, "(r{r})")?;
            }
            Operand::Num(off) => write!(f, "{off}")?,
            Operand::Rel(s) => s.dump(f, ctx)?,
            Operand::Sym(s) => s.dump(f, ctx)?,
        }

        Ok(())
    }
}


const OPER_IND: [u8; 5] = [6, 11, 16, 21, 26];
impl<S: Clone> Operand<S> {
    pub fn mem_oper<const IMM_BITS: u8, const IMM_OFF: u8>(
        raw: &RawInsn<S>,
        op_ind: usize,
    ) -> Res<Self> where
        B<{IMM_BITS + IMM_OFF <= 32}>: Assert,
        B<{IMM_OFF >= 16}>: Assert,
        B<{32 - IMM_OFF <= 16}>: Assert,
    {
        let reg = raw.expect_val::<5>(OPER_IND[op_ind])?;
        let imm = raw.get::<{32 - IMM_OFF}>(ImmOpType::S, IMM_OFF)
            .map_num(|_, n| Num(n.get_u32() & !((1<<(32-IMM_OFF-IMM_BITS)) - 1)));
        Ok(Self::Mem(imm, reg))
    }

    pub fn reg_opers_opt<const SIZE: usize>(
        raw: &RawInsn<S>,
        reg: [Option<RegOpType>; SIZE],
        suffixes: &mut Suffix,
    ) -> Res<[Option<Self>; SIZE]> {
        let mut ret = [0; SIZE].map(|_| None);
        for (i, reg) in reg.into_iter().enumerate() {
            ret[i] = reg.and_then(|reg| {
                Self::reg_oper(raw, reg, i, suffixes).transpose()
            }).transpose()?;
        }
        Ok(ret)
    }

    pub fn reg_opers<const SIZE: usize>(
        raw: &RawInsn<S>,
        reg: [RegOpType; SIZE],
        suffixes: &mut Suffix,
    ) -> Res<[Option<Self>; SIZE]> {
        let mut ret = [0; SIZE].map(|_| None);
        for (i, reg) in reg.into_iter().enumerate() {
            ret[i] = Self::reg_oper(raw, reg, i, suffixes)?;
        }
        Ok(ret)
    }

    pub fn reg_oper(
        raw: &RawInsn<S>,
        reg: RegOpType,
        op_ind: usize,
        suffixes: &mut Suffix,
    ) -> Res<Option<Self>> {
        let op = raw.expect_val::<5>(OPER_IND[op_ind])?;
        Ok(Some(match reg {
            RegOpType::C => Self::CReg(op.bits::<2, 5>()),
            RegOpType::CL => {
                if op.bit::<0>().get_bool() {
                    suffixes.d = true;
                } else {
                    suffixes.w = true;
                }
                Self::CReg(op.bits::<2, 5>())
            }
            RegOpType::QW => {
                suffixes.u = op.bit::<0>().get_bool();
                Self::QReg(op.bit::<4>().get_bool(), op.bits::<1, 4>())
            }
            RegOpType::R => Self::Reg(op),
            RegOpType::F => Self::FReg(op),
            RegOpType::N => Self::Num(Number::U(op.get_u32())),
            RegOpType::Oe => {
                suffixes.oe = raw.expect_val(21)?.get_bool();
                return Ok(None);
            }
            RegOpType::H => {
                let sh_ext = raw.expect_val::<1>(30)?;
                Self::Num(Number::U(sh_ext.combine(op).get_u32()))
            }
            RegOpType::U_ => Self::Num(Number::U(op.bits::<1, 5>().get_u32())),
            RegOpType::M64 => {
                let mask_ext = raw.expect_val::<1>(OPER_IND[op_ind] + 5)?;
                Self::Num(Number::U(mask_ext.combine(op).get_u32()))
            }
            RegOpType::Spr => {
                let top = raw.expect_val::<5>(16)?;
                Self::Num(Number::U(top.combine(op).get_u32()))
            }
            RegOpType::Fm => {
                Self::Num(Number::U(raw.expect_val::<8>(OPER_IND[op_ind] + 1)?.get_u32()))
            }
        }))
    }

    pub fn imm_rel_oper<const BITS: u8>(
        rel: RelValue<S, BITS>,
        aa: Option<bool>,
    ) -> Res<Self> {
        let (is_rel, assert_16bits) = match &rel {
            // TODO: if aa == Some(false) ensure that we are getting a relative
            // address
            RelValue::Value(_, _) => (None, false),
            RelValue::Symbol(_) => (Some(false), false),
            RelValue::SymbolLo(_) => (Some(false), true),
            RelValue::SymbolHi(_) => (Some(false), true),
            RelValue::SymbolHa(_) => (Some(false), true),
            RelValue::SymbolRel(_) => (Some(true), false),
            RelValue::Unknown => (None, false),
        };

        if let Some(assert_rel) = is_rel {
            if Some(assert_rel) == aa {
                if assert_rel {
                    bail!(BadAbsolute);
                } else {
                    bail!(BadRelative);
                }
            }
        }

        if assert_16bits && BITS != 16 {
            bail!(NotBit16);
        }

        Ok(if aa.is_some() {
            Self::Rel(rel.map_num(|sign, i| Num(i.get_num(sign).as_u32())))
        } else {
            Self::Sym(rel.map_num(|sign, i| Num(i.get_num(sign).as_u32())))
        })
    }
}

impl<S: Clone> Instruction<S> {
    pub fn parse(raw: &RawInsn<S>) -> Res<Self> {
        let (base_name, desc) = lookup_insn(raw)?;
        let mut suff = Suffix::default();
        let ops = match desc {
            InsnDesc::D(r1, r2, imm, swap) => {
                let [r1, r2] = Operand::reg_opers(raw, [r1, r2], &mut suff)?;
                let imm = Operand::imm_rel_oper(
                    raw.get::<16>(imm, 16),
                    None,
                )?;
                if swap {
                    vec![r2, r1, Some(imm)]
                } else {
                    vec![r1, r2, Some(imm)]
                }
            },
            InsnDesc::DMem(r1) => {
                let r1 = Operand::reg_oper(raw, r1, 0, &mut suff)?;
                let m2 = Some(Operand::mem_oper::<16, 16>(raw, 1)?);
                vec![r1, m2]
            },
            InsnDesc::DSMem(r1) => {
                let r1 = Operand::reg_oper(raw, r1, 0, &mut suff)?;
                let m2 = Operand::mem_oper::<14, 16>(raw, 1)?;
                vec![r1, Some(m2)]
            },
            InsnDesc::DQMem => {
                let r1 = Operand::reg_oper(raw, RegOpType::R, 0, &mut suff)?;
                let m2 = Operand::mem_oper::<12, 20>(raw, 1)?;
                let r3 = Operand::reg_oper(raw, RegOpType::QW, 2, &mut suff)?;
                vec![r1, Some(m2), r3]
            },
            InsnDesc::B => {
                suff.aa = raw.expect_val(30)?.get_bool();
                suff.lk = raw.expect_val(31)?.get_bool();
                let [bo, bi] = Operand::reg_opers(raw, [RegOpType::N; 2], &mut suff)?;
                let bd = Operand::imm_rel_oper(
                    raw.get::<14>(ImmOpType::S, 16)
                        .map_num(|_, n| n.combine(Num::<2>(0))),
                    Some(suff.aa),
                )?;
                vec![bo, bi, Some(bd)]
            },
            InsnDesc::I => {
                suff.aa = raw.expect_val(30)?.get_bool();
                suff.lk = raw.expect_val(31)?.get_bool();
                let li = Operand::imm_rel_oper(
                    raw.get::<24>(ImmOpType::S, 6)
                        .map_num(|_, n| n.combine(Num::<2>(0))),
                    Some(suff.aa),
                )?;
                vec![Some(li)]
            },
            InsnDesc::M(r1, r2, r3, r4, r5) => {
                suff.rc = raw.expect_val(31)?.get_bool();
                let [r1, r2, r3, r4] = Operand::reg_opers(
                    raw,
                    [r1, r2, r3, r4],
                    &mut suff,
                )?;
                let r5 = r5
                    .and_then(|r5| {
                        Operand::reg_oper(raw, r5, 4, &mut suff).transpose()
                    })
                    .transpose()?;
                vec![r2, r1, r3, r4, r5]
            },
            InsnDesc::SC => {
                let lev = Operand::Num(Number::U(raw.expect_val::<7>(20)?.get_u32()));
                vec![Some(lev)]
            }
            InsnDesc::AX(regs, flag, swap) => {
                if let Some(flag) = flag {
                    let flagval = raw.expect_val(31)?.get_bool();
                    match flag {
                        InsnFlag::Rc => suff.rc = flagval,
                        InsnFlag::Lk => suff.lk = flagval,
                    }
                }
                let [r1, r2, r3, r4] = Operand::reg_opers_opt(raw, regs, &mut suff)?;
                if swap {
                    vec![r2, r1, r4, r3]
                } else {
                    vec![r1, r2, r4, r3]
                }
            },
        };

        let mut name = String::with_capacity(base_name.len() + 5);
        name.push_str(base_name);
        if name.ends_with('i') {
            name.pop();
            suff.i = true;
        }
        if name.ends_with('x') {
            name.pop();
            suff.x = true;
        }
        let pushes = [
            (suff.u, 'u'),
            (suff.x, 'x'),
            (suff.d, 'd'),
            (suff.w, 'w'),
            (suff.i, 'i'),
            (suff.oe, 'o'),
            (suff.lk, 'l'),
            (suff.aa, 'a'),
            (suff.rc, '.'),
        ];
        for (val, c) in pushes {
            if val {
                name.push(c);
            }
        }

        Ok(Self {
            base_name,
            name,
            operands: ops.into_iter()
                .filter_map(|f| f)
                .collect(),
        })
    }
}

impl<Ctx, S> interop::CDump<Ctx> for Instruction<S> where
    S: interop::CDump<Ctx>
{
    type Error = S::Error;

    fn dump(
        &self,
        out: &mut interop::Dumper,
        ctx: &Ctx,
    ) -> Result<(), Self::Error> {
        write!(out, "{} ", self.name)?;
        for (i, op) in self.operands.iter().enumerate() {
            if i > 0 {
                write!(out, ", ")?;
            }
            op.dump(out, ctx)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {

    use std::{error::Error, fs::File};

    use serde::Deserialize;

    use super::{Instruction, RawInsn};

    #[derive(Clone)]
    enum X {}

    impl<T> interop::CDump<T> for X {
        type Error = std::fmt::Error;

        fn dump(
            &self,
            _out: &mut interop::Dumper,
            _ctx: &T,
        ) -> Result<(), Self::Error> {
            match *self {}
        }
    }

    #[derive(Deserialize)]
    struct TestCases {
        decode_tests: Vec<TestCase>,
    }

    #[derive(Deserialize)]
    struct TestCase {
        opcode: u32,
        insn: String,
        operands: Vec<String>,
    }

    #[test]
    fn test_decode() -> Result<(), Box<dyn Error>> {
        let tests: TestCases = serde_yaml::from_reader(File::open("test.yml")?)?;
        for test in tests.decode_tests {
            println!("Testing: {}", test.insn);

            let insn = Instruction::parse(&RawInsn::<X>::Concrete(test.opcode))?;
            assert_eq!(test.insn, insn.name);
            assert_eq!(
                test.operands,
                insn.operands
                    .iter()
                    .map(|s| interop::dumps(s, &()).expect("no format error"))
                    .collect::<Vec<_>>(),
            );
        }
        Ok(())
    }

}
