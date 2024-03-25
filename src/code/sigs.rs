use super::*;

#[allow(unused)]
mod operand_consts {
    macro_rules! mk_const {
        ($($reg:ident $tp:ident $num:expr),* $(,)?) => {
            $(
                pub mod $reg {
                    pub const RAW: ppcdis::Num<5> = ppcdis::Num::new::<$num>();
                    pub const OPERAND: ppcdis::Operand<usize> = ppcdis::Operand::$tp(
                        ppcdis::Num::new::<$num>()
                    );
                }
            )*
        }
    }

    mk_const! {
        r0 Reg 0, r1 Reg 1, r2 Reg 2, r3 Reg 3, r4 Reg 4,
        r5 Reg 5, r6 Reg 6, r7 Reg 7, r8 Reg 8, r9 Reg 9,
        r10 Reg 10, r11 Reg 11, r12 Reg 12, r13 Reg 13, r14 Reg 14,
        r15 Reg 15, r16 Reg 16, r17 Reg 17, r18 Reg 18, r19 Reg 19,
        r20 Reg 20, r21 Reg 21, r22 Reg 22, r23 Reg 23, r24 Reg 24,
        r25 Reg 25, r26 Reg 26, r27 Reg 27, r28 Reg 28, r29 Reg 29,
        r30 Reg 30, r31 Reg 31,
    }

    pub const UNKNOWN: ppcdis::Operand<usize> = r0::OPERAND;
}

macro_rules! operands {
    ($name:ident) => { operand_consts::$name::OPERAND };
    ([$mode:ident $num:expr, $reg:ident]) => {
        ppcdis::Operand::Mem(
            ppcdis::RelValue::Value(
                operands!(optp ImmOpType $mode),
                ppcdis::Num::new::<{$num as i32 as u16 as u32}>(),
            ),
            operand_consts::$reg::RAW,
        )
    };
    ([$mode:ident $num:expr]) => {
        ppcdis::Operand::Num(operands!(optp Number $mode)($num))
    };
    ([$sym:ident]) => {
        operands!([$sym @ n])
    };
    ([$sym:ident @ $symopt:ident]) => {
        ppcdis::Operand::Sym(operands!(symopt $symopt)($sym))
    };
    ([$sym:ident @@ rel]) => {
        ppcdis::Operand::Rel(ppcdis::RelValue::SymbolRel($sym))
    };
    ($tt:tt) => { operands!([u $tt]) };
    (symopt n) => { ppcdis::RelValue::Symbol };
    (symopt lo) => { ppcdis::RelValue::SymbolLo };
    (symopt hi) => { ppcdis::RelValue::SymbolHi };
    (symopt ha) => { ppcdis::RelValue::SymbolHa };
    (symopt rel) => { ppcdis::RelValue::SymbolRel };
    (optp $tp:ident u) => { ppcdis::$tp::U };
    (optp $tp:ident s) => { ppcdis::$tp::S };
}

macro_rules! insns {
    ($($name:ident $($op:tt),*;)*) => {
        [$(InsnTempl {
            name: stringify!($name),
            operands: &[$(operands!($op)),*],
        }),*]
    }
}

static PROLOG_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const ctors: usize = 0;
    #[allow(non_upper_case_globals)]
    const rel_set_evt_addr: usize = 1;
    #[allow(non_upper_case_globals)]
    const area_str: usize = 2;
    #[allow(non_upper_case_globals)]
    const evt: usize = 3;
    CCodeTemplate {
        name: "prolog",
        templ: TemplateRegExp::Concat(&[
            TemplateRegExp::Fragment(CCodeTemplateFragment {
                snippets: &templated!(
                    concat!(
                        "    void (*ctor)();\n",
                        "    for (ctor = &{ctors}; *ctor; ctor++) {{\n",
                        "        (*ctor)();\n",
                        "    }}",
                    )
                ),
                asm: &insns!(
                    stwu r1, [s -16, r1];
                    mfspr r0, 8;
                    addis r3, r0, [ctors@ha];
                    stw r0, [s 20, r1];
                    addi r0, r3, [ctors@lo];
                    stw r31, [s 12, r1];
                    or r31, r0, r0;
                    b 16;
                    mtspr 9, r12;
                    bcctrl 20, 0;
                    addi r31, r31, 4;
                    lwz r12, [s 0, r31];
                    cmplwi 0, r12, 0;
                    bc 4, 2, [s -20];
                ),
            }),
            TemplateRegExp::Rep(&TemplateRegExp::Fragment(CCodeTemplateFragment {
                snippets: &templated!(
                    concat!(
                        "\n    {rel_set_evt_addr}({area_str}, {evt});",
                    )
                ),
                asm: &insns!(
                    addis r3, r0, [area_str@ha];
                    addis r4, r0, [evt@ha];
                    addi r3, r3, [area_str@lo];
                    addi r4, r4, [evt@lo];
                    bl [rel_set_evt_addr@@rel];
                ),
            })),
            TemplateRegExp::Fragment(CCodeTemplateFragment {
                snippets: &[],
                asm: &insns!(
                    lwz r0, [s 20, r1];
                    lwz r31, [s 12, r1];
                    mtspr 8, r0;
                    addi r1, r1, 16;
                    bclr 20, 0;
                ),
            })
        ]),
        args: &[],
        return_type: "void",
    }
};

static EPILOG_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const mapdelete: usize = 0;
    #[allow(non_upper_case_globals)]
    const dtors: usize = 1;
    CCodeTemplate {
        name: "epilog",
        templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
            snippets: &templated!(
                concat!(
                    "    void (*dtor)();\n",
                    "    {mapdelete}();\n",
                    "    for (dtor = &{dtors}; *dtor; dtor++) {{\n",
                    "        (*dtor)();\n",
                    "    }}",
                )
            ),
            asm: &insns!(
                stwu r1, [s -16, r1];
                mfspr r0, 8;
                stw r0, [s 20, r1];
                stw r31, [s 12, r1];
                bl [mapdelete@@rel];
                addis r3, r0, [dtors@ha];
                addi r0, r3, [dtors@lo];
                or r31, r0, r0;
                b 16;
                mtspr 9, r12;
                bcctrl 20, 0;
                addi r31, r31, 4;
                lwz r12, [s 0, r31];
                cmplwi 0, r12, 0;
                bc 4, 2, [s -20];
                lwz r0, [s 20, r1];
                lwz r31, [s 12, r1];
                mtspr 8, r0;
                addi r1, r1, 16;
                bclr 20, 0;
            ),
        }),
        args: &[],
        return_type: "void",
    }
};

static STUB_TEMPL: CCodeTemplate<'static> = CCodeTemplate {
    name: "stub",
    templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
        snippets: &templated!("    // stub function"),
        asm: &insns!(bclr 20, 0;),
    }),
    args: &[],
    return_type: "void",
};

static MAP_DELETE_TEMPL: CCodeTemplate<'static> = {
    #[allow(non_upper_case_globals)]
    const wp: usize = 0;
    #[allow(non_upper_case_globals)]
    const fileFree: usize = 1;
    CCodeTemplate {
        name: "mapdelete",
        templ: TemplateRegExp::Fragment(CCodeTemplateFragment {
            snippets: &templated!(
                concat!(
                    "    if ({wp} != NULL && {wp}->texture != NULL) {{\n",
                    "        {fileFree}({wp}->texture);\n",
                    "    }}",
                )
            ),
            asm: &insns!(
                stwu r1, [s -16, r1];
                mfspr r0, 8;
                addis r3, r0, [wp@ha];
                stw r0, [s 20, r1];
                addi r3, r3, [wp@lo];
                lwz r3, [s 0, r3];
                cmplwi 0, r3, 0;
                bc 12, 2, 20;
                lwz r3, [s 0, r3];
                cmplwi 0, r3, 0;
                bc 12, 2, 8;
                bl [fileFree@@rel];
                lwz r0, [s 20, r1];
                mtspr 8, r0;
                addi r1, r1, 16;
                bclr 20, 0;
            ),
        }),
        args: &[],
        return_type: "void",
    }
};

static DEFAULT_SIGS: &[&'static CCodeTemplate<'static>] = &[
    &PROLOG_TEMPL,
    &EPILOG_TEMPL,
    &STUB_TEMPL,
    &MAP_DELETE_TEMPL,
];

impl Code {
    pub fn find_default_sig(
        &self,
        base_addr: sym::SymAddr,
        symdb: &sym::SymbolDatabase,
    ) -> Option<CCode> {
        for sig in DEFAULT_SIGS {
            if let Some(res) = self.match_template(sig, base_addr) {
                return Some(res);
            }
        }

        let mut fuzzy_matched = false;
        for sig in DEFAULT_SIGS {
            let res = self.match_fuzzy_template(sig, base_addr);
            if res.insn_count != 0 {
                let fuzzy = 100.0 - res.edit_dist as f32 / (res.insn_count *
                    EDIT_INSN_UNIT) as f32 * 100.0;
                fuzzy_matched = true;
                eprintln!(
                    "WARNING: {} does not match any signatures, but fuzzy matches `{}` \
                    at {fuzzy:.2}% ({})",
                    symdb.symbol_name(base_addr, false),
                    sig.name,
                    res.edit_dist,
                )
            }
        }
        if fuzzy_matched {
            let area = match base_addr {
                sym::SymAddr::Dol(_) => 0,
                sym::SymAddr::Rel(area, _) => area,
            };
            let strings = sym::StringsMap::default();
            let ctx = sym::AddrDumpCtx::new(
                area,
                &interop::ctype!(mut [i32]),
                symdb,
                &strings,
            );
            let asm = match interop::dumps(self, &ctx) {
                Ok(s) => s,
                Err(_) => "<error>".into(),
            };
            eprintln!("  for reference, here is the code:\n{asm}");
        }
        None
    }
}
