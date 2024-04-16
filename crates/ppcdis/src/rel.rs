use num_derive::FromPrimitive;

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, FromPrimitive)]
pub enum RelocType {
    /// Do nothing. Skip this entry.
    PPCNone = 0,
    /// Writes a 32-bit value of the symbol address.
    ///
    /// word32; S + A
    PPCAddr32,
    /// Writes the 24-bit value of the symbol address shifted right. Fails if does not fit.
    ///
    /// low24_32; (S + A) >> 2
    PPCAddr24,
    /// Writes the 16-bit value of the symbol address. Fails if does not fit.
    ///
    /// half16; S + A
    PPCAddr16,
    /// Writes the low 16-bit value of the symbol address.
    ///
    /// half16; #lo(S + A)
    PPCAddr16Lo,
    /// Writes the high 16-bit value of the symbol address.
    ///
    /// half16; #hi(S + A)
    PPCAddr16Hi,
    /// Writes the high 16-bit value of the symbol address plus 0x8000. Accounts
    /// for when the low 16-bit is signed.
    ///
    /// half16; #ha(S + A)
    PPCAddr16Ha,
    /// Writes the 14-bit value of the symbol address shifted right. Fails if does not fit.
    ///
    /// low14_32; (S + A) >> 2
    PPCAddr14,
    /// Writes the 14-bit value of the symbol address shifted right. Fails if does not fit.
    ///
    /// low14_32; (S + A) >> 2; mark with branch taken
    PPCAddr14BrTaken,
    /// Writes the 14-bit value of the symbol address shifted right. Fails if does not fit.
    ///
    /// low14_32; (S + A) >> 2; mark with branch not taken
    PPCAddr14BrNotTaken,
    /// Writes the 24-bit value of the symbol address shifted right, relative to where this
    /// relocation is occuring. Fails if does not fit.
    ///
    /// low24_32; (S + A - P) >> 2
    PPCRel24,
    /// Writes the 14-bit value of the symbol address shifted right, relative to where this
    /// relocation is occuring. Fails if does not fit.
    ///
    /// low14_32; (S + A - P) >> 2
    PPCRel14,
    /// Writes the 14-bit value of the symbol address shifted right, relative to where this
    /// relocation is occuring. Fails if does not fit.
    ///
    /// low14_32; (S + A - P) >> 2; mark with branch taken
    PPCRel14BrTaken,
    /// Writes the 14-bit value of the symbol address shifted right, relative to where this
    /// relocation is occuring. Fails if does not fit.
    ///
    /// low14_32; (S + A - P) >> 2; mark with branch not taken
    PPCRel14BrNotTaken,

    /// word32; (S + A - P)
    PPCRel32 = 26,

    /// Do not relocate anything, but accumulate the `offset` field for the next relocation offset
    /// calculation. These types are used for referring to relocations that are more than `0xffff`
    /// apart from each other.
    RvlNone = 201,
    /// Change which section relocations are being applied to. Set the offset into the section to 0.
    RvlSect,
    /// Stop parsing the relocation list.
    RvlEnd,
}

#[derive(Debug)]
pub enum RelocAction {
    None,
    Write32 { val: u32, mask: u32 },
    Write16 { val: u16, mask: u16 },
    Sect,
    End,
}

impl RelocType {
    pub fn is_rel(&self) -> bool {
        match self {
            RelocType::PPCNone => false,
            RelocType::PPCAddr32 => false,
            RelocType::PPCAddr24 => false,
            RelocType::PPCAddr16 => false,
            RelocType::PPCAddr16Lo => false,
            RelocType::PPCAddr16Hi => false,
            RelocType::PPCAddr16Ha => false,
            RelocType::PPCAddr14 => false,
            RelocType::PPCAddr14BrTaken => false,
            RelocType::PPCAddr14BrNotTaken => false,
            RelocType::PPCRel24 => true,
            RelocType::PPCRel14 => true,
            RelocType::PPCRel14BrTaken => true,
            RelocType::PPCRel14BrNotTaken => true,
            RelocType::PPCRel32 => true,
            RelocType::RvlNone => false,
            RelocType::RvlSect => false,
            RelocType::RvlEnd => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            RelocType::PPCNone => 0,
            RelocType::PPCAddr32 => 4,
            RelocType::PPCAddr24 => 4,
            RelocType::PPCAddr16 => 2,
            RelocType::PPCAddr16Lo => 2,
            RelocType::PPCAddr16Hi => 2,
            RelocType::PPCAddr16Ha => 2,
            RelocType::PPCAddr14 => 4,
            RelocType::PPCAddr14BrTaken => 4,
            RelocType::PPCAddr14BrNotTaken => 4,
            RelocType::PPCRel24 => 4,
            RelocType::PPCRel14 => 4,
            RelocType::PPCRel14BrTaken => 4,
            RelocType::PPCRel14BrNotTaken => 4,
            RelocType::PPCRel32 => 4,
            RelocType::RvlNone => 0,
            RelocType::RvlSect => 0,
            RelocType::RvlEnd => 0,
        }
    }

    pub fn reloc_bits(&self) -> u8 {
        match self {
            RelocType::PPCNone => 0,
            RelocType::PPCAddr32 => 32,
            RelocType::PPCAddr24 => 24,
            RelocType::PPCAddr16 => 16,
            RelocType::PPCAddr16Lo => 16,
            RelocType::PPCAddr16Hi => 16,
            RelocType::PPCAddr16Ha => 16,
            RelocType::PPCAddr14 => 14,
            RelocType::PPCAddr14BrTaken => 14,
            RelocType::PPCAddr14BrNotTaken => 14,
            RelocType::PPCRel24 => 24,
            RelocType::PPCRel14 => 14,
            RelocType::PPCRel14BrTaken => 14,
            RelocType::PPCRel14BrNotTaken => 14,
            RelocType::PPCRel32 => 32,
            RelocType::RvlNone => 0,
            RelocType::RvlSect => 0,
            RelocType::RvlEnd => 0,
        }
    }

    pub fn mask(&self) -> u32 {
        match self.eval(0, Some(0)).expect("should not be none") {
            RelocAction::None => 0,
            RelocAction::Write32 { mask, .. } => mask,
            RelocAction::Write16 { mask, .. } => mask.into(),
            RelocAction::Sect => 0,
            RelocAction::End => 0,
        }
    }

    pub fn eval_always(&self, target: u32, site: u32) -> RelocAction {
        self.eval(target, Some(site)).expect("Should give action")
    }

    /// Evaluates a relocation type based on the target address to relocate to
    /// and the address site where the relocation is taking place. This returns
    /// a relocation action that either maps to a special action or an action
    /// to write a masked 32/16-bit value
    pub fn eval(&self, target: u32, site: Option<u32>) -> Option<RelocAction> {
        const MASK_32: u32 = 0xffffffff;
        const MASK_24: u32 = 0x03fffffc;
        const MASK_16: u16 = 0xffff;
        const MASK_14_32: u32 = 0x0000fffc;
        const BIT_TAKEN: u32 = 1 << 21;
        const SHF_HI: usize = 16;
        const ADJ: u32 = 0x8000;
        Some(match self {
            RelocType::PPCNone => RelocAction::None,
            RelocType::PPCAddr32 => RelocAction::Write32 {
                val: target,
                mask: MASK_32,
            },
            RelocType::PPCAddr24 => RelocAction::Write32 {
                val: target & MASK_24,
                mask: MASK_24,
            },
            RelocType::PPCAddr16 => RelocAction::Write16 {
                val: target as u16,
                mask: MASK_16,
            },
            RelocType::PPCAddr16Lo => RelocAction::Write16 {
                val: target as u16,
                mask: MASK_16,
            },
            RelocType::PPCAddr16Hi => RelocAction::Write16 {
                val: (target >> SHF_HI) as u16,
                mask: MASK_16,
            },
            RelocType::PPCAddr16Ha => RelocAction::Write16 {
                val: ((target + ADJ) >> SHF_HI) as u16,
                mask: MASK_16,
            },
            RelocType::PPCAddr14 => RelocAction::Write32 {
                val: target & MASK_14_32,
                mask: MASK_14_32,
            },
            RelocType::PPCAddr14BrTaken => RelocAction::Write32 {
                val: (target & MASK_14_32) | BIT_TAKEN,
                mask: MASK_14_32 | BIT_TAKEN,
            },
            RelocType::PPCAddr14BrNotTaken => RelocAction::Write32 {
                val: target & MASK_14_32,
                mask: MASK_14_32 | BIT_TAKEN,
            },
            RelocType::PPCRel24 => RelocAction::Write32 {
                val: target.wrapping_sub(site?) & MASK_24,
                mask: MASK_24,
            },
            RelocType::PPCRel14 => RelocAction::Write32 {
                val: target.wrapping_sub(site?) & MASK_14_32,
                mask: MASK_14_32,
            },
            RelocType::PPCRel14BrTaken => RelocAction::Write32 {
                val: (target.wrapping_sub(site?) & MASK_14_32) | BIT_TAKEN,
                mask: MASK_14_32 | BIT_TAKEN,
            },
            RelocType::PPCRel14BrNotTaken => RelocAction::Write32 {
                val: target.wrapping_sub(site?) & MASK_14_32,
                mask: MASK_14_32 | BIT_TAKEN,
            },
            RelocType::PPCRel32 => RelocAction::Write32 {
                val: target.wrapping_sub(site?),
                mask: MASK_32,
            },
            RelocType::RvlNone => RelocAction::None,
            RelocType::RvlSect => RelocAction::Sect,
            RelocType::RvlEnd => RelocAction::End,
        })
    }
}
