use std::{
    collections::{
        btree_map as tm,
        hash_map as hm,
        BTreeMap,
        HashMap,
    },
    mem::size_of,
};

use bytemuck::{Pod, Zeroable};
use num::FromPrimitive;
use num_derive::FromPrimitive;

use crate::{mk_err_wrapper, utils::assert_sane_size};

// Sanity check to ensure that usize is as big (if not bigger) than u32
const _: () = assert!(size_of::<usize>() >= size_of::<u32>());


#[derive(Debug, thiserror::Error)]
pub enum ErrorType {
    #[error("Bad section number ({0})")]
    BadSection(u8),
    #[error("Bad offset in section (0x{0:x})")]
    BadOffset(u32),
    #[error("Invalid relocation type ({0})")]
    InvalidRelocType(u8),
    #[error("No section specified for relocations")]
    NoSection,
    #[error("Relocation is too complex")]
    ComplexRelocation,
}

mk_err_wrapper! { ErrorType }

macro_rules! mk_be_int {
    ($($name:ident: $ty:ty),* $(,)?) => {
        $(#[repr(C)]
        #[derive(Pod, Zeroable, Default, Clone, Copy)]
        pub struct $name {
            int: $ty
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_tuple(stringify!($name)).field(&self.get()).finish()
            }
        }

        impl serde::Serialize for $name {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error> where
                S: serde::Serializer
            {
                self.get().serialize(serializer)
            }
        }

        #[allow(unused)]
        impl $name {
            pub fn new(val: $ty) -> Self {
                Self { int: <$ty>::to_be(val) }
            }

            pub fn get(&self) -> $ty {
                <$ty>::from_be(self.int)
            }
        })*
    }
}

mk_be_int! {
    BigU16: u16,
    BigU32: u32,
    BigU64: u64,
}

#[repr(C)]
#[derive(Pod, Zeroable, Default, Clone, Copy, Debug)]
pub struct RelHeader {
    pub id: BigU32,
    /// Pointer to next module, filled at runtime.
    pub next: BigU32,
    /// Pointer to previous module, filled at runtime.
    pub prev: BigU32,
    /// Number of sections in file.
    pub num_sections: BigU32,
    /// Offset to the start of the section table
    pub section_info_offset: BigU32,
    /// Offset to the ASCII string containing the path of this module's `.plf` file. Relative to
    /// the start of the `.str` file.
    pub name_offset: BigU32,
    /// Length of the path of this module's `.plf` file.
    pub name_size: BigU32,
    /// Version number of the REL file format.
    pub version: BigU32,
    /// Size of the `.bss` section
    pub bss_size: BigU32,
    /// Offset to the first relocation list.
    pub rel_offset: BigU32,
    /// Offset to the imp table.
    pub imp_offset: BigU32,
    /// Size of the imp table in bytes.
    pub imp_size: BigU32,
    /// Index into section table which `prolog` is relative to. Skip if this field is 0.
    pub prolog_section: u8,
    /// Index into section table which `epilog` is relative to. Skip if this field is 0.
    pub epilog_section: u8,
    /// Index into section table which `unresolved` is relative to. Skip if this field is 0.
    pub unresolved_section: u8,
    /// Index into section table which `bss` is relative to. Filled at runtime!
    pub bss_section: u8,
    /// Offset into specified section of the `_prolog` function.
    pub prolog: BigU32,
    /// Offset into specified section of the `_epilog` function.
    pub epilog: BigU32,
    /// Offset into specified section of the `_unresolved` function.
    pub unresolved: BigU32,
    /// Version ≥ 2 only. Alignment constraint of the load address of the REL, expressed as power
    /// of 2.
    pub align: BigU32,
    /// Version ≥ 2 only. Alignment constraint on all `.bss` section, expressed as power of 2.
    pub bss_align: BigU32,
    /// Version ≥ 3 only. If REL is linked with OSLinkFixed (instead of OSLink), the space after
    /// this address can be used for other purposes (like BSS).
    pub fix_size: BigU32,
}

#[repr(C)]
#[derive(Pod, Zeroable, Default, Clone, Copy, Debug)]
pub struct SectionHeader {
    offset: BigU32,
    length: BigU32,
}

impl SectionHeader {
    /// Offset from the beginning of the REL to the section. If this is None, the section is an
    /// uninitialized section (i.e. .bss). 
    pub fn offset(&self) -> Option<u32> {
        let val = self.offset.get() & !1;
        if val == 0 {
            None
        } else {
            Some(val)
        }
    }

    /// Whether if the section is executable or not. Implementation-wise, this is the LSB of offset
    /// field.
    pub fn is_exec(&self) -> bool {
        (self.offset.get() & 1) != 0
    }

    /// Length in bytes of the section. If this is zero, this entry is skipped.
    pub fn length(&self) -> u32 {
        self.length.get()
    }
}

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SectionAddr {
    pub sect: u8,
    pub offset: u32,
}

impl std::fmt::Display for SectionAddr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:08x}", self.sect, self.offset)
    }
}

impl SectionAddr {
    pub fn new(sect: u8, offset: u32) -> Self {
        Self { sect, offset }
    }

    pub fn var_name(&self) -> String {
        format!("v_s{}_{:08x}", self.sect, self.offset)
    }
}

impl std::ops::Add<u32> for SectionAddr {
    type Output = Self;

    fn add(self, rhs: u32) -> Self::Output {
        Self::Output {
            sect: self.sect,
            offset: rhs + self.offset,
        }
    }
}

impl std::ops::AddAssign<u32> for SectionAddr {
    fn add_assign(&mut self, rhs: u32) {
        self.offset += rhs;
    }
}

impl std::ops::Sub<u32> for SectionAddr {
    type Output = Self;

    fn sub(self, rhs: u32) -> Self::Output {
        Self::Output {
            sect: self.sect,
            offset: self.offset - rhs,
        }
    }
}

impl std::ops::SubAssign<u32> for SectionAddr {
    fn sub_assign(&mut self, rhs: u32) {
        self.offset -= rhs;
    }
}

#[repr(C)]
#[derive(Pod, Zeroable, Default, Clone, Copy, Debug)]
/// An entry to the imp table. The imp acts as a directory for the relocation information. It
/// specifies the offset of each relocation list and which module it refers to.
pub struct ImpEntry {
    id: BigU32,
    offset: BigU32,
}

impl ImpEntry {
    /// Module number that these relocations refer to. 0 means these refer to main.dol instead.
    fn id(&self) -> u32 {
        self.id.get()
    }
    
    /// Offset from the beginning of the REL to the relocation data.
    fn offset(&self) -> u32 {
        self.offset.get()
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[derive(FromPrimitive)]
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
    Write32 {
        val: u32,
        mask: u32,
    },
    Write16 {
        val: u16,
        mask: u16,
    },
    Sect,
    End,
}

impl RelocType {
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

    /// Evaluates a relocation type based on the target address to relocate to
    /// and the address site where the relocation is taking place. This returns
    /// a relocation action that either maps to a special action or an action
    /// to write a masked 32/16-bit value
    pub fn eval(&self, target: u32, site: Option<u32>) -> Option<RelocAction> {
        const MASK_32: u32 = 0xffffffff;
        const MASK_24: u32 = 0x03fffffc;
        const MASK_16: u16 = 0xffff;
        const MASK_14_32: u32 = 0x0000fffc;
        const BIT_TAKEN: u32 = 1<<21;
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
            RelocType::RvlNone => RelocAction::None,
            RelocType::RvlSect => RelocAction::Sect,
            RelocType::RvlEnd => RelocAction::End,
        })
    }
}

#[repr(C)]
#[derive(Pod, Zeroable, Default, Clone, Copy)]
/// An entry for relocation in a reloc table. The beginning of each relocation table is specified
/// by the imp table.
pub struct RelocEntry {
    offset: BigU16,
    type_: u8,
    section: u8,
    addend: BigU32,
}

impl RelocEntry {
    /// Offset in bytes from the previous relocation to this one. If this is 
    /// the first relocation in the section, this is relative to the section start.
    pub fn offset(&self) -> u16 {
        self.offset.get()
    }

    /// The section of the symbol to relocate against. For the special relocation type 202, this is
    /// instead the number of the section in this file which the following relocation entries apply
    /// to.
    pub fn section(&self) -> u8 {
        self.section
    }

    /// The relocation type.
    pub fn rtype(&self) -> Result<RelocType, u8> {
        RelocType::from_u8(self.type_)
            .ok_or(self.type_)
    }

    /// Offset in bytes of the symbol to relocate against, relative to the start of its section.
    /// This is an absolute address instead for relocations against main.dol.
    pub fn addend(&self) -> u32 {
        self.addend.get()
    }
}

impl std::fmt::Debug for RelocEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Reloc(type: {}, offset: {}, section: {}, addend: 0x{:08x})",
            self.type_,
            self.offset.get(),
            self.section,
            self.addend.get(),
        )
    }
}

pub struct RelFile<'b> {
    data: &'b [u8],
    header: RelHeader,
    sections: Vec<SectionHeader>,
    imp: Vec<ImpEntry>,
    relocs: HashMap<u32, Vec<RelocEntry>>,
}

enum Size {
    Length(usize),
    Bytes(usize),
}

fn slice_from_bytes<T: Pod>(data: &[u8], offset: usize, len: Size) -> &[T] {
    let size = match len {
        Size::Length(len) => size_of::<T>() * len,
        Size::Bytes(sz) => sz,
    };
    bytemuck::cast_slice(&data[offset..offset + size])
}

impl<'b> RelFile<'b> {
    pub fn new(data: &'b [u8]) -> Self {
        let header: RelHeader = bytemuck::pod_read_unaligned(
            &data[0..size_of::<RelHeader>()]
        );

        let sections = slice_from_bytes(
            data,
            header.section_info_offset.get() as usize,
            Size::Length(header.num_sections.get() as usize),
        ).to_vec();

        let imp: Vec<ImpEntry> = slice_from_bytes(
            data,
            header.imp_offset.get() as usize,
            Size::Bytes(header.imp_size.get() as usize),
        ).to_vec();

        let mut relocs = HashMap::new();
        let mut reloc_off = header.rel_offset.get() as usize;

        for ent in &imp {
            let tbl = match relocs.entry(ent.id()) {
                hm::Entry::Occupied(ent) => ent.into_mut(),
                hm::Entry::Vacant(ent) => ent.insert(Vec::new()),
            };
            loop {
                let ent: RelocEntry = *bytemuck::from_bytes(
                    &data[reloc_off..reloc_off + size_of::<RelocEntry>()]
                );
                tbl.push(ent);
                reloc_off += size_of::<RelocEntry>();
                if ent.type_ == RelocType::RvlEnd as u8 {
                    break;
                }
            }
        }
        Self {
            data,
            header,
            sections,
            imp,
            relocs,
        }
    }

    pub fn header(&self) -> &RelHeader {
        &self.header
    }

    pub fn section_header(&self, id: u8) -> Option<&SectionHeader> {
        self.sections.get(usize::from(id))
    }

    pub fn relocations(&self) -> impl Iterator<Item = (u32, &RelocEntry)> {
        self.relocs
            .iter()
            .flat_map(|(id, relocs)| relocs.into_iter().map(move |r| (*id, r)))
    }

    pub fn relocations_for_file(&self, id: u32) -> impl Iterator<Item = &RelocEntry> {
        self.relocs.get(&id)
            .map_or_else(|| [].iter(), |r| r.iter())
    }

    pub fn imp_tbl(&self) -> impl Iterator<Item = &ImpEntry> {
        self.imp.iter()
    }

    pub fn slice_of(
        &self,
        addr: SectionAddr,
        size: u32,
    ) -> Result<&[u8], Error> {
        let sect = self.sections
            .get(usize::from(addr.sect))
            .ok_or_else(|| error!(BadSection(addr.sect)))?;

        let sect_offset = sect
            .offset()
            .ok_or_else(|| error!(BadSection(addr.sect)))? as usize;

        let end = addr.offset
            .checked_add(size)
            .ok_or_else(|| error!(BadOffset(addr.offset)))?;

        if end > sect.length() {
            Err(error!(BadOffset(addr.offset)))
        } else {
            Ok(&self.data[sect_offset + addr.offset as usize..sect_offset + end as usize])
        }
    }

    pub fn read<T: Pod>(&self, addr: SectionAddr) -> Result<T, Error> {
        assert_sane_size::<T>();
        Ok(bytemuck::pod_read_unaligned(self.slice_of(addr, size_of::<T>() as u32)?))
    }

    pub fn dump_headers(&self) {
        println!("{:#?}", self.header);
        println!("{:#?}", self.sections);
        println!("{:#?}", self.imp);
        println!("{:#?}", self.relocs);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum RelocBacking {
    Val16(u16),
    Val32(u32),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocSymbol {
    pub file: u32,
    pub target: SectionAddr,
    pub rtype: RelocType,
    pub orig: Option<RelocBacking>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Symbol<T> {
    Value(T),
    Partial,
    Unknown,
    Rel(RelocSymbol),
}

impl<T> Symbol<T> {
    pub fn map<U>(self, trans: impl FnOnce(T) -> U) -> Symbol<U> {
        match self {
            Symbol::Value(v) => Symbol::Value(trans(v)),
            Symbol::Partial => Symbol::Partial,
            Symbol::Unknown => Symbol::Unknown,
            Symbol::Rel(rsym) => Symbol::Rel(rsym),
        }
    }

    pub fn get_value(self) -> Res<T> {
        match self {
            Symbol::Value(v) => Ok(v),
            _ => Err(error!(ComplexRelocation)),
        }
    }
}

#[derive(Debug)]
enum SymbolicActions {
    Concrete(RelocAction),
    RelocSymbol(RelocType),
}

pub struct RelocOverlay<'r, 'b> {
    backing: &'r RelFile<'b>,
    relocs: BTreeMap<SectionAddr, Symbol<u8>>,
}

impl<'r, 'b> RelocOverlay<'r, 'b> {
    pub fn new(backing: &'r RelFile<'b>) -> Self {
        Self {
            backing,
            relocs: BTreeMap::new()
        }
    }

    pub fn backing(&self) -> &'r RelFile<'b> {
        self.backing
    }

    pub fn resolve_relocations(
        &mut self,
        resolved: &HashMap<(u32, u8), u32>,
        has_dol: bool,
    ) -> Result<(), Error> {
        let mut cur_addr: Option<SectionAddr> = None;
        for (file, rel) in self.backing.relocations() {
            let rtype = rel.rtype()
                    .map_err(|rt| error!(InvalidRelocType(rt)))?;
            let action = if has_dol && file == 0 {
                rtype.eval(rel.addend(), None)
                    .map_or_else(
                        || SymbolicActions::RelocSymbol(rtype),
                        SymbolicActions::Concrete,
                    )
            } else if let Some(base) = resolved.get(&(file, rel.section())) {
                rtype.eval(base + rel.addend(), None)
                    .map_or_else(
                        || SymbolicActions::RelocSymbol(rtype),
                        SymbolicActions::Concrete,
                    )
            } else {
                match rtype {
                    RelocType::PPCNone => SymbolicActions::Concrete(RelocAction::None),
                    RelocType::RvlNone => SymbolicActions::Concrete(RelocAction::None),
                    RelocType::RvlSect => SymbolicActions::Concrete(RelocAction::Sect),
                    RelocType::RvlEnd => SymbolicActions::Concrete(RelocAction::End),
                    r => SymbolicActions::RelocSymbol(r),
                }
            };

            cur_addr.as_mut().map(|a| *a += rel.offset.get().into());

            match action {
                SymbolicActions::Concrete(action) => match action {
                    RelocAction::None => {},
                    RelocAction::Write32 { val, mask } => {
                        let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                        if mask == !0 {
                            self.write_32(addr, val);
                        } else if mask != 0 {
                            let old_val = self.read_32(addr)?;
                            if let Symbol::Value(old_val) = old_val {
                                self.write_32(addr, (old_val & !mask) | (val & mask));
                            } else {
                                self.write_reloc(addr, None, RelocType::PPCAddr32)?;
                            }
                        }
                    }
                    RelocAction::Write16 { val, mask } => {
                        let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                        if mask == !0 {
                            self.write_16(addr, val);
                        } else if mask != 0 {
                            let old_val = self.read_16(addr)?;
                            if let Symbol::Value(old_val) = old_val {
                                self.write_16(addr, (old_val & !mask) | (val & mask));
                            } else {
                                self.write_reloc(addr, None, RelocType::PPCAddr16)?;
                            }
                        }
                    }
                    RelocAction::Sect => {
                        cur_addr = Some(SectionAddr::new(rel.section(), 0));
                    }
                    RelocAction::End => {
                        cur_addr = None;
                        continue;
                    }
                }
                SymbolicActions::RelocSymbol(rtype) => {
                    let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                    self.write_reloc(addr, Some((file, SectionAddr::new(
                        rel.section(),
                        rel.addend(),
                    ))), rtype)?;
                }
            }
        }

        Ok(())
    }

    fn invalidate_write(&mut self, addr: SectionAddr, size: usize) {
        // If there is a partial value that overflows through addr, make sure
        // that we invalidate that value
        let mut off = 0;
        while let tm::Entry::Occupied(mut ent) = self.relocs.entry(addr - off) {
            match ent.insert(Symbol::Unknown) {
                // Continue until we find the relocation entry associated with
                // these partials.
                Symbol::Partial => off += 1,
                // We reached a termination point
                Symbol::Unknown | Symbol::Rel(_) => {
                    break;
                }
                // Oops this shouldn't have been removed! Also reached
                // termination point
                s@Symbol::Value(_) => {
                    ent.insert(s);
                    break;
                }
            }
        }

        // Make sure to then mark any symbolic values into unknowns
        let mut invalidating = false;
        let mut off = 0;
        while (off < size) || invalidating {
            let addr_off = addr + u32::try_from(off).expect("should be small");
            match self.relocs.entry(addr_off) {
                tm::Entry::Occupied(mut ent) => {
                    ent.insert(Symbol::Unknown);
                    match ent.get() {
                        Symbol::Rel(_) | Symbol::Partial => invalidating = true,
                        Symbol::Value(_) | Symbol::Unknown => invalidating = false,
                    }
                }
                tm::Entry::Vacant(ent) => {
                    ent.insert(Symbol::Unknown);
                    invalidating = false;
                }
            }
            off += 1;
        }
    }

    pub fn write_reloc(
        &mut self,
        addr_site: SectionAddr,
        reloc: Option<(u32, SectionAddr)>,
        rtype: RelocType,
    ) -> Result<(), Error> {
        let orig = match rtype.size() {
            0 => None,
            2 => if let Symbol::Value(v) = self.read_16(addr_site)? {
                Some(RelocBacking::Val16(v & !rtype.mask() as u16))
            } else {
                None
            },
            4 => if let Symbol::Value(v) = self.read_32(addr_site)? {
                Some(RelocBacking::Val32(v & !rtype.mask()))
            } else {
                None
            },
            _ => unreachable!(),
        };
        
        self.invalidate_write(addr_site, rtype.size());

        let sym = match reloc {
            None => Symbol::Unknown,
            Some((file, target)) => Symbol::Rel(RelocSymbol {
                file,
                target,
                rtype,
                orig,
            }),
        };

        self.relocs.insert(addr_site, sym);
        for i in 1..(rtype.size() as u32) {
            self.relocs.insert(addr_site + i, Symbol::Partial);
        }

        Ok(())
    }

    pub fn write<T: Pod>(&mut self, addr: SectionAddr, val: &T) {
        assert_sane_size::<T>();

        self.invalidate_write(addr, size_of::<T>());

        let data = bytemuck::bytes_of(val);
        for i in 0..data.len() {
            self.relocs.insert(
                addr + i as u32,
                Symbol::Value(data[i])
            );
        }
    }

    pub fn write_16(&mut self, addr: SectionAddr, val: u16) {
        self.write(addr, &BigU16::new(val));
    }

    pub fn write_32(&mut self, addr: SectionAddr, val: u32) {
        self.write(addr, &BigU32::new(val));
    }

    pub fn read<T: Pod>(&self, addr: SectionAddr) -> Result<Symbol<T>, Error> {
        assert_sane_size::<T>();

        Ok(match self.relocs.get(&addr) {
            // Assume all relocations are concrete values
            None | Some(Symbol::Value(_)) => {
                let mut data = self.backing
                    .slice_of(addr, size_of::<T>() as u32)?
                    .to_vec();
                for i in 0..data.len() {
                    match self.relocs.get(&(addr + i as u32)) {
                        None => (),
                        Some(Symbol::Value(b)) => data[i] = *b,
                        Some(_) => {
                            return Ok(Symbol::Unknown);
                        }
                    }
                }

                Symbol::Value(bytemuck::pod_read_unaligned(&data))
            }
            // We might be cutting a partial value of some former symbolic
            // relocation entry
            Some(Symbol::Unknown | Symbol::Partial) => Symbol::Unknown,
            // Getting the first byte of a symbolic relocation entry
            Some(Symbol::Rel(rsym)) => {
                // Ensure that we are getting the full relocation entry
                if rsym.rtype.size() != size_of::<T>() {
                    return Ok(Symbol::Unknown);
                }

                // Ensure that all the rest of the markers following this
                // relocation entry is in fact just UNK markers
                for i in 1..size_of::<T>() {
                    if !matches!(self.relocs.get(&(addr + i as u32)), Some(Symbol::Partial)) {
                        return Ok(Symbol::Unknown);
                    }
                }

                Symbol::Rel(*rsym)
            }
        })
    }

    pub fn read_16(&self, addr: SectionAddr) -> Result<Symbol<u16>, Error> {
        self.read::<BigU16>(addr).map(|r| r.map(|v| v.get()))
    }

    pub fn read_32(&self, addr: SectionAddr) -> Result<Symbol<u32>, Error> {
        self.read::<BigU32>(addr).map(|r| r.map(|v| v.get()))
    }
}
