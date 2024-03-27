use std::{
    collections::{btree_map as tm, hash_map as hm, BTreeMap, HashMap},
    mem::size_of,
};

use bytemuck::{Pod, Zeroable};
use error::mk_err_wrapper;
use num::FromPrimitive;
use object::{build::elf as belf, elf::{self, SHN_UNDEF}, Endianness};


use crate::{sym, utils::SaneSize};

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
    #[error("Unable to address `{0}` relative to any symbol")]
    UnaddressableSymbol(SectionAddr),
    #[error("Unable to find symbol `{0:?}` in symbol database")]
    SymbolNotFound(sym::SymAddr),
    #[error("Unable to build elf: {0}")]
    BuildElfError(object::build::Error),
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
    pub fn rtype(&self) -> Result<ppcdis::RelocType, u8> {
        ppcdis::RelocType::from_u8(self.type_).ok_or(self.type_)
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

impl<'b> RelFile<'b> {
    pub fn to_elf(&self, symdb: &sym::SymbolDatabase) -> Res<Vec<u8>> {
        let our_id = self.header.id.get();
        let mut builder = belf::Builder::new(
            Endianness::Big,
            false,
        );

        builder.header.e_type = elf::ET_REL;
        builder.header.os_abi = elf::ELFOSABI_SYSV;
        builder.header.abi_version = 0;
        builder.header.e_machine = elf::EM_PPC;

        // Create all the main program section headers
        let mut data_cnt = 0;
        let mut bss_cnt = 0;
        let mut text_cnt = 0;
        let mut section_map = BTreeMap::new();
        let mut local_syms = BTreeMap::new();
        for (i, sect) in self.sections.iter().enumerate() {
            let offset = sect.offset();
            let length = sect.length();
            if length == 0 && offset.is_none() {
                continue;
            }

            let (section_name, sh_type, sh_flags, cnt) = if offset.is_none() {
                (".bss", elf::SHT_NOBITS, elf::SHF_WRITE, &mut bss_cnt)
            } else if sect.is_exec() {
                (
                    ".text",
                    elf::SHT_PROGBITS,
                    elf::SHF_ALLOC | elf::SHF_EXECINSTR,
                    &mut text_cnt,
                )
            } else {
                (
                    ".data",
                    elf::SHT_PROGBITS,
                    elf::SHF_ALLOC | elf::SHF_WRITE,
                    &mut data_cnt,
                )
            };
            *cnt += 1;
            let section_name = if *cnt <= 1 {
                section_name.into()
            } else {
                format!("{section_name}{cnt}")
            };

            let section = builder.sections.add();
            let id = section.id();
            section_map.insert(i, (section.id(), section_name.clone()));
            section.name = section_name.as_bytes().to_vec().into();
            section.sh_type = sh_type;
            section.sh_flags = sh_flags.into();
            section.sh_offset = 0x800 + u64::from(offset.unwrap_or_default());
            section.sh_addralign = self.header.align.get().into();
            section.data = match offset {
                None => belf::SectionData::UninitializedData(length.into()),
                Some(offset) => belf::SectionData::Data(
                    self.data[offset as usize..offset as usize+length as usize].into()
                ),
            };

            // Add section symbol.
            let sect_sym = builder.symbols.add();
            sect_sym.name = section.name.clone();
            sect_sym.section = Some(id);
            sect_sym.set_st_info(elf::STB_LOCAL, elf::STT_SECTION);
            sect_sym.st_value = 0;
            sect_sym.st_other = elf::STV_DEFAULT;
            local_syms.insert(SectionAddr::new(i as u8, 0), sect_sym.id());
        }

        println!("{local_syms:?}");

        // Popluate the symbol table, hoisting the locals to before the
        // globals because requirements
        let mut locals = builder.symbols.count();
        let mut syms: Vec<_> = symdb.rel_iter(our_id).collect();
        syms.sort_by_key(|s| s.local as u8);
        for sym in syms {
            let saddr = sym.section_addr();
            let (sid, _) = section_map.get(&saddr.sect.into())
                .ok_or_else(|| error!(ErrorType::BadSection(saddr.sect)))?;
            if sym.local {
                locals += 1;
            }

            let elf_sym = builder.symbols.add();
            elf_sym.name = sym.name.as_bytes().to_vec().into();
            elf_sym.section = Some(*sid);
            elf_sym.set_st_info(if sym.local {
                elf::STB_LOCAL
            } else {
                elf::STB_GLOBAL
            }, match sym.value_type {
                sym::DataType::Simple(sym::SimpleType::Function) => elf::STT_FUNC,
                _ => elf::STT_OBJECT,
            });
            elf_sym.st_value = saddr.offset.into();
            elf_sym.st_other = elf::STV_DEFAULT;
            local_syms.insert(saddr, elf_sym.id());
        }

        // Prepare symbol and string table
        let section = builder.sections.add();
        section.name = (b".shstrtab" as &[u8]).into();
        section.sh_type = elf::SHT_STRTAB;
        section.sh_addralign = 1;
        section.data = belf::SectionData::SectionString;

        let section = builder.sections.add();
        let shstrtab_id = section.id();
        section.name = (b".strtab" as &[u8]).into();
        section.sh_type = elf::SHT_STRTAB;
        section.sh_addralign = 1;
        section.data = belf::SectionData::String;

        let section = builder.sections.add();
        let symtab_id = section.id();
        section.name = (b".symtab" as &[u8]).into();
        section.sh_link_section = Some(shstrtab_id);
        section.sh_info_section = None;
        section.sh_info = locals as u32;
        section.sh_addralign = 4;
        section.sh_type = elf::SHT_SYMTAB;
        section.data = belf::SectionData::Symbol;

        // Add all relocations into a temporary storage, categorizing by
        // section. We then transfer all of them at once
        let mut relocs = BTreeMap::new();
        let mut cur_addr: Option<SectionAddr> = None;
        let mut known_syms = HashMap::new();
        for (file, rel) in self.relocations() {
            // Always increment offset
            cur_addr.as_mut().map(|a| *a += rel.offset.get().into());

            // Translate the relocation type from rel to elf format.
            let rel_rtype = rel.rtype().map_err(|rt| error!(InvalidRelocType(rt)))?;
            let r_type = match rel_rtype {
                ppcdis::RelocType::PPCNone => continue,
                ppcdis::RelocType::PPCAddr32 => elf::R_PPC_ADDR32,
                ppcdis::RelocType::PPCAddr24 => elf::R_PPC_ADDR24,
                ppcdis::RelocType::PPCAddr16 => elf::R_PPC_ADDR16,
                ppcdis::RelocType::PPCAddr16Lo => elf::R_PPC_ADDR16_LO,
                ppcdis::RelocType::PPCAddr16Hi => elf::R_PPC_ADDR16_HI,
                ppcdis::RelocType::PPCAddr16Ha => elf::R_PPC_ADDR16_HA,
                ppcdis::RelocType::PPCAddr14 => elf::R_PPC_ADDR14,
                ppcdis::RelocType::PPCAddr14BrTaken => elf::R_PPC_ADDR14_BRTAKEN,
                ppcdis::RelocType::PPCAddr14BrNotTaken => elf::R_PPC_ADDR14_BRNTAKEN,
                ppcdis::RelocType::PPCRel24 => elf::R_PPC_REL24,
                ppcdis::RelocType::PPCRel14 => elf::R_PPC_REL14,
                ppcdis::RelocType::PPCRel14BrTaken => elf::R_PPC_REL14_BRTAKEN,
                ppcdis::RelocType::PPCRel14BrNotTaken => elf::R_PPC_REL14_BRNTAKEN,
                ppcdis::RelocType::RvlNone => continue,
                ppcdis::RelocType::RvlSect => {
                    cur_addr = Some(SectionAddr::new(
                        rel.section(),
                        rel.offset.get().into(),
                    ));
                    continue;
                }
                ppcdis::RelocType::RvlEnd => {
                    cur_addr = None;
                    continue;
                }
            };

            // Compute the symbol id and addend that this is relative to
            let target_saddr = SectionAddr::new(rel.section, rel.addend());
            let (sym, addend) = if file == our_id {
                // A local relocation would use our local syms
                let (base_saddr, sid) = local_syms.range(..=target_saddr)
                    .last()
                    .filter(|(addr, _)| addr.sect == target_saddr.sect)
                    .or_else(|| local_syms
                        .range(target_saddr..)
                        .next()
                        .filter(|(addr, _)| addr.sect == target_saddr.sect))
                    .ok_or_else(|| error!(UnaddressableSymbol(target_saddr)))?;

                (*sid, target_saddr.offset as i32 - base_saddr.offset as i32)
            } else {
                // Otherwise search in the symdb or our known_syms mapping for
                // a symbol to link against
                let target_symaddr = if file == 0 {
                    sym::SymAddr::Dol(rel.addend.get())
                } else {
                    sym::SymAddr::Rel(file, target_saddr)
                };
                match known_syms.entry(target_symaddr) {
                    hm::Entry::Occupied(ent) => *ent.get(),
                    hm::Entry::Vacant(ent) => {
                        let (syment, addend) = match symdb.get_near(target_symaddr) {
                            Some(ent) => ent,
                            None => {
                                eprintln!("ERROR: symbol not found: {target_symaddr:?}");
                                continue;
                            }
                        };
                        //let syment = symdb.get(target_symaddr)
                        //    .ok_or_else(|| error!(SymbolNotFound(target_symaddr)))?;
                        let sym = builder.symbols.add();
                        sym.name = syment.name.as_bytes().to_vec().into();
                        sym.section = None;
                        sym.st_shndx = SHN_UNDEF;
                        sym.set_st_info(elf::STB_GLOBAL, elf::STT_NOTYPE);
                        sym.st_value = 0;
                        sym.st_other = elf::STV_DEFAULT;
                        ent.insert((sym.id(), addend));
                        (sym.id(), addend)
                    }
                }
            };

            let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
            relocs.entry(addr.sect)
                .or_insert_with(Vec::new)
                .push(belf::Relocation {
                    r_offset: addr.offset.into(),
                    symbol: Some(sym),
                    r_type,
                    r_addend: addend.into(),
                });
        }

        // Add relocation section for each section
        for (section, relocs) in relocs {
            let (our_sid, name) = section_map.get(&section.into())
                .expect("Section should exist");

            let section = builder.sections.add();
            section.name = format!(".rela{name}").into_bytes().into();
            section.sh_link_section = Some(symtab_id);
            section.sh_info_section = Some(*our_sid);
            section.sh_addralign = 4;
            section.sh_type = elf::SHT_RELA;
            section.sh_entsize = 0xc;
            section.data = belf::SectionData::Relocation(relocs);
        }

        let mut out = Vec::new();
        builder.write(&mut out).map_err(|e| error!(BuildElfError(e)))?;
        Ok(out)
    }
}

enum Size {
    Length(usize),
    Bytes(usize),
}

fn bytes_to_vec<T: Pod + Copy>(data: &[u8], mut offset: usize, len: Size) -> Vec<T> {
    let size = match len {
        Size::Length(len) => size_of::<T>() * len,
        Size::Bytes(sz) => sz,
    };
    match bytemuck::try_cast_slice(&data[offset..offset + size]) {
        Ok(res) => res.to_vec(),
        Err(bytemuck::PodCastError::TargetAlignmentGreaterAndInputNotAligned) => {
            let mut ret = Vec::new();
            let end = offset + size;
            while offset < end {
                ret.push(bytemuck::pod_read_unaligned(
                    &data[offset..offset+size_of::<T>()]
                ));
                offset += size_of::<T>();
            }

            ret
        }
        Err(e) => panic!("cast_slice>{e}"),
    }
}

impl<'b> RelFile<'b> {
    pub fn new(data: &'b [u8]) -> Self {
        let header: RelHeader =
            bytemuck::pod_read_unaligned(&data[0..size_of::<RelHeader>()]);

        let sections = bytes_to_vec(
            data,
            header.section_info_offset.get() as usize,
            Size::Length(header.num_sections.get() as usize),
        );

        let imp: Vec<ImpEntry> = bytes_to_vec(
            data,
            header.imp_offset.get() as usize,
            Size::Bytes(header.imp_size.get() as usize),
        );

        let mut relocs = HashMap::new();
        let mut reloc_off = header.rel_offset.get() as usize;

        for ent in &imp {
            let tbl = match relocs.entry(ent.id()) {
                hm::Entry::Occupied(ent) => ent.into_mut(),
                hm::Entry::Vacant(ent) => ent.insert(Vec::new()),
            };
            loop {
                let ent: RelocEntry = bytemuck::pod_read_unaligned(
                    &data[reloc_off..reloc_off + size_of::<RelocEntry>()],
                );
                tbl.push(ent);
                reloc_off += size_of::<RelocEntry>();
                if ent.type_ == ppcdis::RelocType::RvlEnd as u8 {
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

    pub fn relocations_for_file(
        &self,
        id: u32,
    ) -> impl Iterator<Item = &RelocEntry> {
        self.relocs.get(&id).map_or_else(|| [].iter(), |r| r.iter())
    }

    pub fn imp_tbl(&self) -> impl Iterator<Item = &ImpEntry> {
        self.imp.iter()
    }

    pub fn slice_of(
        &self,
        addr: SectionAddr,
        size: u32,
    ) -> Result<&[u8], Error> {
        let sect = self
            .sections
            .get(usize::from(addr.sect))
            .ok_or_else(|| error!(BadSection(addr.sect)))?;

        let sect_offset =
            sect.offset().ok_or_else(|| error!(BadSection(addr.sect)))?
                as usize;

        let end = addr
            .offset
            .checked_add(size)
            .ok_or_else(|| error!(BadOffset(addr.offset)))?;

        if end > sect.length() {
            Err(error!(BadOffset(addr.offset)))
        } else {
            Ok(&self.data[sect_offset + addr.offset as usize
                ..sect_offset + end as usize])
        }
    }

    pub fn read<T: Pod>(&self, addr: SectionAddr) -> Result<T, Error> where
        T: SaneSize
    {
        Ok(bytemuck::pod_read_unaligned(
            self.slice_of(addr, size_of::<T>() as u32)?,
        ))
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

impl From<RelocBacking> for u32 {
    fn from(value: RelocBacking) -> Self {
        match value {
            RelocBacking::Val16(v) => v.into(),
            RelocBacking::Val32(v) => v,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct RelocSymbol {
    pub file: u32,
    pub target: SectionAddr,
    pub rtype: ppcdis::RelocType,
    pub orig: Option<RelocBacking>,
}

impl RelocSymbol {
    pub fn get_address(&self) -> sym::SymAddr {
        if self.file == 0 {
            sym::SymAddr::Dol(self.target.offset)
        } else {
            sym::SymAddr::Rel(self.file, self.target)
        }
    }
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
    Concrete(ppcdis::RelocAction),
    RelocSymbol(ppcdis::RelocType),
}

pub struct RelocOverlay<'r, 'b> {
    backing: &'r RelFile<'b>,
    relocs: BTreeMap<SectionAddr, Symbol<u8>>,
}

impl<'r, 'b> RelocOverlay<'r, 'b> {
    pub fn new(backing: &'r RelFile<'b>) -> Self {
        Self {
            backing,
            relocs: BTreeMap::new(),
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
            let rtype =
                rel.rtype().map_err(|rt| error!(InvalidRelocType(rt)))?;
            let action = if has_dol && file == 0 {
                rtype.eval(rel.addend(), None).map_or_else(
                    || SymbolicActions::RelocSymbol(rtype),
                    SymbolicActions::Concrete,
                )
            } else if let Some(base) = resolved.get(&(file, rel.section())) {
                rtype.eval(base + rel.addend(), None).map_or_else(
                    || SymbolicActions::RelocSymbol(rtype),
                    SymbolicActions::Concrete,
                )
            } else {
                match rtype {
                    ppcdis::RelocType::PPCNone => {
                        SymbolicActions::Concrete(ppcdis::RelocAction::None)
                    }
                    ppcdis::RelocType::RvlNone => {
                        SymbolicActions::Concrete(ppcdis::RelocAction::None)
                    }
                    ppcdis::RelocType::RvlSect => {
                        SymbolicActions::Concrete(ppcdis::RelocAction::Sect)
                    }
                    ppcdis::RelocType::RvlEnd => {
                        SymbolicActions::Concrete(ppcdis::RelocAction::End)
                    }
                    r => SymbolicActions::RelocSymbol(r),
                }
            };

            cur_addr.as_mut().map(|a| *a += rel.offset.get().into());

            match action {
                SymbolicActions::Concrete(action) => match action {
                    ppcdis::RelocAction::None => {}
                    ppcdis::RelocAction::Write32 { val, mask } => {
                        let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                        if mask == !0 {
                            self.write_32(addr, val);
                        } else if mask != 0 {
                            let old_val = self.read_32(addr)?;
                            if let Symbol::Value(old_val) = old_val {
                                self.write_32(
                                    addr,
                                    (old_val & !mask) | (val & mask),
                                );
                            } else {
                                self.write_reloc(
                                    addr,
                                    None,
                                    ppcdis::RelocType::PPCAddr32,
                                )?;
                            }
                        }
                    }
                    ppcdis::RelocAction::Write16 { val, mask } => {
                        let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                        if mask == !0 {
                            self.write_16(addr, val);
                        } else if mask != 0 {
                            let old_val = self.read_16(addr)?;
                            if let Symbol::Value(old_val) = old_val {
                                self.write_16(
                                    addr,
                                    (old_val & !mask) | (val & mask),
                                );
                            } else {
                                self.write_reloc(
                                    addr,
                                    None,
                                    ppcdis::RelocType::PPCAddr16,
                                )?;
                            }
                        }
                    }
                    ppcdis::RelocAction::Sect => {
                        cur_addr = Some(SectionAddr::new(rel.section(), 0));
                    }
                    ppcdis::RelocAction::End => {
                        cur_addr = None;
                        continue;
                    }
                },
                SymbolicActions::RelocSymbol(rtype) => {
                    let addr = cur_addr.ok_or_else(|| error!(NoSection))?;
                    self.write_reloc(
                        addr,
                        Some((
                            file,
                            SectionAddr::new(rel.section(), rel.addend()),
                        )),
                        rtype,
                    )?;
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
                s @ Symbol::Value(_) => {
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
                        Symbol::Value(_) | Symbol::Unknown => {
                            invalidating = false
                        }
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
        rtype: ppcdis::RelocType,
    ) -> Result<(), Error> {
        let orig = match rtype.size() {
            0 => None,
            2 => {
                if let Symbol::Value(v) = self.read_16(addr_site)? {
                    Some(RelocBacking::Val16(v & !rtype.mask() as u16))
                } else {
                    None
                }
            }
            4 => {
                if let Symbol::Value(v) = self.read_32(addr_site)? {
                    Some(RelocBacking::Val32(v & !rtype.mask()))
                } else {
                    None
                }
            }
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

    pub fn write<T: Pod + SaneSize>(&mut self, addr: SectionAddr, val: &T) {
        self.invalidate_write(addr, size_of::<T>());

        let data = bytemuck::bytes_of(val);
        for i in 0..data.len() {
            self.relocs.insert(addr + i as u32, Symbol::Value(data[i]));
        }
    }

    pub fn write_16(&mut self, addr: SectionAddr, val: u16) {
        self.write(addr, &BigU16::new(val));
    }

    pub fn write_32(&mut self, addr: SectionAddr, val: u32) {
        self.write(addr, &BigU32::new(val));
    }

    pub fn read<T: Pod + SaneSize>(
        &self,
        addr: SectionAddr,
    ) -> Result<Symbol<T>, Error> {
        Ok(match self.relocs.get(&addr) {
            // Assume all relocations are concrete values
            None | Some(Symbol::Value(_)) => {
                let mut data = self
                    .backing
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
                    if !matches!(
                        self.relocs.get(&(addr + i as u32)),
                        Some(Symbol::Partial)
                    ) {
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
