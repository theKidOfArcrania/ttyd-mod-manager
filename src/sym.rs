use std::{
    cell::Cell,
    collections::{BTreeSet, HashMap, HashSet},
    fmt::{self, Write as _},
    io,
    mem::size_of, ops::{Add, AddAssign},
};

use serde::{de::IntoDeserializer, Deserialize, Serialize};

use crate::{clsdata, rel};

#[derive(Clone, Copy, Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
pub enum SectionType {
    #[serde(alias = ".init")]
    Init,
    #[serde(alias = ".text")]
    Text,
    #[serde(alias = ".data", alias = ".sdata")]
    Data,
    #[serde(alias = ".rodata", alias = ".sdata2")]
    Rodata,
    #[serde(alias = ".dtors")]
    Dtors,
    #[serde(alias = ".ctors")]
    Ctors,
    #[serde(alias = ".bss", alias = ".sbss", alias = ".sbss2")]
    Bss,
}

impl SectionType {
    pub fn is_exec(self) -> bool {
        match self {
            Self::Text => true,
            Self::Data => false,
            Self::Bss => false,
            Self::Init => true,
            Self::Rodata => false,
            Self::Dtors => true,
            Self::Ctors => true,
        }
    }

    pub fn is_ro(self) -> bool {
        match self {
            Self::Text => true,
            Self::Data => false,
            Self::Bss => false,
            Self::Init => true,
            Self::Rodata => true,
            Self::Dtors => true,
            Self::Ctors => true,
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Copy, PartialEq, Eq, Debug)]
#[serde(rename_all = "lowercase")]
pub enum SimpleType {
    PtrArr,
    Ptr,
    String,
    Float,
    Double,
    Zero,
    Evt,
    Function,
    Vec3,
    Unknown,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DataType {
    Simple(SimpleType),
    Class(clsdata::ClsDataType),
}

impl serde::Serialize for DataType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            DataType::Simple(st) => st.serialize(serializer),
            DataType::Class(ct) => ct.serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for DataType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ParseVariant;
        impl<'de> serde::de::Visitor<'de> for ParseVariant {
            type Value = DataType;

            fn expecting(
                &self,
                f: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                write!(f, "Expecting str")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(match v.chars().next() {
                    None => DataType::Simple(SimpleType::Unknown),
                    Some(c) => {
                        let de = v.into_deserializer();
                        if c.is_lowercase() {
                            DataType::Simple(SimpleType::deserialize(de)?)
                        } else {
                            DataType::Class(clsdata::ClsDataType::deserialize(de)?)
                        }
                    }
                })
            }
        }
        deserializer.deserialize_str(ParseVariant)
    }
}

#[derive(Clone, Serialize, Deserialize, Debug)]
pub struct RawSymEntry {
    pub area: String,
    pub sec_id: u8,
    #[serde(with = "serde_u32_hex")]
    pub sec_offset: u32,
    pub sec_name: SectionType,
    pub sec_type: SectionType,
    #[serde(with = "serde_opt_u32_hex")]
    pub ram_addr: Option<u32>,
    #[serde(with = "serde_opt_u32_hex")]
    pub file_addr: Option<u32>,
    pub name: String,
    pub namespace: String,
    #[serde(with = "serde_u32_hex")]
    pub size: u32,
    pub align: u32,
    #[serde(rename = "type")]
    pub value_type: DataType,
    pub value: String,
    #[serde(skip)]
    pub local: bool,
}

impl RawSymEntry {
    pub fn section_addr(&self) -> rel::SectionAddr {
        rel::SectionAddr::new(self.sec_id, self.sec_offset)
    }
}

mod serde_opt_u32_hex {
    pub(super) fn serialize<S>(
        val: &Option<u32>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&match val {
            None => "".into(),
            Some(val) => format!("{val:x}"),
        })
    }

    pub(super) fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<Option<u32>, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ParseU32;
        impl<'de> serde::de::Visitor<'de> for ParseU32 {
            type Value = Option<u32>;

            fn expecting(
                &self,
                f: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                write!(f, "Expecting str")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                if v.is_empty() {
                    Ok(None)
                } else {
                    u32::from_str_radix(v, 16).map(Some).map_err(|e| {
                        E::custom(format!("Unable to parse integer: {e}"))
                    })
                }
            }
        }
        deserializer.deserialize_str(ParseU32)
    }
}

mod serde_u32_hex {
    pub(super) fn serialize<S>(
        val: &u32,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{val:x}"))
    }

    pub(super) fn deserialize<'de, D>(deserializer: D) -> Result<u32, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ParseU32;
        impl<'de> serde::de::Visitor<'de> for ParseU32 {
            type Value = u32;

            fn expecting(
                &self,
                f: &mut std::fmt::Formatter,
            ) -> std::fmt::Result {
                write!(f, "Expecting str")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                u32::from_str_radix(v, 16).map_err(|e| {
                    E::custom(format!("Unable to parse integer: {e}"))
                })
            }
        }
        deserializer.deserialize_str(ParseU32)
    }
}

#[derive(Clone)]
pub struct RawSymtab {
    syms: Vec<RawSymEntry>,
}

impl IntoIterator for RawSymtab {
    type Item = RawSymEntry;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.syms.into_iter()
    }
}

impl<'a> IntoIterator for &'a RawSymtab {
    type Item = &'a RawSymEntry;

    type IntoIter = std::slice::Iter<'a, RawSymEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.syms.iter()
    }
}

impl<'a> IntoIterator for &'a mut RawSymtab {
    type Item = &'a mut RawSymEntry;

    type IntoIter = std::slice::IterMut<'a, RawSymEntry>;

    fn into_iter(self) -> Self::IntoIter {
        self.syms.iter_mut()
    }
}

impl RawSymtab {
    pub fn from_reader<R: io::Read>(rdr: R) -> anyhow::Result<Self> {
        let mut rdr = csv::Reader::from_reader(rdr);
        let mut syms = Vec::new();

        let headers = rdr.headers()?.clone();
        let replmap =
            HashMap::from([('（', "$op"), ('）', "$cp"), ('$', "$$")]);
        let special_rel = HashSet::from([
            String::from("_unresolved"),
            "_prolog".into(),
            "_epilog".into(),
        ]);

        for record in rdr.records() {
            let record = record?;
            let mut ent: RawSymEntry = record.deserialize(Some(&headers))?;

            // Make sure function entries do not exist in data
            if ent.value_type == DataType::Simple(SimpleType::Unknown) &&
                ent.sec_name.is_exec() {
                    eprintln!(
                        "WARNING: {} entry's type assumed to be function",
                        ent.name,
                    );
                    ent.value_type = DataType::Simple(SimpleType::Function);
            }

            // Mangle illegal characters
            if ent.name.chars().any(|c| replmap.contains_key(&c)) {
                let mut ret = String::with_capacity(ent.name.capacity() * 2);
                for c in ent.name.chars() {
                    match replmap.get(&c) {
                        None => ret.push(c),
                        Some(repl) => ret.push_str(repl),
                    }
                }
                ent.name = ret;
            }

            // Set local flag
            ent.local = false;
            if &ent.area != "_main" {
                ent.local = true;
            }

            // Special rel sections that need to be global
            if special_rel.contains(&ent.name) {
                ent.local = false;
            }

            // *_init_evt types need to be global since the area will need to
            // set the initial event for each map
            if ent.name.ends_with("_init_evt") {
                ent.local = false;
            }
            syms.push(ent);
        }

        Ok(Self { syms })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub enum SymAddr {
    Dol(u32),
    Rel(u32, rel::SectionAddr),
}

impl Add<u32> for SymAddr {
    type Output = SymAddr;

    fn add(self, rhs: u32) -> Self::Output {
        match self {
            Self::Dol(off) => Self::Dol(off + rhs),
            Self::Rel(file, saddr) => Self::Rel(file, saddr + rhs),
        }
    }
}

impl AddAssign<u32> for SymAddr {
    fn add_assign(&mut self, rhs: u32) {
        *self = *self + rhs;
    }
}

impl From<u32> for SymAddr {
    fn from(value: u32) -> Self {
        Self::Dol(value)
    }
}

impl interop::Ptr for SymAddr {
    type Num = u32;
    type Rel<T> = rel::Symbol<T>;
}

impl SymAddr {
    pub fn name(&self, is_ref: bool) -> String {
        let ref_sym = if is_ref { "&" } else { "" };
        match self {
            SymAddr::Dol(0) => format!("NULL"),
            SymAddr::Dol(addr) => format!("{ref_sym}lbl_{addr:08x}"),
            SymAddr::Rel(file, saddr) => {
                format!("{ref_sym}{}_file_{file}", saddr.var_name())
            }
        }
    }
}

impl<T> interop::Symbolic<T, SymAddr> for rel::Symbol<T> {
    fn get_type(&self) -> interop::SymbolicType<&T, SymAddr> {
        use interop::SymbolicType::*;
        match self {
            rel::Symbol::Value(v) => Value(v),
            rel::Symbol::Rel(reloc) => {
                if reloc.rtype.size() != size_of::<T>() {
                    return Unknown;
                }
                match reloc.rtype {
                    ppcdis::RelocType::PPCAddr32 | ppcdis::RelocType::PPCAddr16 => (),
                    _ => return Unknown,
                };

                Pointer(reloc.get_address())
            }
            rel::Symbol::Partial | rel::Symbol::Unknown => Unknown,
        }
    }
}

#[derive(Default)]
pub struct StringsMap(HashMap<SymAddr, (String, Cell<bool>)>);
impl StringsMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, addr: SymAddr, value: String) {
        self.0.insert(addr, (value, Cell::new(false)));
    }

    pub fn visited(&self, addr: SymAddr) -> Option<bool> {
        self.0.get(&addr).map(|f| f.1.get())
    }
}

#[derive(Clone)]
pub struct AddrDumpCtx<'a> {
    area: u32,
    symdb: &'a SymbolDatabase,
    pointee_tp: Option<String>,
    strings: &'a StringsMap,
    is_refs: bool,
}

impl<'a> AddrDumpCtx<'a> {
    pub fn new(
        area: u32,
        var_type: &interop::CType,
        symdb: &'a SymbolDatabase,
        strings: &'a StringsMap,
    ) -> Self {
        let pointee_tp = var_type.get_pointee().map(|tp| {
            match &tp.kind {
                interop::CTypeKind::Prim(_) | interop::CTypeKind::TDef(_) => {
                    "".into()
                }
                interop::CTypeKind::Array(_, _)
                | interop::CTypeKind::PtrArray(_, _)
                | interop::CTypeKind::Ptr(_) => format!("({tp}) "),
            }
        });
        Self {
            area,
            symdb,
            pointee_tp,
            strings,
            is_refs: true,
        }
    }

    pub fn set_refs(&self, val: bool) -> Self {
        let mut ret = self.clone();
        ret.is_refs = val;
        ret
    }

    pub fn area(&self) -> u32 {
        self.area
    }

    pub fn pointee_tp(&self) -> Option<&str> {
        self.pointee_tp.as_ref().map(AsRef::as_ref)
    }

    pub fn symdb(&self) -> &SymbolDatabase {
        self.symdb
    }

    pub fn to_string<T>(&self, data: &T) -> Result<String, T::Error> where
        T: interop::CDump<AddrDumpCtx<'a>>,
    {
        let mut dumper = interop::Dumper::default();
        data.dump(&mut dumper, self)?;
        Ok(dumper.as_ref().to_string())
    }
}

impl<'a> interop::CDump<AddrDumpCtx<'a>> for SymAddr {
    type Error = fmt::Error;

    fn dump(
        &self,
        f: &mut interop::Dumper,
        ctx: &AddrDumpCtx<'a>,
    ) -> std::fmt::Result {
        let cast = ctx.pointee_tp().unwrap_or("");
        if let Some((value, accessed)) = ctx.strings.0.get(self) {
            accessed.set(true);
            write!(f, "{value:?}")
        } else {
            write!(f, "{cast}{}", ctx.symdb.symbol_name(*self, ctx.is_refs))
        }
    }
}

#[derive(Default, Clone)]
pub struct SymbolDatabase {
    primary_name: HashMap<SymAddr, String>,
    addr_names: HashMap<SymAddr, Vec<String>>,
    syms: HashMap<String, SymAddr>,
    raw_ent: HashMap<SymAddr, RawSymEntry>,
    by_area: HashMap<u32, BTreeSet<rel::SectionAddr>>,
    dol_addrs: BTreeSet<u32>,
    area_map: HashMap<String, u32>,
    null: BTreeSet<rel::SectionAddr>,
}

impl SymbolDatabase {
    pub fn new(area_map: HashMap<String, u32>, raw: RawSymtab) -> Self {
        // TODO: return error
        let mut ret = Self::default();
        for sym in raw.syms {
            let addr = if sym.area == "_main" {
                SymAddr::Dol(sym.ram_addr.expect("DOL should have RAM address"))
            } else {
                let area = match area_map.get(&sym.area) {
                    Some(a) => *a,
                    None => continue,
                };
                SymAddr::Rel(
                    area,
                    rel::SectionAddr {
                        sect: sym.sec_id,
                        offset: sym.sec_offset,
                    },
                )
            };

            ret.add_symbol(addr, sym.name.clone(), Some(sym));
        }

        ret.area_map = area_map;
        ret
    }

    pub fn add_symbol(
        &mut self,
        addr: SymAddr,
        name: String,
        entry: Option<RawSymEntry>,
    ) {
        self.primary_name.insert(addr, name.clone());
        self.addr_names
            .entry(addr)
            .or_insert_with(Vec::new)
            .push(name.clone());
        self.syms.insert(name, addr);
        match addr {
            SymAddr::Dol(abs) => {
                self.dol_addrs.insert(abs);
            }
            SymAddr::Rel(area, saddr) => {
                self.by_area
                    .entry(area)
                    .or_insert_with(BTreeSet::new)
                    .insert(saddr);
            }
        }
        if let Some(ent) = entry {
            self.raw_ent.insert(addr, ent);
        }
    }

    pub fn get(&self, addr: SymAddr) -> Option<RawSymEntry> {
        self.raw_ent.get(&addr).map(Clone::clone)
    }

    pub fn name_of(&self, addr: SymAddr) -> Option<&str> {
        self.primary_name.get(&addr).map(String::as_str)
    }

    pub fn symbol_name(&self, addr: SymAddr, is_ref: bool) -> String {
        let ref_sym = if is_ref { "&" } else { "" };
        match self.name_of(addr) {
            Some(name) => format!("{ref_sym}{name}"),
            None => addr.name(is_ref),
        }
    }

    pub fn all_names_of(&self, addr: SymAddr) -> &[String] {
        self.addr_names.get(&addr).map_or(&[], |f| f.as_ref())
    }

    pub fn get_addr(&self, name: &str) -> Option<SymAddr> {
        self.syms.get(name).map(|v| *v)
    }

    pub fn dol_iter(&self) -> impl Iterator<Item = &RawSymEntry> {
        self.dol_addrs
            .iter()
            .filter_map(|a| self.raw_ent.get(&SymAddr::Dol(*a)))
    }

    pub fn rel_iter<'e>(
        &'e self,
        area: u32,
    ) -> impl Iterator<Item = &'e RawSymEntry> {
        self.by_area
            .get(&area)
            .unwrap_or(&self.null)
            .iter()
            .filter_map(move |addr| {
                self.raw_ent.get(&SymAddr::Rel(area, *addr))
            })
    }
}
