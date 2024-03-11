use thiserror::Error;

use crate::{clsdata, evt, mk_err_wrapper, reader, rel, sym};

use std::fmt::Write as _;

use interop::{CReader, CTypeable};

// Sanity check to ensure that usize is as big (if not bigger) than u32
const _: () = assert!(core::mem::size_of::<usize>() >= core::mem::size_of::<u32>());

const ERROR_JIS: &'static str = "Unable to encode string to/from shift_jis";
#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("{0}")]
    ReaderError(reader::ErrorType),
    #[error("{ERROR_JIS}")]
    EncodingError,
    #[error("Formatting error")]
    FormattingError,
}

mk_err_wrapper!{
    ErrorType {
        ReaderError => reader::ErrorType,
    }
}

impl From<std::fmt::Error> for Error {
    fn from(_: std::fmt::Error) -> Self {
        error!(FormattingError)
    }
}

impl From<rel::Error> for Error {
    fn from(value: rel::Error) -> Self {
        let (backtrace, tp) = value.unwrap();
        Error::new_with(
            backtrace,
            ErrorType::ReaderError(reader::ErrorType::RelFileError(tp)),
        )
    }
}

pub enum CodeLine {
    Variable {
        vartype: interop::CType,
        value: Option<String>,
    }
}

impl CodeLine {
    pub fn gen(&self, ident: String, local: bool) -> interop::Definition {
        match self {
            CodeLine::Variable { vartype, value } => {
                vartype.make_decl(ident, value.clone(), local)
            }
        }
    }
}

#[derive(Debug)]
pub struct JPString(pub String);
impl<P: interop::Ptr> interop::CRead<P> for JPString {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
        R: CReader<P> + ?Sized
    {
        let vec: Vec<u8> = reader.read_val()?;
        let (s, _, errors) = encoding_rs::SHIFT_JIS.decode(&vec);
        if errors {
            Err(reader.error_custom(ERROR_JIS.into()))
        } else {
            Ok(Self(s.to_string()))
        }
    }
}

impl<Ctx> interop::CDump<Ctx> for JPString {
    type Error = Error;

    fn dump(&self, f: &mut interop::Dumper, _: &Ctx) -> Result<(), Self::Error> {
        let (raw_bytes, _, errors) = encoding_rs::SHIFT_JIS.encode(&self.0);
        if errors {
            bail!(EncodingError);
        }

        write!(f, "\"")?;
        for b in &*raw_bytes {
            if let Ok(s) = std::str::from_utf8(&[*b]) {
                if *b == b'"' || *b == b'\\' {
                    write!(f, "\\{s}")?;
                    continue;
                }

                if b.is_ascii_graphic() {
                    write!(f, "{s}")?;
                    continue;
                }
            }
            write!(f, "\\{b:03o}")?;
        }
        write!(f, "\"")?;

        Ok(())
    }
}

impl interop::Size for JPString {
    fn len(&self) -> usize {
        interop::Size::len(&self.0)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Zeroed(pub usize);
impl<P: interop::Ptr> interop::CRead<P> for Zeroed {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
        R: CReader<P> + ?Sized
    {
        let cnt = reader.remaining();
        reader.skip(cnt)?;
        Ok(Zeroed(cnt))
    }
}

impl<Ctx> interop::CDump<Ctx> for Zeroed {
    type Error = std::fmt::Error;

    fn dump(&self, f: &mut interop::Dumper, ctx: &Ctx) -> std::fmt::Result {
        interop::CDump::dump(&vec![0; self.0], f, ctx)
    }
}

impl interop::Size for Zeroed {
    fn len(&self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug)]
pub enum Data {
    AsmFunc(Vec<u8>),
    CFunc(String),
    PtrArr(Vec<sym::SymAddr>),
    Ptr(sym::SymAddr),
    String(JPString),
    Float(Vec<f32>),
    Double(f64),
    Zero(Zeroed),
    Evt(evt::Script),
    Vec3([f32; 3]),
    NpcSetupInfo(Vec<clsdata::NpcSetupInfo>),
}

impl Data {
    pub fn read(
        overlay: &rel::RelocOverlay,
        ent: &sym::RawSymEntry,
    ) -> Res<Self> {
        let mut reader = reader::Reader::new(overlay, ent);
        let res = match ent.value_type {
            sym::DataType::Simple(tp) => match tp {
                sym::SimpleType::PtrArr => Data::PtrArr(reader.read_val_full()?),
                sym::SimpleType::Ptr => Data::Ptr(reader.read_val_full()?),
                sym::SimpleType::String => Data::String(reader.read_val_full()?),
                sym::SimpleType::Float => Data::Float(reader.read_val_full()?),
                sym::SimpleType::Double => Data::Double(reader.read_val_full()?),
                sym::SimpleType::Zero => Data::Zero(reader.read_val_full()?),
                sym::SimpleType::Evt => Data::Evt(reader.read_val_full()?),
                sym::SimpleType::Function => {
                    // TODO:
                    Data::Zero(reader.read_val_full()?)
                }
                sym::SimpleType::Vec3 => Data::Vec3(reader.read_val_full()?),
            },
            sym::DataType::Class(tp) => {
                match tp {
                    sym::ClassType::NpcSetupInfo => Data::NpcSetupInfo(
                        reader.read_val_full()?
                    ),
                    _ => todo!(),
                }
            },
        };
        Ok(res)
    }

    pub fn get_type(
        &self,
        sec_type: sym::SectionType,
        overlay: &rel::RelocOverlay,
        symdb: &sym::SymbolDatabase,
    ) -> interop::CType {
        let kind = match self {
            Data::AsmFunc(_) => interop::ctype_kind!([void]),
            Data::CFunc(_) => interop::ctype_kind!([void]),
            Data::PtrArr(addrs) => {
                let mut pointee = None;
                for addr in addrs {
                    let cur_pointee = symdb.get(*addr)
                        .and_then(|sym| Some(Data::read(overlay, &sym)
                            .ok()?
                            .get_type(sym.sec_name, overlay, symdb)
                        ))
                        .unwrap_or_else(|| interop::ctype!(mut void));

                    match &pointee {
                        Some(old_pointee) if &cur_pointee != old_pointee => {
                            pointee = None;
                            break;
                        }
                        None => {
                            pointee = Some(cur_pointee);
                        }
                        _ => { }
                    }
                }

                let pointee = pointee.unwrap_or_else(|| interop::ctype!(mut void));
                interop::ctype_kind!([&{pointee}])
            },
            Data::Ptr(addr) => {
                let pointee = symdb.get(*addr)
                    .and_then(|sym| Some(Data::read(overlay, &sym)
                        .ok()?
                        .get_type(sym.sec_name, overlay, symdb)
                    ))
                    .unwrap_or_else(|| interop::ctype!(mut void));
                interop::ctype_kind!(&{pointee})
            }
            Data::String(_) => interop::ctype_kind!([i8]),
            Data::Float(values) => {
                if values.len() == 1 {
                    interop::ctype_kind!(f32)
                } else {
                    interop::ctype_kind!([f32])
                }
            },
            Data::Double(_) => interop::ctype_kind!(f64),
            Data::Zero(_) => interop::ctype_kind!([i8]),
            Data::Evt(_) => interop::ctype_kind!([i32]),
            Data::Vec3(_) => interop::ctype_kind!([f32; 3]),
            Data::NpcSetupInfo(_) => clsdata::NpcSetupInfo::get_type()
        };

        interop::CType::new(sec_type.is_ro(), kind)
    }

    pub fn to_code(
        &self,
        sec_type: sym::SectionType,
        overlay: &rel::RelocOverlay,
        symdb: &sym::SymbolDatabase,
    ) -> Res<CodeLine> {
        let vartype = self.get_type(sec_type, overlay, symdb);
        let value = if let sym::SectionType::Bss = sec_type {
            None
        } else {
            let addr_ctx = sym::AddrDumpCtx::new(&vartype, symdb);
            Some(match self {
                Data::AsmFunc(_) => todo!(),
                Data::CFunc(_) => todo!(),
                Data::PtrArr(addrs) => interop::dumps(addrs, &addr_ctx)?,
                Data::Ptr(addr) => interop::dumps(addr, &addr_ctx)?,
                Data::String(dt) => interop::dumps(dt, &())?,
                Data::Float(dt) => {
                    match dt.as_slice() {
                        [single] => interop::dumps(single, &())?,
                        dt => interop::dumps(dt, &())?,
                    }
                },
                Data::Double(dt) => interop::dumps(dt, &())?,
                Data::Zero(dt) => interop::dumps(dt, &())?,
                Data::Evt(dt) => sym::SymContext {
                    symdb,
                    area: overlay.backing().header().id.get(),
                    val: dt,
                }.to_string(),
                Data::Vec3(dt) => interop::dumps(dt, &())?,
                Data::NpcSetupInfo(dt) => {
                    interop::dumps(dt, &symdb)?
                },
            })
        };

        Ok(CodeLine::Variable {
            vartype,
            value,
        })
    }
}

impl interop::Size for Data {
    fn len(&self) -> usize {
        match &self {
            Data::AsmFunc(dat) => dat as &dyn interop::Size,
            Data::CFunc(dat) => dat,
            Data::PtrArr(dat) => dat,
            Data::Ptr(dat) => dat,
            Data::String(dat) => dat,
            Data::Float(dat) => dat,
            Data::Double(dat) => dat,
            Data::Zero(dat) => dat,
            Data::Evt(dat) => dat,
            Data::Vec3(dat) => dat,
            Data::NpcSetupInfo(dat) => dat,
        }.len()
    }
}

pub fn generate_line(
    overlay: &rel::RelocOverlay,
    symdb: &sym::SymbolDatabase,
    ent: &sym::RawSymEntry,
) -> Res<interop::Definition> {
    let area_id = overlay.backing().header().id.get();
    let addr = ent.section_addr();
    let dat = Data::read(overlay, ent)?;
    let code_line = dat.to_code(ent.sec_name, overlay, symdb)?;
    let symbol_name = symdb.symbol_name(sym::SymAddr::Rel(area_id, addr), false);
    Ok(code_line.gen(symbol_name, ent.local))
}
