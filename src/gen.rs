use error::mk_err_wrapper;
use std::fmt::Write as _;
use thiserror::Error;
use interop::CReader;

use crate::{clsdata, code, dol, evt, reader, rel, sym};

// Sanity check to ensure that usize is as big (if not bigger) than u32
const _: () =
    assert!(core::mem::size_of::<usize>() >= core::mem::size_of::<u32>());

const ERROR_JIS: &'static str = "Unable to encode string to/from shift_jis";
#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("{0}: {1}")]
    ReaderError(String, reader::ErrorType),
    #[error("{0}: {ERROR_JIS}")]
    EncodingError(String),
    #[error("{0}: Formatting error")]
    FormattingError(String),
    #[error("{0}: Expected a concrete RAM address")]
    NoRamAddr(String),
}

mk_err_wrapper! {
    ErrorType
}

impl Error {
    fn provide_sym(self, sym: &sym::RawSymEntry) -> Self {
        let (bt, e) = self.unwrap();
        let name = sym.name.clone();
        let e = match e {
            ErrorType::ReaderError(_, e) => ErrorType::ReaderError(name, e),
            ErrorType::EncodingError(_) => ErrorType::EncodingError(name),
            ErrorType::FormattingError(_) => ErrorType::FormattingError(name),
            ErrorType::NoRamAddr(_) => ErrorType::NoRamAddr(name),
        };
        Self::new_with(bt, e)
    }
}

impl From<reader::Error> for Error {
    fn from(e: reader::Error) -> Self {
        let (bt, e) = e.unwrap();
        Self::new_with(bt, ErrorType::ReaderError("".into(), e))
    }
}

impl From<std::fmt::Error> for Error {
    fn from(_: std::fmt::Error) -> Self {
        error!(FormattingError("".into()))
    }
}

impl From<rel::Error> for Error {
    fn from(value: rel::Error) -> Self {
        let (backtrace, tp) = value.unwrap();
        Error::new_with(
            backtrace,
            ErrorType::ReaderError("".into(), reader::ErrorType::RelFileError(tp)),
        )
    }
}

pub enum CodeLine {
    Variable {
        vartype: interop::CType,
        value: Option<String>,
    },
    AsmFunc(String),
    Func {
        args: Vec<String>,
        body: String,
        return_type: String,
    },
}

impl CodeLine {
    pub fn order(&self) -> u32 {
        match self {
            CodeLine::Variable { .. } => 0,
            CodeLine::AsmFunc(_) => 1,
            CodeLine::Func { .. } => 1,
        }
    }
    pub fn gen(
        &self,
        ident: String,
        local: bool,
    ) -> interop::Definition {
        let order = self.order();
        match self {
            CodeLine::Variable { vartype, value } => {
                vartype.make_decl(ident, value.clone(), local)
            }
            CodeLine::AsmFunc(code) => {
                let decl = format!(
                    "{}void {ident}(void)",
                    if local {
                        "static "
                    } else {
                        ""
                    }
                );
                interop::Definition {
                    definition: format!("asm {decl} {{\n{code}\n}}"),
                    declare: Some(format!("{decl};")),
                    order,
                }
            }
            CodeLine::Func { args, body, return_type } => {
                let decl = format!(
                    "{}{return_type} {ident}({})",
                    if local {
                        "static "
                    } else {
                        ""
                    },
                    if args.is_empty() {
                        "void".into()
                    } else {
                        args.join(", ")
                    },
                );
                interop::Definition {
                    definition: format!("{decl} {{\n{body}\n}}"),
                    declare: Some(format!("{decl};")),
                    order,
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct JPString(pub String);
impl<P: interop::Ptr> interop::CRead<P> for JPString {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized,
    {
        let mut vec: Vec<u8> = reader.read_val()?;
        if vec.last() == Some(&0) {
            vec.pop();
        }
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

    fn dump(
        &self,
        f: &mut interop::Dumper,
        _: &Ctx,
    ) -> Result<(), Self::Error> {
        let (raw_bytes, _, errors) = encoding_rs::SHIFT_JIS.encode(&self.0);
        if errors {
            bail!(EncodingError("".into()));
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
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized,
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
    AsmFunc(code::Code),
    CFunc(code::CCode),
    PtrArr(Vec<sym::SymAddr>),
    Ptr(sym::SymAddr),
    String(JPString),
    Float(Vec<f32>),
    Double(f64),
    Zero(Zeroed),
    Evt(evt::Script),
    // TODO: convert to Vec type
    Vec3([f32; 3]),
    Struct(clsdata::ClsData),
}

impl Data {
    pub fn read_rel(
        overlay: &rel::RelocOverlay,
        ent: &sym::RawSymEntry,
        symdb: &sym::SymbolDatabase,
    ) -> Res<Self> {
        let mut reader = reader::Reader(reader::RelocOverlayReader::new(
            overlay,
            ent,
        ));
        Data::read_no_provide(
            &mut reader,
            overlay.backing().header().id.get(),
            ent,
            symdb,
        ).map_err(|e| e.provide_sym(ent))
    }

    pub fn read_dol(
        file: &dol::DolFile,
        ent: &sym::RawSymEntry,
        symdb: &sym::SymbolDatabase,
    ) -> Res<Self> {
        let mut reader = reader::Reader(reader::DolReader::new(
            file,
            ent,
        ));
        Data::read_no_provide(
            &mut reader,
            0,
            ent,
            symdb,
        ).map_err(|e| e.provide_sym(ent))
    }

    fn read_no_provide<T: reader::SymReader + Clone>(
        reader: &mut reader::Reader<T>,
        file_id: u32,
        ent: &sym::RawSymEntry,
        symdb: &sym::SymbolDatabase,
    ) -> Res<Self> {
        let res = match ent.value_type {
            sym::DataType::Simple(tp) => match tp {
                sym::SimpleType::PtrArr => {
                    Data::PtrArr(reader.read_val_full()?)
                }
                sym::SimpleType::Ptr => Data::Ptr(reader.read_val_full()?),
                sym::SimpleType::String => {
                    Data::String(reader.read_val_full()?)
                }
                sym::SimpleType::Float => Data::Float(reader.read_val_full()?),
                sym::SimpleType::Double => {
                    Data::Double(reader.read_val_full()?)
                }
                sym::SimpleType::Zero
                | sym::SimpleType::Unknown => Data::Zero(reader.read_val_full()?),
                sym::SimpleType::Evt => Data::Evt(reader.read_val_full()?),
                sym::SimpleType::Function => {
                    let base_addr = if file_id != 0 {
                        sym::SymAddr::Rel(file_id, ent.section_addr())
                    } else {
                        sym::SymAddr::Dol(ent.ram_addr.ok_or_else(|| {
                            error!(NoRamAddr(String::new()))
                        })?)
                    };
                    let asm: code::Code = reader.read_val_full()?;
                    match asm.find_default_sig(base_addr, symdb) {
                        None => Data::AsmFunc(asm),
                        Some(cfunc) => Data::CFunc(cfunc),
                    }
                },
                sym::SimpleType::Vec3 => Data::Vec3(reader.read_val_full()?),
            },
            sym::DataType::Class(tp) => Data::Struct(tp.read(reader)?),
        };
        Ok(res)
    }

    fn get_type(
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
                    let cur_pointee = symdb
                        .get(*addr)
                        .and_then(|sym| {
                            Some(Data::read_rel(overlay, &sym, symdb).ok()?.get_type(
                                sym.sec_name,
                                overlay,
                                symdb,
                            ))
                        })
                        .unwrap_or_else(|| interop::ctype!(mut void));

                    match &pointee {
                        Some(old_pointee) if &cur_pointee != old_pointee => {
                            pointee = None;
                            break;
                        }
                        None => {
                            pointee = Some(cur_pointee);
                        }
                        _ => {}
                    }
                }

                let pointee =
                    pointee.unwrap_or_else(|| interop::ctype!(mut void));
                interop::ctype_kind!([&{ pointee }])
            }
            Data::Ptr(addr) => {
                let pointee = symdb
                    .get(*addr)
                    .and_then(|sym| {
                        Some(Data::read_rel(overlay, &sym, symdb).ok()?.get_type(
                            sym.sec_name,
                            overlay,
                            symdb,
                        ))
                    })
                    .unwrap_or_else(|| interop::ctype!(mut void));
                interop::ctype_kind!(&{ pointee })
            }
            Data::String(_) => interop::ctype_kind!([i8]),
            Data::Float(values) => {
                if values.len() == 1 {
                    interop::ctype_kind!(f32)
                } else {
                    interop::ctype_kind!([f32])
                }
            }
            Data::Double(_) => interop::ctype_kind!(f64),
            Data::Zero(_) => interop::ctype_kind!([i8]),
            Data::Evt(_) => interop::ctype_kind!([i32]),
            Data::Vec3(_) => interop::ctype_kind!([f32; 3]),
            Data::Struct(s) => s.get_type(),
        };

        interop::CType::new(sec_type.is_ro(), kind)
    }

    fn to_code(
        &self,
        sec_type: sym::SectionType,
        overlay: &rel::RelocOverlay,
        symdb: &sym::SymbolDatabase,
        strings: &sym::StringsMap,
    ) -> Res<CodeLine> {
        let vartype = self.get_type(sec_type, overlay, symdb);
        let value = if let sym::SectionType::Bss = sec_type {
            None
        } else {
            let addr_ctx = sym::AddrDumpCtx::new(
                overlay.backing().header().id.get(),
                &vartype,
                symdb,
                strings,
            );
            Some(match self {
                Data::AsmFunc(code) => {
                    return Ok(CodeLine::AsmFunc(interop::dumps(code, &addr_ctx)?));
                },
                Data::CFunc(code) => {
                    return Ok(CodeLine::Func {
                        args: code.args.clone(),
                        body: interop::dumps(code, &addr_ctx)?,
                        return_type: code.return_type.clone(),
                    })
                },
                Data::PtrArr(addrs) => interop::dumps(addrs, &addr_ctx)?,
                Data::Ptr(addr) => interop::dumps(addr, &addr_ctx)?,
                Data::String(dt) => interop::dumps(dt, &())?,
                Data::Float(dt) => match dt.as_slice() {
                    [single] => interop::dumps(single, &())?,
                    dt => interop::dumps(dt, &())?,
                },
                Data::Double(dt) => interop::dumps(dt, &())?,
                Data::Zero(dt) => interop::dumps(dt, &())?,
                Data::Evt(dt) => interop::dumps(dt, &addr_ctx)?,
                Data::Vec3(dt) => interop::dumps(dt, &())?,
                Data::Struct(dt) =>  interop::dumps(dt, &addr_ctx)?,
            })
        };

        Ok(CodeLine::Variable { vartype, value })
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
            Data::Struct(dat) => dat,
        }
        .len()
    }
}

pub fn generate_line(
    overlay: &rel::RelocOverlay,
    symdb: &sym::SymbolDatabase,
    ent: &sym::RawSymEntry,
    strings: &sym::StringsMap,
) -> Res<interop::Definition> {
    let addr = sym::SymAddr::Rel(
        overlay.backing().header().id.get(),
        ent.section_addr(),
    );
    let dat = Data::read_rel(overlay, ent, symdb)?;
    let code_line = dat.to_code(ent.sec_name, overlay, symdb, strings)
        .map_err(|e| e.provide_sym(ent))?;
    let symbol_name = symdb.symbol_name(addr, false);
    Ok(code_line.gen(symbol_name, ent.local))
}
