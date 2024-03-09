use thiserror::Error;

use crate::{evt, mk_err_wrapper, reader, rel, sym};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("{0}")]
    ReaderError(reader::ErrorType),
    #[error("{0}")]
    ScriptError(evt::ErrorType),
    #[error("Unable to encode string to/from shift_jis")]
    EncodingError,
}

mk_err_wrapper!{
    ErrorType {
        ReaderError => reader::ErrorType,
        ScriptError => evt::ErrorType,
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
        vartype: CType,
        value: Option<String>,
    }
}

pub struct Definition {
    pub declare: Option<String>,
    pub definition: String,
}

impl CodeLine {
    pub fn gen(&self, ident: String, local: bool) -> Definition {
        match self {
            CodeLine::Variable { vartype, value } => {
                vartype.make_decl(ident, value.clone(), local)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CTypePrim {
    Void,
    I8,
    I16,
    I32,
    F32,
    F64,
}

impl CTypePrim {
    pub fn c_type(&self) -> &'static str {
        match self {
            CTypePrim::Void => "void",
            CTypePrim::I8 => "char",
            CTypePrim::I16 => "short",
            // TODO: assume int = 32 bit
            CTypePrim::I32 => "int",
            CTypePrim::F32 => "float",
            CTypePrim::F64 => "double",
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum CTypeKind {
    Prim(CTypePrim),
    TDef(String),
    Array(Option<usize>, CTypePrim),
    PtrArray(Option<usize>, Box<CType>),
    Ptr(Box<CType>),
}

#[derive(Clone, PartialEq, Eq)]
pub struct CType {
    kind: CTypeKind,
    const_: bool,
}

macro_rules! ctype {
    (const $($rest:tt)+) => {
        CType::new(true, ctype_kind!($($rest)+))
    };
    (mut $($rest:tt)+) => {
        CType::new(false, ctype_kind!($($rest)+))
    };
}

macro_rules! ctype_prim {
    (void) => { CTypePrim::Void };
    (i8) => { CTypePrim::I8 };
    (i16) => { CTypePrim::I16 };
    (i32) => { CTypePrim::I32 };
    (f32) => { CTypePrim::F32 };
    (f64) => { CTypePrim::F64 };
}

macro_rules! ctype_kind {
    (void) => { CTypeKind::Prim(CTypePrim::Void) };
    (i8) => { CTypeKind::Prim(CTypePrim::I8) };
    (i16) => { CTypeKind::Prim(CTypePrim::I16) };
    (i32) => { CTypeKind::Prim(CTypePrim::I32) };
    (f32) => { CTypeKind::Prim(CTypePrim::F32) };
    (f64) => { CTypeKind::Prim(CTypePrim::F64) };
    ($tdef:ident) => { CTypeKind::TDef(stringify!($tdef)) };
    ([&{$e:expr}]) => { CTypeKind::PtrArray(None, Box::new($e)) };
    ([&$($rest:tt)+]) => { CTypeKind::PtrArray(None, Box::new(ctype!($($rest)+))) };
    ([$tp:tt]) => { CTypeKind::Array(None, ctype_prim!($tp)) };
    ([$tp:tt; $sz:expr]) => { CTypeKind::Array(Some($sz), ctype_prim!($tp)) };
    (&{$e:expr}) => { CTypeKind::Ptr(Box::new($e)) };
    (&$($rest:tt)+) => { CTypeKind::Ptr(Box::new(ctype!($($rest)+))) };
}

impl CType {
    pub fn new(const_: bool, kind: CTypeKind) -> Self {
        Self { kind, const_ }
    }

    pub fn get_pointee(&self) -> Option<Self> {
        match &self.kind {
            CTypeKind::Prim(_) => None,
            CTypeKind::TDef(_) => None,
            CTypeKind::Array(_, p) => Some(Self::new(
                false,
                CTypeKind::Prim(*p),
            )),
            CTypeKind::PtrArray(_, elem) => Some(ctype!(
                mut &{(&**elem).clone()}
            )),
            CTypeKind::Ptr(elem) => Some((&**elem).clone()),
        }
    }

    fn make_decl_parts(&self) -> (bool, String, String) {
        match &self.kind {
            CTypeKind::Prim(prim) => (
                self.const_,
                prim.c_type().to_string(),
                String::new(),
            ),
            CTypeKind::TDef(s) => (self.const_, s.to_string(), String::new()),
            CTypeKind::Array(dim, elem) => (
                self.const_,
                elem.c_type().to_string(),
                match dim {
                    None => "[]".into(),
                    Some(dim) => format!("[{dim}]"),
                },
            ),
            CTypeKind::PtrArray(dim, elem) => {
                let (cst, mut base_type, _) = elem.make_decl_parts();
                base_type.push_str(&format!(
                    "*{}",
                    if self.const_ {
                        "const "
                    } else {
                        ""
                    },
                ));

                let arr_data = match dim {
                    None => "[]".into(),
                    Some(dim) => format!("[{dim}]"),
                };

                (cst, base_type, arr_data)
            }
            CTypeKind::Ptr(elem) => {
                let (cst, mut base_type, arr_data) = elem.make_decl_parts();
                base_type.push_str(&format!(
                    "{}*{}",
                    if !arr_data.is_empty() {
                        "*"
                    } else {
                        ""
                    },
                    if self.const_ {
                        "const "
                    } else {
                        ""
                    },
                ));
                (cst, base_type, String::new())
            }
        }
    }

    pub fn make_decl(
        &self,
        name: String,
        value: Option<String>,
        local: bool,
    ) -> Definition {
        let (cst, base_type, array) = self.make_decl_parts();
        let definition = format!(
            "{}{}{base_type} {name}{array}{}{};",
            if local {
                "static "
            } else {
                ""
            },
            if cst {
                "const "
            } else {
                ""
            },
            if value.is_some() {
                " = "
            } else {
                ""
            },
            match value.as_ref() {
                None => "",
                Some(s) => s,
            },
        );
        let declare = if local {
            None
        } else {
            Some(format!(
                "extern {}{base_type} {name}{array};",
                if cst {
                    "const "
                } else {
                    ""
                },
            ))
        };
        Definition { declare, definition }
    }
}

impl std::fmt::Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (cst, base_type, array) = self.make_decl_parts();
        write!(
            f,
            "{}{base_type}{}",
            if cst {
                "const "
            } else {
                ""
            },
            if array.is_empty() {
                ""
            } else {
                "*"
            }
        )
    }
}

pub struct Zeroed(pub u32);

impl reader::Size for Zeroed {
    fn len(&self) -> u32 {
        self.0
    }
}

pub enum Data {
    AsmFunc(Vec<u8>),
    CFunc(String),
    PtrArr(Vec<sym::SymAddr>),
    Ptr(sym::SymAddr),
    String(String),
    Float(Vec<f32>),
    Double(f64),
    Zero(Zeroed),
    Evt(evt::Script),
    Vec3([f32; 3]),
}

impl Data {
    pub fn read(
        overlay: &rel::RelocOverlay,
        ent: &sym::RawSymEntry,
    ) -> Res<reader::Span<Self>> {
        let mut reader = reader::Reader::new(overlay, ent);
        Ok(reader::Span::new(reader.start(), match ent.value_type {
            sym::DataType::Simple(tp) => match tp {
                sym::SimpleType::PtrArr => {
                    let mut arr = Vec::new();
                    while !reader.eof() {
                        arr.push(reader.read_ptr()?);
                    }
                    Data::PtrArr(arr)
                }
                sym::SimpleType::Ptr => {
                    let ptr = reader.read_ptr()?;
                    reader.expect_fully_read()?;
                    Data::Ptr(ptr)
                }
                sym::SimpleType::String => {
                    let mut vec = Vec::new();
                    while !reader.eof() {
                        vec.push(reader.read_8()?);
                    }
                    let (s, _, errors) = encoding_rs::SHIFT_JIS.decode(&vec);
                    if errors {
                        bail!(EncodingError);
                    }
                    Data::String(s.to_string())
                }
                sym::SimpleType::Float => {
                    let mut vec = Vec::new();
                    while !reader.eof() {
                        vec.push(reader.read_float()?);
                    }
                    reader.expect_fully_read()?;
                    Data::Float(vec)
                }
                sym::SimpleType::Double => {
                    let upper = reader.read_32()?.get_value()? as u64;
                    let lower = reader.read_32()?.get_value()? as u64;
                    reader.expect_fully_read()?;
                    Data::Double(bytemuck::cast(upper << 32 | lower))
                }
                sym::SimpleType::Zero => {
                    Data::Zero(Zeroed(ent.size))
                }
                sym::SimpleType::Evt => {
                    Data::Evt(evt::Script::parse(&mut reader)?)
                }
                sym::SimpleType::Function => {
                    // TODO:
                    Data::Zero(Zeroed(ent.size))
                }
                sym::SimpleType::Vec3 => {
                    let x = reader.read_float()?;
                    let y = reader.read_float()?;
                    let z = reader.read_float()?;
                    Data::Vec3([x, y, z])
                },
            },
            sym::DataType::Class(_) => {
                // TODO:
                Data::Zero(Zeroed(ent.size))
            },
        }))
    }

    pub fn get_type(
        &self,
        sec_type: sym::SectionType,
        overlay: &rel::RelocOverlay,
        symdb: &sym::SymbolDatabase,
    ) -> CType {
        let kind = match self {
            Data::AsmFunc(_) => ctype_kind!([void]),
            Data::CFunc(_) => ctype_kind!([void]),
            Data::PtrArr(addrs) => {
                let mut pointee = None;
                for addr in addrs {
                    let cur_pointee = symdb.get(*addr)
                        .and_then(|sym| Some(Data::read(overlay, &sym)
                            .ok()?
                            .data
                            .get_type(sym.sec_name, overlay, symdb)
                        ))
                        .unwrap_or_else(|| ctype!(mut void));

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

                let pointee = pointee.unwrap_or_else(|| ctype!(mut void));
                ctype_kind!([&{pointee}])
            },
            Data::Ptr(addr) => {
                let pointee = symdb.get(*addr)
                    .and_then(|sym| Some(Data::read(overlay, &sym)
                        .ok()?
                        .data
                        .get_type(sym.sec_name, overlay, symdb)
                    ))
                    .unwrap_or_else(|| ctype!(mut void));
                ctype_kind!(&{pointee})
            }
            Data::String(_) => ctype_kind!([i8]),
            Data::Float(values) => {
                if values.len() == 1 {
                    ctype_kind!(f32)
                } else {
                    ctype_kind!([f32])
                }
            },
            Data::Double(_) => ctype_kind!(f64),
            Data::Zero(_) => ctype_kind!([i8]),
            Data::Evt(_) => ctype_kind!([i32]),
            Data::Vec3(_) => ctype_kind!([f32; 3]),
        };

        CType::new(sec_type.is_ro(), kind)
    }

    pub fn to_code(
        &self,
        sec_type: sym::SectionType,
        overlay: &rel::RelocOverlay,
        symdb: &sym::SymbolDatabase,
    ) -> Res<CodeLine> {
        let vartype = self.get_type(sec_type, overlay, symdb);
        let mut value = match self {
            Data::AsmFunc(_) => todo!(),
            Data::CFunc(_) => todo!(),
            Data::PtrArr(addrs) => {
                let pointee_tp = vartype
                    .get_pointee()
                    .expect("should have pointee");
                let cast = match &pointee_tp.kind {
                    CTypeKind::Prim(_)
                    | CTypeKind::TDef(_) => "".into(),
                    CTypeKind::Array(_, _)
                    | CTypeKind::PtrArray(_, _)
                    | CTypeKind::Ptr(_) => pointee_tp.to_string(),
                };

                let mut ret = String::new();
                ret.push_str("{");
                for (i, addr) in addrs.iter().enumerate() {
                    let name = symdb.symbol_name(*addr);
                    if i > 0 {
                        ret.push_str(", ");
                    }
                    ret.push_str(&format!("({cast}){name}"));
                }
                ret.push_str("}");
                Some(ret)
            },
            Data::Ptr(addr) => {
                let name = symdb.symbol_name(*addr);
                let pointee_tp = vartype
                    .get_pointee()
                    .expect("should have pointee");
                let cast = match &pointee_tp.kind {
                    CTypeKind::Prim(_)
                    | CTypeKind::TDef(_) => "".into(),
                    CTypeKind::Array(_, _)
                    | CTypeKind::PtrArray(_, _)
                    | CTypeKind::Ptr(_) => pointee_tp.to_string(),
                };
                Some(format!("({cast}){name}"))
            }
            Data::String(dt) => {
                let (raw_bytes, _, errors) = encoding_rs::SHIFT_JIS.encode(&dt);
                let mut escaped = String::new();
                if errors {
                    bail!(EncodingError);
                }
                for b in &*raw_bytes {
                    if let Ok(s) = std::str::from_utf8(&[*b]) {
                        if b.is_ascii_graphic() {
                            escaped.push_str(s);
                            continue;
                        }
                    }
                    escaped.push_str(&format!("\\x{b:02x}"));
                }
                Some(format!("\"{escaped}\""))
            }
            Data::Float(dt) => {
                match dt.as_slice() {
                    [single] => Some(single.to_string()),
                    _ => {
                        let mut value = String::new();
                        value.push_str("{");
                        for (i, val) in dt.iter().enumerate() {
                            if i > 0 {
                                value.push_str(", ");
                            }
                            value.push_str(&val.to_string());
                        }
                        value.push_str("}");
                        Some(value)
                    }
                }
            },
            Data::Double(dt) => Some(format!("{dt}")),
            Data::Zero(dt) => {
                let mut value = String::with_capacity(dt.0 as usize * 3 + 10);
                value.push_str("{");
                for i in 0..dt.0 {
                    if i > 0 {
                        value.push_str(", ");
                    }
                    value.push_str("0");
                }
                value.push_str("}");

                Some(value)
            },
            Data::Evt(dt) => Some(sym::SymContext {
                symdb,
                area: overlay.backing().header().id.get(),
                val: dt,
            }.to_string()),
            Data::Vec3(dt) => Some(format!(
                "{{{0}, {1}, {2}}}",
                dt[0],
                dt[1],
                dt[2],
            )),
        };

        if let sym::SectionType::Bss = sec_type {
            value = None;
        }

        Ok(CodeLine::Variable {
            vartype,
            value,
        })
    }
}

impl reader::Size for Data {
    fn len(&self) -> u32 {
        match &self {
            Data::AsmFunc(dat) => dat as &dyn reader::Size,
            Data::CFunc(dat) => dat,
            Data::PtrArr(dat) => dat,
            Data::Ptr(dat) => dat,
            Data::String(dat) => dat,
            Data::Float(dat) => dat,
            Data::Double(dat) => dat,
            Data::Zero(dat) => dat,
            Data::Evt(dat) => dat,
            Data::Vec3(dat) => dat,
        }.len()
    }
}

pub fn generate_line(
    overlay: &rel::RelocOverlay,
    symdb: &sym::SymbolDatabase,
    ent: &sym::RawSymEntry,
) -> Res<Definition> {
    let area_id = overlay.backing().header().id.get();
    let addr = rel::SectionAddr::new(ent.sec_id, ent.sec_offset);
    let dat = Data::read(overlay, ent)?;
    let code_line = dat.data
        .to_code(ent.sec_name, overlay, symdb)?;
    let symbol_name = symdb.symbol_name(sym::SymAddr::Rel(area_id, addr));
    Ok(code_line.gen(symbol_name, ent.local))
}
