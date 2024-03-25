use std::{
    collections::HashMap,
    hash::Hash,
    marker::PhantomData,
    sync::{Arc, Mutex, OnceLock},
};

struct KeyCType<K: ?Sized>(PhantomData<*const K>);
impl<K: 'static + ?Sized> typemap::Key for KeyCType<K> {
    type Value = CTypeKind;
}

struct KeyPoisoned<K: ?Sized>(PhantomData<*const K>);
impl<K: 'static + ?Sized> typemap::Key for KeyPoisoned<K> {
    type Value = ();
}

pub trait CTypeable {
    type Canonical: 'static + ?Sized;

    fn compute_type() -> CTypeKind;

    fn get_type() -> CTypeKind {
        static TYPE: OnceLock<Mutex<typemap::ShareMap>> = OnceLock::new();
        let map = TYPE.get_or_init(|| Mutex::new(typemap::ShareMap::custom()));

        // See if we already cached it
        let mut map_lock = map.lock().expect("should not be poisoned");
        if let Some(ret) = map_lock.get::<KeyCType<Self::Canonical>>() {
            return ret.clone();
        }
        if map_lock
            .insert::<KeyPoisoned<Self::Canonical>>(())
            .is_some()
        {
            panic!("Recursive types are not supported!");
        }
        drop(map_lock);

        // Otherwise try to compute the type once
        let res = Self::compute_type();

        // ... favoring if we already have a type existing here.
        let mut map_lock = map.lock().expect("should not be poisoned");
        match map_lock.entry::<KeyCType<Self::Canonical>>() {
            typemap::Entry::Occupied(ent) => ent.get().clone(),
            typemap::Entry::Vacant(ent) => ent.insert(res).clone(),
        }
    }
}

macro_rules! typeable_prim {
    ($($tp:ty: $prim:ident),* $(,)?) => {
        $(
            impl CTypeable for $tp {
                type Canonical = Self;
                fn compute_type() -> CTypeKind {
                    CTypeKind::Prim(CTypePrim::$prim)
                }
            }
            impl CTypeable for [$tp] {
                type Canonical = Self;
                fn compute_type() -> CTypeKind {
                    CTypeKind::Array(None, CTypePrim::$prim)
                }
            }
            impl<const SIZE: usize> CTypeable for [$tp; SIZE] {
                type Canonical = Self;
                fn compute_type() -> CTypeKind {
                    CTypeKind::Array(Some(SIZE), CTypePrim::$prim)
                }
            }
        )*
    }
}

typeable_prim! {
    i8:  I8,
    i16: I16,
    i32: I32,
    i64: I64,
    u8:  U8,
    u16: U16,
    u32: U32,
    u64: U64,
    f32: F32,
    f64: F64,
}

impl CTypeable for str {
    type Canonical = Self;
    fn compute_type() -> CTypeKind {
        CTypeKind::Array(None, CTypePrim::I8)
    }
}

impl CTypeable for String {
    type Canonical = Self;
    fn compute_type() -> CTypeKind {
        CTypeKind::Ptr(Box::new(CType::new(
            true,
            CTypeKind::Array(None, CTypePrim::I8),
        )))
    }
}

impl<T: CTypeable> CTypeable for [&T] {
    type Canonical = [&'static T::Canonical];
    fn compute_type() -> CTypeKind {
        CTypeKind::PtrArray(None, Box::new(CType::new(false, T::get_type())))
    }
}

impl<const SIZE: usize, T: CTypeable> CTypeable for [&T; SIZE] {
    type Canonical = [&'static T::Canonical; SIZE];

    fn compute_type() -> CTypeKind {
        CTypeKind::PtrArray(
            Some(SIZE),
            Box::new(CType::new(false, T::get_type())),
        )
    }
}

pub struct Definition {
    pub declare: Option<String>,
    pub definition: String,
    pub order: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CTypePrim {
    Void,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
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
            CTypePrim::I64 => "long long",
            CTypePrim::U8 => "unsigned char",
            CTypePrim::U16 => "unsigned short",
            CTypePrim::U32 => "unsigned int",
            CTypePrim::U64 => "unsigned long long",
            CTypePrim::F32 => "float",
            CTypePrim::F64 => "double",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Field {
    pub name: String,
    pub tp: Box<CType>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CStruct {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Copy, Clone, Debug)]
pub enum Number {
    Signed(i64),
    Unsigned(u64),
}

impl Number {
    fn cast(&self) -> u64 {
        match self {
            Self::Signed(i) => *i as u64,
            Self::Unsigned(u) => *u,
        }
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.cast() == other.cast()
    }
}

impl Eq for Number {}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Variant {
    pub name: String,
    pub value: Number,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CEnum {
    pub name: String,
    variants: Vec<Variant>,
    lookup_value: HashMap<Number, usize>,
    lookup_name: HashMap<String, usize>,
}

impl CEnum {
    pub fn new(name: String, variants: Vec<Variant>) -> Self {
        let mut lookup_value = HashMap::new();
        let mut lookup_name = HashMap::new();
        for (i, v) in variants.iter().enumerate() {
            lookup_name.insert(v.name.clone(), i);
            lookup_value.insert(v.value, i);
        }
        Self {
            name,
            variants,
            lookup_name,
            lookup_value,
        }
    }

    pub fn variants(&self) -> &[Variant] {
        &self.variants
    }

    pub fn lookup_by_name(&self, name: &str) -> Option<&Variant> {
        self.lookup_name.get(name).map(|ind| &self.variants[*ind])
    }

    pub fn lookup_by_value(&self, value: Number) -> Option<&Variant> {
        self.lookup_value
            .get(&value)
            .map(|ind| &self.variants[*ind])
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CTDefType {
    Struct(CStruct),
    Enum(CEnum),
}

impl CTDefType {
    pub fn name(&self) -> &str {
        match self {
            CTDefType::Struct(s) => &s.name,
            CTDefType::Enum(e) => &e.name,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum CTypeKind {
    Prim(CTypePrim),
    TDef(Arc<CTDefType>),
    Array(Option<usize>, CTypePrim),
    PtrArray(Option<usize>, Box<CType>),
    Ptr(Box<CType>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct CType {
    pub kind: CTypeKind,
    pub const_: bool,
}

#[macro_export]
macro_rules! ctype {
    (const $($rest:tt)+) => {
        $crate::CType::new(true, $crate::ctype_kind!($($rest)+))
    };
    (mut $($rest:tt)+) => {
        $crate::CType::new(false, $crate::ctype_kind!($($rest)+))
    };
}

#[macro_export]
macro_rules! ctype_prim {
    (void) => {
        $crate::CTypePrim::Void
    };
    (i8) => {
        $crate::CTypePrim::I8
    };
    (i16) => {
        $crate::CTypePrim::I16
    };
    (i32) => {
        $crate::CTypePrim::I32
    };
    (i64) => {
        $crate::CTypePrim::I64
    };
    (f32) => {
        $crate::CTypePrim::F32
    };
    (f64) => {
        $crate::CTypePrim::F64
    };
}

#[macro_export]
macro_rules! ctype_kind {
    (void) => { $crate::CTypeKind::Prim($crate::CTypePrim::Void) };
    (i8) => { $crate::CTypeKind::Prim($crate::CTypePrim::I8) };
    (i16) => { $crate::CTypeKind::Prim($crate::CTypePrim::I16) };
    (i32) => { $crate::CTypeKind::Prim($crate::CTypePrim::I32) };
    (i64) => { $crate::CTypeKind::Prim($crate::CTypePrim::I64) };
    (f32) => { $crate::CTypeKind::Prim($crate::CTypePrim::F32) };
    (f64) => { $crate::CTypeKind::Prim($crate::CTypePrim::F64) };
    ($tdef:ident) => { $crate::CTypeKind::TDef(stringify!($tdef)) };
    ([&{$e:expr}]) => { $crate::CTypeKind::PtrArray(None, ::std::boxed::Box::new($e)) };
    ([&$($rest:tt)+]) => {
        $crate::CTypeKind::PtrArray(
            None,
            ::std::boxed::Box::new($crate::ctype!($($rest)+),
        ))
    };
    ([$tp:tt]) => { $crate::CTypeKind::Array(None, $crate::ctype_prim!($tp)) };
    ([$tp:tt; $sz:expr]) => {
        $crate::CTypeKind::Array(Some($sz), $crate::ctype_prim!($tp))
    };
    (&{$e:expr}) => { $crate::CTypeKind::Ptr(::std::boxed::Box::new($e)) };
    (&$($rest:tt)+) => {
        $crate::CTypeKind::Ptr(::std::boxed::Box::new($crate::ctype!($($rest)+)))
    };
}

impl CType {
    pub fn new(const_: bool, kind: CTypeKind) -> Self {
        Self { kind, const_ }
    }

    pub fn get_pointee(&self) -> Option<Self> {
        match &self.kind {
            CTypeKind::Prim(_) => None,
            CTypeKind::TDef(_) => None,
            CTypeKind::Array(_, p) => {
                Some(Self::new(false, CTypeKind::Prim(*p)))
            }
            CTypeKind::PtrArray(_, elem) => Some(ctype!(
                mut &{(&**elem).clone()}
            )),
            CTypeKind::Ptr(elem) => Some((&**elem).clone()),
        }
    }

    fn make_decl_parts(&self) -> (bool, String, String) {
        match &self.kind {
            CTypeKind::Prim(prim) => {
                (self.const_, prim.c_type().to_string(), String::new())
            }
            CTypeKind::TDef(s) => (self.const_, s.name().into(), String::new()),
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
                    if self.const_ { "const " } else { "" },
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
                    if !arr_data.is_empty() { "*" } else { "" },
                    if self.const_ { "const " } else { "" },
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
            if local { "static " } else { "" },
            if cst { "const " } else { "" },
            if value.is_some() { " = " } else { "" },
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
                if cst { "const " } else { "" },
            ))
        };
        Definition {
            order: 0,
            declare,
            definition,
        }
    }
}

impl std::fmt::Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (cst, base_type, array) = self.make_decl_parts();
        write!(
            f,
            "{}{base_type}{}",
            if cst { "const " } else { "" },
            if array.is_empty() { "" } else { "*" }
        )
    }
}
