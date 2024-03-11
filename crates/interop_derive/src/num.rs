use syn::parse::{Parse, ParseStream};
use core::mem::size_of;
use quote::format_ident;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Number {
    Signed(i64),
    Unsigned(u64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Signed(i) => write!(f, "{i}"),
            Self::Unsigned(u) => write!(f, "{u}"),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Number::Signed(a), Number::Signed(b)) => Some(a.cmp(b)),
            (Number::Unsigned(a), Number::Unsigned(b)) => Some(a.cmp(b)),
            _ => None,
        }
    }
}

impl std::ops::Add<u64> for Number {
    type Output = Number;

    fn add(self, rhs: u64) -> Self::Output {
        match self {
            Number::Signed(i) => Number::Signed(i + rhs as i64),
            Number::Unsigned(u) => Number::Unsigned(u + rhs),
        }
    }
}

macro_rules! impl_PrimType {
    ($($signed: ident $var:ident: $tp:ty),* $(,)?) => {
        #[derive(Copy, Clone)]
        pub enum PrimType {
            $($var),*
        }

        impl Parse for PrimType {
            fn parse(input: ParseStream) -> syn::Result<Self> {
                let ty: syn::Ident = input.parse()?;
                Ok(match ty.to_string().as_str() {
                    $(stringify!($tp) => Self::$var,)*
                    _ => {
                        return Err(syn::Error::new_spanned(
                            ty,
                            "Invalid integer type: {ty}"
                        ));
                    }
                })
            }
        }

        impl PrimType {
            pub fn repr(self) -> &'static str {
                match self {
                    $(Self::$var => stringify!($tp),)*
                }
            }

            pub fn is_signed(self) -> bool {
                #[allow(non_upper_case_globals)]
                const Signed: bool = true;
                #[allow(non_upper_case_globals)]
                const Unsigned: bool = false;
                match self {
                    $(Self::$var => $signed,)*
                }
            }

            pub fn read_num_fn(self) -> syn::Ident {
                match self {
                    $(Self::$var => format_ident!("read_{}", stringify!($tp)),)*
                }
            }

            pub fn size(self) -> usize {
                match self {
                    $(Self::$var => size_of::<$tp>(),)*
                }
            }
        }
    }
}

impl_PrimType! {
    Signed I8: i8,
    Signed I16: i16,
    Signed I32: i32,
    Signed I64: i64,
    Unsigned U8: u8,
    Unsigned U16: u16,
    Unsigned U32: u32,
    Unsigned U64: u64,
}
