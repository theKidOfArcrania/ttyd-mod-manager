#![feature(array_try_map)]
#![feature(never_type)]

pub mod ctype;
pub mod dumper;
pub mod reader;
pub mod size;

pub use ctype::{
    CType,
    CTypeKind,
    CTypePrim,
    CTypeable,
    Definition,
};

pub use reader::{
    CRead,
    CReader,
    Ptr,
    Symbolic,
    SymbolicType,
};

pub use size::{
    Size,
    ConstSize,
};

pub use dumper::{CDump, Dumper, dumps};

#[cfg(feature = "derive")]
pub use interop_derive::{CTypeable, CRead, ConstSize, Size, CDump};
