#![feature(array_try_map)]
#![feature(never_type)]

pub mod ctype;
pub mod dumper;
pub mod reader;
pub mod size;

pub use ctype::{CType, CTypeKind, CTypePrim, CTypeable, Definition};

pub use reader::{CRead, CReader, Ptr, Symbolic, SymbolicType};

pub use size::{ConstSize, Size};

pub use dumper::{dumps, CDump, Dumper};

#[cfg(feature = "derive")]
pub use interop_derive::{CDump, CRead, CTypeable, ConstSize, Size};
