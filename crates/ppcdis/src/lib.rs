#![feature(lazy_cell)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![allow(incomplete_features)]

mod insn;
mod rel;

pub use insn::{Instruction, RawInsn, Error, ErrorType};
pub use rel::{RelocAction, RelocType};

