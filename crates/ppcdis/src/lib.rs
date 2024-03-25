#![feature(lazy_cell)]
#![feature(const_trait_impl)]
#![feature(generic_const_exprs)]
#![feature(error_generic_member_access)]
#![allow(incomplete_features)]

mod insn;
mod rel;

pub use insn::{Instruction, RawInsn, Error, ErrorType, Operand, Num, Number, RelValue, ImmOpType};
pub use rel::{RelocAction, RelocType};

