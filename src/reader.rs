use std::mem::size_of;

use bytemuck::Pod;
use thiserror::Error;

use error::mk_err_wrapper;

use crate::{rel, sym, utils::SaneSize};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Relocation error: {0}")]
    RelFileError(rel::ErrorType),
    #[error("Read overflow: {0}/{1}")]
    ReadOverflow(u32, u32),
    #[error("Read underflow: {0}/{1}")]
    ReadUnderflow(u32, u32),
    #[error("Bad value while paring: {0}")]
    BadValue(String),
    #[error("{0}")]
    Custom(String),
}

mk_err_wrapper! {
    ErrorType {
        RelFileError => rel::ErrorType,
    }
}

impl interop::ConstSize for sym::SymAddr {
    fn len() -> usize {
        4
    }
}

impl interop::Size for sym::SymAddr {
    fn len(&self) -> usize {
        4
    }
}

#[derive(Clone)]
pub struct Reader<'r, 'b> {
    overlay: &'r rel::RelocOverlay<'b, 'b>,
    start: rel::SectionAddr,
    current: rel::SectionAddr,
    size: u32,
    bounded: bool,
}

impl<'r, 'b> interop::CReader<sym::SymAddr> for Reader<'r, 'b> {
    type Error = Error;

    fn error_expected_eof(&self) -> Self::Error {
        error!(ReadUnderflow(self.rel_pos(), self.size,))
    }

    fn error_complex_symbol(&self) -> Self::Error {
        error!(RelFileError(rel::ErrorType::ComplexRelocation))
    }

    fn error_bad_value(&self, val: String) -> Self::Error {
        error!(BadValue(val))
    }

    fn error_custom(&self, msg: String) -> Self::Error {
        error!(Custom(msg))
    }

    fn error_custom_with(
        &self,
        msg: String,
        backtrace: std::backtrace::Backtrace,
    ) -> Self::Error {
        Error::new_with(backtrace, ErrorType::Custom(msg))
    }

    fn read_rel_u8(&mut self) -> Result<rel::Symbol<u8>, Self::Error> {
        self.read::<u8>()
    }

    fn read_rel_u16(&mut self) -> Result<rel::Symbol<u16>, Self::Error> {
        self.read::<rel::BigU16>().map(|v| v.map(|raw| raw.get()))
    }

    fn read_rel_u32(&mut self) -> Result<rel::Symbol<u32>, Self::Error> {
        self.read::<rel::BigU32>().map(|v| v.map(|raw| raw.get()))
    }

    fn read_rel_u64(&mut self) -> Result<rel::Symbol<u64>, Self::Error> {
        self.read::<rel::BigU64>().map(|v| v.map(|raw| raw.get()))
    }

    fn is_bounded(&self) -> bool {
        self.bounded
    }

    fn eof(&self) -> bool {
        self.rel_pos() >= self.size
    }

    fn remaining(&self) -> usize {
        self.size.saturating_sub(self.rel_pos()) as usize
    }

    fn skip(&mut self, cnt: usize) -> Result<(), Self::Error> {
        let cnt: u32 = cnt
            .try_into()
            .map_err(|_| error!(ReadOverflow(u32::MAX, self.size)))?;
        if self.size - self.rel_pos() < cnt {
            bail!(ReadOverflow(self.rel_pos() + 1, self.size));
        }
        self.current += cnt;
        Ok(())
    }
}

impl<'r, 'b> Reader<'r, 'b> {
    pub fn new_unsized(
        overlay: &'r rel::RelocOverlay<'b, 'b>,
        addr: rel::SectionAddr,
    ) -> Self {
        Self {
            overlay,
            start: addr,
            current: addr,
            size: overlay
                .backing()
                .section_header(addr.sect)
                .expect("section should exist")
                .length(),
            bounded: false,
        }
    }
    pub fn new(
        overlay: &'r rel::RelocOverlay<'b, 'b>,
        ent: &sym::RawSymEntry,
    ) -> Self {
        let addr = ent.section_addr();
        Self {
            overlay,
            start: addr,
            current: addr,
            size: ent.size,
            bounded: true,
        }
    }

    pub fn rel_pos(&self) -> u32 {
        self.current.offset - self.start.offset
    }

    fn read<T: Pod + SaneSize>(&mut self) -> Res<rel::Symbol<T>> {
        if self.size - self.rel_pos() < size_of::<T>() as u32 {
            bail!(ReadOverflow(self.rel_pos() + 1, self.size));
        }
        let current = self.current;
        self.current += size_of::<T>() as u32;
        Ok(self.overlay.read(current)?)
    }
}
