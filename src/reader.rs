use std::mem::size_of;

use bytemuck::Pod;
use thiserror::Error;

use error::mk_err_wrapper;

use crate::{dol, rel, sym, utils::SaneSize};

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

pub trait SymReader {
    fn rel_pos(&self) -> u32;

    fn read<T: Pod + SaneSize>(&mut self) -> Result<rel::Symbol<T>, Error>;

    fn skip(&mut self, cnt: usize) -> Result<(), Error>;

    fn max_bound(&self) -> u32;

    fn is_bounded(&self) -> bool;
}

#[derive(Clone)]
pub struct DolReader<'r, 'b> {
    file: &'r dol::DolFile<'b>,
    start: u32,
    current: u32,
    max_bound: u32,
    bounded: bool,
}

impl<'r, 'b> DolReader<'r, 'b> {
    pub fn new(
        file: &'r dol::DolFile<'b>,
        ent: &sym::RawSymEntry,
    ) -> Self {
        let offset = ent.ram_addr.unwrap_or(0);
        Self {
            file,
            start: offset,
            current: offset,
            max_bound: ent.size,
            bounded: true,
        }
    }
}

impl<'r, 'b> SymReader for DolReader<'r, 'b> {
    fn rel_pos(&self) -> u32 {
        self.current - self.start
    }

    fn read<T: Pod + SaneSize>(&mut self) -> Result<rel::Symbol<T>, Error> {
        let data = self.file
            .lookup_section_data(self.current)
            .ok_or_else(|| error!(Custom(format!(
                "Bad address: 0x{:08x}",
                self.current,
            ))))?;
        let cnt = T::size();
        if data.len() < size_of::<T>() {
            let rel_pos = self.rel_pos();
            bail!(ReadOverflow(rel_pos, rel_pos + data.len() as u32))
        }
        if self.max_bound - self.rel_pos() < cnt {
            bail!(ReadOverflow(self.rel_pos() + cnt, self.max_bound));
        }
        self.current += cnt;
        Ok(rel::Symbol::Value(bytemuck::pod_read_unaligned(&data[0..size_of::<T>()])))
    }

    fn skip(&mut self, cnt: usize) -> Result<(), Error> {
        let cnt: u32 = cnt
            .try_into()
            .map_err(|_| error!(ReadOverflow(u32::MAX, self.max_bound)))?;
        if self.max_bound - self.rel_pos() < cnt {
            bail!(ReadOverflow(self.rel_pos() + cnt, self.max_bound));
        }
        self.current += cnt;
        Ok(())
    }

    fn max_bound(&self) -> u32 {
        self.max_bound
    }

    fn is_bounded(&self) -> bool {
        self.bounded
    }
}

#[derive(Clone)]
pub struct RelocOverlayReader<'r, 'b> {
    overlay: &'r rel::RelocOverlay<'b, 'b>,
    start: rel::SectionAddr,
    current: rel::SectionAddr,
    max_bound: u32,
    bounded: bool,
}

impl<'r, 'b> RelocOverlayReader<'r, 'b> {
    pub fn new_unsized(
        overlay: &'r rel::RelocOverlay<'b, 'b>,
        addr: rel::SectionAddr,
    ) -> Self {
        Self {
            overlay,
            start: addr,
            current: addr,
            max_bound: overlay
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
            max_bound: ent.size,
            bounded: true,
        }
    }
}

impl<'r, 'b> SymReader for RelocOverlayReader<'r, 'b> {
    fn rel_pos(&self) -> u32 {
        self.current.offset - self.start.offset
    }

    fn read<T: Pod + SaneSize>(&mut self) -> Result<rel::Symbol<T>, Error> {
        let res = self.overlay.read(self.current)?;
        let cnt = T::size();
        if self.max_bound - self.rel_pos() < cnt {
            bail!(ReadOverflow(self.rel_pos() + cnt, self.max_bound));
        }
        self.current += cnt;
        Ok(res)
    }

    fn skip(&mut self, cnt: usize) -> Result<(), Error> {
        let cnt: u32 = cnt
            .try_into()
            .map_err(|_| error!(ReadOverflow(u32::MAX, self.max_bound)))?;
        if self.max_bound - self.rel_pos() < cnt {
            bail!(ReadOverflow(self.rel_pos() + cnt, self.max_bound));
        }
        self.current += cnt;
        Ok(())
    }

    fn max_bound(&self) -> u32 {
        self.max_bound
    }

    fn is_bounded(&self) -> bool {
        self.bounded
    }
}

#[derive(Clone)]
pub struct Reader<T>(pub T);

impl<T: SymReader + Clone> interop::CReader<sym::SymAddr> for Reader<T> {
    type Error = Error;

    fn error_expected_eof(&self) -> Self::Error {
        error!(ReadUnderflow(self.0.rel_pos(), self.0.max_bound()))
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
        self.0.read::<u8>()
    }

    fn read_rel_u16(&mut self) -> Result<rel::Symbol<u16>, Self::Error> {
        self.0.read::<rel::BigU16>().map(|v| v.map(|raw| raw.get()))
    }

    fn read_rel_u32(&mut self) -> Result<rel::Symbol<u32>, Self::Error> {
        self.0.read::<rel::BigU32>().map(|v| v.map(|raw| raw.get()))
    }

    fn read_rel_u64(&mut self) -> Result<rel::Symbol<u64>, Self::Error> {
        self.0.read::<rel::BigU64>().map(|v| v.map(|raw| raw.get()))
    }

    fn is_bounded(&self) -> bool {
        self.0.is_bounded()
    }

    fn eof(&self) -> bool {
        self.0.rel_pos() >= self.0.max_bound()
    }

    fn remaining(&self) -> usize {
        self.0.max_bound().saturating_sub(self.0.rel_pos()) as usize
    }

    fn skip(&mut self, cnt: usize) -> Result<(), Self::Error> {
        self.0.skip(cnt)
    }
}

/*
impl<R: SymReader> Reader<R> {

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
}*/
