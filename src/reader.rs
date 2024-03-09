use thiserror::Error;

use crate::{mk_err_wrapper, rel, sym};

#[derive(Debug, Error)]
pub enum ErrorType {
    #[error("Relocation error: {0}")]
    RelFileError(rel::ErrorType),
    #[error("Read overflow: {0}/{1}")]
    ReadOverflow(u32, u32),
    #[error("Read underflow: {0}/{1}")]
    ReadUnderflow(u32, u32),
}

mk_err_wrapper! {
    ErrorType {
        RelFileError => rel::ErrorType,
    }
}

pub trait Size {
    fn len(&self) -> u32;
}

pub trait ConstSize {
    fn len() -> u32;
}

macro_rules! impl_ConstSize {
    ( $($tp:ty);* $(;)?) => {
        $(impl ConstSize for $tp {
            fn len() -> u32 {
                ::core::mem::size_of::<Self>() as u32
            }
        }

        impl Size for $tp {
            fn len(&self) -> u32 {
                <Self as ConstSize>::len()
            }
        })*
    };
}

impl Size for String {
    fn len(&self) -> u32 {
        Self::len(self) as u32
    }
}

impl<T: ConstSize> Size for Vec<T> {
    fn len(&self) -> u32 {
        Self::len(self) as u32 * T::len()
    }
}

impl<T: ConstSize, const SIZE: usize> ConstSize for [T; SIZE] {
    fn len() -> u32 {
        SIZE as u32 * T::len()
    }
}

impl<T: ConstSize, const SIZE: usize> Size for [T; SIZE] {
    fn len(&self) -> u32 {
        SIZE as u32 * T::len()
    }
}

impl ConstSize for sym::SymAddr {
    fn len() -> u32 {
        4
    }
}

impl Size for sym::SymAddr {
    fn len(&self) -> u32 {
        4
    }
}

impl_ConstSize! {
    u8; u16; u32; u64; usize;
    i8; i16; i32; i64; isize;
    f32; f64;
}

pub struct Span<T> {
    start: rel::SectionAddr,
    pub data: T,
}

impl<T> Span<T> {
    pub fn new(start: rel::SectionAddr, data: T) -> Self {
        Self { start, data }
    }
}

impl<T: Size> Size for Span<T> {
    fn len(&self) -> u32 {
        self.data.len()
    }
}

pub struct Reader<'r, 'b> {
    overlay: &'r rel::RelocOverlay<'b, 'b>,
    start: rel::SectionAddr,
    current: rel::SectionAddr,
    size: u32,
    bounded: bool,
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
        let addr = rel::SectionAddr::new(ent.sec_id, ent.sec_offset);
        Self {
            overlay,
            start: addr,
            current: addr,
            size: ent.size,
            bounded: true,
        }
    }

    pub fn start(&self) -> rel::SectionAddr {
        self.start
    }

    pub fn read_8(&mut self) -> Res<u8> {
        if self.size - self.rel_pos() < 1 {
            bail!(ReadOverflow(self.rel_pos() + 1, self.size));
        }
        let current = self.current;
        self.current += 1;
        Ok(self.overlay.read(current)?.get_value()?)
    }

    pub fn read_16(&mut self) -> Res<rel::Symbol<u16>> {
        if self.size - self.rel_pos() < 2 {
            bail!(ReadOverflow(self.rel_pos() + 2, self.size));
        }
        let current = self.current;
        self.current += 2;
        Ok(self.overlay.read_16(current)?)
    }

    pub fn read_32(&mut self) -> Res<rel::Symbol<u32>> {
        if self.size - self.rel_pos() < 4 {
            bail!(ReadOverflow(self.rel_pos() + 4, self.size));
        }
        let current = self.current;
        self.current += 4;
        Ok(self.overlay.read_32(current)?)
    }

    pub fn read_float(&mut self) -> Res<f32> {
        Ok(bytemuck::cast(self.read_32()?.get_value()?))
    }

    pub fn read_ptr(&mut self) -> Res<sym::SymAddr> {
        Ok(match self.read_32()? {
            rel::Symbol::Value(abs) => sym::SymAddr::Dol(abs),
            rel::Symbol::Rel(rel::RelocSymbol {
                file,
                target,
                rtype: rel::RelocType::PPCAddr32,
                orig: _,
            }) => sym::SymAddr::Rel(file, target),
            _  => bail!(RelFileError(rel::ErrorType::ComplexRelocation)),
        })
    }

    pub fn is_bounded(&self) -> bool {
        self.bounded
    }

    pub fn eof(&self) -> bool {
        self.rel_pos() >= self.size
    }

    pub fn expect_fully_read(&self) -> Res<()> {
        if !self.is_bounded() || self.eof() {
            Ok(())
        } else {
            Err(error!(ReadUnderflow(
                self.rel_pos(),
                self.size,
            )))
        }
    }

    pub fn rel_pos(&self) -> u32 {
        self.current.offset - self.start.offset
    }
}

