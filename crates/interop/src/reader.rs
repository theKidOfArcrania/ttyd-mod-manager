use std::backtrace::Backtrace;

mod __private {
    pub trait PtrInt {}
}

pub trait PtrInt: __private::PtrInt + Sized + Copy {
    fn read_rel<P, R>(reader: &mut R) -> Result<P::Rel<Self>, R::Error>
    where
        P: Ptr,
        R: CReader<P> + ?Sized;
}

macro_rules! impl_PtrInt {
    ($($tp:ty: $fn:ident),* $(,)?) => {
        $(impl __private::PtrInt for $tp {}

        impl PtrInt for $tp {
            fn read_rel<P, R>(reader: &mut R) -> Result<P::Rel<Self>, R::Error> where
                P: Ptr,
                R: CReader<P> + ?Sized,
            {
                reader.$fn()
            }
        })*
    };
}

impl_PtrInt! {
    u8: read_rel_u8,
    u16: read_rel_u16,
    u32: read_rel_u32,
    u64: read_rel_u64,
}

pub enum SymbolicType<T, P> {
    Value(T),
    Pointer(P),
    Unknown,
}

pub trait Symbolic<T, P: Ptr> {
    fn get_type(&self) -> SymbolicType<&T, P>;
}

pub trait Ptr: Sized {
    type Num: PtrInt + Into<Self>;
    type Rel<T>: Symbolic<T, Self>;
}

pub trait CReader<P: Ptr>: Clone {
    type Error;

    fn error_expected_eof(&self) -> Self::Error;
    fn error_complex_symbol(&self) -> Self::Error;
    fn error_bad_value(&self, val: String) -> Self::Error;
    fn error_custom(&self, msg: String) -> Self::Error;
    fn error_custom_with(
        &self,
        msg: String,
        backtrace: Backtrace,
    ) -> Self::Error;

    fn read_rel_u8(&mut self) -> Result<P::Rel<u8>, Self::Error>;
    fn read_rel_u16(&mut self) -> Result<P::Rel<u16>, Self::Error>;
    fn read_rel_u32(&mut self) -> Result<P::Rel<u32>, Self::Error>;
    fn read_rel_u64(&mut self) -> Result<P::Rel<u64>, Self::Error>;
    fn is_bounded(&self) -> bool;
    fn eof(&self) -> bool;
    fn remaining(&self) -> usize;

    fn skip(&mut self, cnt: usize) -> Result<(), Self::Error> {
        for _ in 0..cnt {
            self.read_rel_u8()?;
        }
        Ok(())
    }

    fn read_val<T: CRead<P>>(&mut self) -> Result<T, Self::Error> {
        T::read(self)
    }

    fn read_val_full<T: CRead<P>>(&mut self) -> Result<T, Self::Error> {
        let res = T::read(self)?;
        self.expect_fully_read()?;
        Ok(res)
    }

    fn read_u8(&mut self) -> Result<u8, Self::Error> {
        match self.read_rel_u8()?.get_type() {
            SymbolicType::Value(v) => Ok(*v),
            _ => Err(self.error_complex_symbol()),
        }
    }
    fn read_u16(&mut self) -> Result<u16, Self::Error> {
        match self.read_rel_u16()?.get_type() {
            SymbolicType::Value(v) => Ok(*v),
            _ => Err(self.error_complex_symbol()),
        }
    }
    fn read_u32(&mut self) -> Result<u32, Self::Error> {
        match self.read_rel_u32()?.get_type() {
            SymbolicType::Value(v) => Ok(*v),
            _ => Err(self.error_complex_symbol()),
        }
    }
    fn read_u64(&mut self) -> Result<u64, Self::Error> {
        match self.read_rel_u64()?.get_type() {
            SymbolicType::Value(v) => Ok(*v),
            _ => Err(self.error_complex_symbol()),
        }
    }
    fn read_ptr(&mut self) -> Result<P, Self::Error> {
        match P::Num::read_rel(self)?.get_type() {
            SymbolicType::Value(v) => Ok((*v).into()),
            SymbolicType::Pointer(p) => Ok(p),
            SymbolicType::Unknown => Err(self.error_complex_symbol()),
        }
    }
    fn expect_fully_read(&self) -> Result<(), Self::Error> {
        if !self.is_bounded() || self.eof() {
            Ok(())
        } else {
            Err(self.error_expected_eof())
        }
    }

    fn read_bool(&mut self) -> Result<bool, Self::Error> {
        Ok(self.read_u8()? != 0)
    }
    fn read_i8(&mut self) -> Result<i8, Self::Error> {
        Ok(self.read_u8()? as i8)
    }
    fn read_i16(&mut self) -> Result<i16, Self::Error> {
        Ok(self.read_u16()? as i16)
    }
    fn read_i32(&mut self) -> Result<i32, Self::Error> {
        Ok(self.read_u32()? as i32)
    }
    fn read_i64(&mut self) -> Result<i64, Self::Error> {
        Ok(self.read_u64()? as i64)
    }
    fn read_f32(&mut self) -> Result<f32, Self::Error> {
        Ok(f32::from_bits(self.read_u32()?))
    }
    fn read_f64(&mut self) -> Result<f64, Self::Error> {
        Ok(f64::from_bits(self.read_u64()?))
    }
}

pub trait CRead<P: Ptr>: Sized {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized;
}

macro_rules! impl_CRead {
    ($($tp:ty: $fn:ident),* $(,)?) => {
        $(impl<P: Ptr> CRead<P> for $tp {
            fn read<R>(reader: &mut R) -> Result<Self, R::Error> where
                R: CReader<P> + ?Sized,
            {
                reader.$fn()
            }
        })*
    };
}

impl_CRead! {
    u8: read_u8,
    u16: read_u16,
    u32: read_u32,
    u64: read_u64,
    i8: read_i8,
    i16: read_i16,
    i32: read_i32,
    i64: read_i64,
    f32: read_f32,
    f64: read_f64,
}

impl<P: Ptr, T: CRead<P>> CRead<P> for Vec<T> {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized,
    {
        let mut ret = Vec::new();
        while !reader.eof() {
            ret.push(T::read(reader)?);
        }
        Ok(ret)
    }
}

impl<P: Ptr, T: CRead<P>, const SIZE: usize> CRead<P> for [T; SIZE] {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized,
    {
        [(); SIZE].try_map(|_| T::read(reader))
    }
}

impl<P: Ptr> CRead<P> for P {
    fn read<R>(reader: &mut R) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized,
    {
        reader.read_ptr()
    }
}

pub trait CReadWith<P: Ptr>: Sized {
    type ValType;
    fn read_with<R>(
        reader: &mut R,
        tp: Self::ValType,
    ) -> Result<Self, R::Error>
    where
        R: CReader<P> + ?Sized;
}
