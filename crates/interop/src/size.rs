pub trait Size {
    fn len(&self) -> usize;
}

pub trait ConstSize {
    fn len() -> usize;
}

macro_rules! impl_ConstSize {
    ( $($tp:ty);* $(;)?) => {
        $(impl ConstSize for $tp {
            fn len() -> usize {
                ::core::mem::size_of::<Self>()
            }
        }

        impl Size for $tp {
            fn len(&self) -> usize {
                <Self as ConstSize>::len()
            }
        })*
    };
}

impl Size for String {
    fn len(&self) -> usize {
        Self::len(self)
    }
}

impl<T: ConstSize> Size for Vec<T> {
    fn len(&self) -> usize {
        Self::len(self) * T::len()
    }
}

impl<T: ConstSize, const SIZE: usize> ConstSize for [T; SIZE] {
    fn len() -> usize {
        SIZE * T::len()
    }
}

impl<T: ConstSize, const SIZE: usize> Size for [T; SIZE] {
    fn len(&self) -> usize {
        SIZE * T::len()
    }
}

impl_ConstSize! {
    u8; u16; u32; u64; usize;
    i8; i16; i32; i64; isize;
    f32; f64;
}
