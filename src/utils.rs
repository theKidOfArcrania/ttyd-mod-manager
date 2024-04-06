pub struct B<const RES: bool>(());
pub trait Assert {}
impl Assert for B<true> {}

pub trait SaneSize: Sized {
    fn size() -> u32 {
        core::mem::size_of::<Self>() as u32
    }
}

impl<T> const SaneSize for T where
    B<{core::mem::size_of::<T>() <= u32::MAX as usize}>: Assert
{}
