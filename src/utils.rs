pub struct B<const RES: bool>(());
pub trait Assert {}
impl Assert for B<true> {}

pub trait SaneSize: Sized {
}

impl<T> SaneSize for T where
    B<{core::mem::size_of::<T>() <= u32::MAX as usize}>: Assert
{}
