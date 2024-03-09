use std::{marker::PhantomData, mem::size_of};

pub fn assert_sane_size<T>() {
    struct AssertSmallSize<T> {
        _x: PhantomData<T>,
    }
    #[allow(dead_code)]
    impl<T> AssertSmallSize<T> {
        const OK: () = assert!(size_of::<T>() < (u32::MAX as usize));
    }

    let () = AssertSmallSize::<T>::OK;
}

#[macro_export]
macro_rules! mk_err_wrapper {
    {
        $et:ident $({
            $($err_variant:ident => $tp:ty),*
            $(,)?
        })?
    } => {
        pub type Error = $crate::error::Error<$et>;
        #[allow(unused)]
        pub type Res<T> = Result<T, $crate::error::Error<$et>>;

        $($(
            impl ::core::convert::From<$crate::error::Error<$tp>> for Error {
                fn from(value: $crate::error::Error<$tp>) -> Self {
                    let (backtrace, tp) = value.unwrap();
                    Error::new_with(backtrace, $et::$err_variant(tp))
                }
            }
        )*)?

        #[allow(unused)]
        macro_rules! error {
            ($e:expr) => {{
                use $et::*;
                Error::new($e)
            }}
        }

        #[allow(unused)]
        macro_rules! bail {
            ($e:expr) => {{
                use $et::*;
                return Err(Error::new($e));
            }}
        }
    }

}
