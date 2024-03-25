#![feature(error_generic_member_access)]

use std::{backtrace::Backtrace, error::Error as StdError};
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{tp}")]
struct ErrorImpl<T: StdError> {
    #[source]
    tp: T,
    #[backtrace]
    backtrace: ::std::backtrace::Backtrace,
}

#[derive(Debug)]
pub struct Error<T: StdError>(
    Box<ErrorImpl<T>>,
);

impl<T: StdError> std::fmt::Display for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&*self.0, f)
    }
}

impl<T: StdError + 'static> std::error::Error for Error<T> {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.0.source()
    }

    fn cause(&self) -> Option<&dyn StdError> {
        #[allow(deprecated)]
        self.0.cause()
    }

    fn provide<'a>(&'a self, request: &mut std::error::Request<'a>) {
        self.0.provide(request)
    }
}

pub trait ErrorWrap {
    type E: std::error::Error;
}

impl<T: StdError> Error<T> {
    pub fn new(tp: T) -> Self {
        Self(Box::new(ErrorImpl {
            tp,
            backtrace: Backtrace::capture(),
        }))
    }

    pub fn new_with(backtrace: Backtrace, tp: T) -> Self {
        Self(Box::new(ErrorImpl { tp, backtrace }))
    }

    pub fn backtrace(&self) -> &Backtrace {
        &self.0.backtrace
    }

    pub fn error_type(&self) -> &T {
        &self.0.tp
    }

    pub fn unwrap(self) -> (std::backtrace::Backtrace, T) {
        (self.0.backtrace, self.0.tp)
    }
}


#[macro_export]
macro_rules! mk_err_wrapper {
    {
        $et:ident $({
            $($err_variant:ident => $tp:ty),*
            $(,)?
        })?
    } => {
        impl $crate::ErrorWrap for $et {
            type E = Error;
        }

        #[derive(Debug)]
        #[repr(transparent)]
        pub struct Error($crate::Error<$et>);
        impl ::core::ops::Deref for Error {
            type Target = $crate::Error<$et>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl ::core::ops::DerefMut for Error {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl ::std::fmt::Display for Error {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                ::std::fmt::Display::fmt(&self.0, f)
            }
        }

        impl Error {
            pub fn new(tp: $et) -> Self {
                Self($crate::Error::new(tp))
            }

            pub fn new_with(backtrace: ::std::backtrace::Backtrace, tp: $et) -> Self {
                Self($crate::Error::new_with(backtrace, tp))
            }

            pub fn unwrap(self) -> (std::backtrace::Backtrace, $et) {
                self.0.unwrap()
            }
        }

        impl ::std::error::Error for Error {
            fn source(&self) -> Option<&(dyn ::std::error::Error + 'static)> {
                self.0.source()
            }

            fn cause(&self) -> Option<&dyn ::std::error::Error> {
                #[allow(deprecated)]
                self.0.cause()
            }

            fn provide<'a>(&'a self, request: &mut ::std::error::Request<'a>) {
                self.0.provide(request)
            }
        }

        #[allow(unused)]
        pub type Res<T> = Result<T, Error>;

        $($(
            impl ::core::convert::From<<$tp as $crate::ErrorWrap>::E> for Error {
                fn from(value: <$tp as $crate::ErrorWrap>::E) -> Self {
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
