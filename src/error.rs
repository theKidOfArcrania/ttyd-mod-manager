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

#[derive(Debug, Error)]
#[error("{0}")]
pub struct Error<T: StdError>(
    #[source]
    #[backtrace]
    Box<ErrorImpl<T>>,
);

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
