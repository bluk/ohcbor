//! Possible crate errors.

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, string::String};
#[cfg(feature = "std")]
use std::{boxed::Box, string::String};

use core::{
    error,
    fmt::{self, Display},
    result,
    str::Utf8Error,
};

/// Alias for a [`Result`][std::result::Result] with a [`ohcbor::Error`][Error] error type.
pub type Result<T> = result::Result<T, Error>;

/// Errors during encoding and decoding.
pub struct Error {
    inner: Box<ErrorImpl>,
}

impl Error {
    /// Constructs an error with the kind and the byte offset where the error
    /// was detected.
    ///
    /// A byte offset value of `0` indicates that the byte offset is either
    /// unknown or not relevant.
    #[must_use]
    #[inline]
    pub fn new(kind: ErrorKind, byte_offset: usize) -> Self {
        Self {
            inner: Box::new(ErrorImpl { kind, byte_offset }),
        }
    }

    /// The kind of error encountered
    #[must_use]
    #[inline]
    pub fn kind(&self) -> &ErrorKind {
        &self.inner.kind
    }

    /// The byte offset where the error was detected.
    ///
    /// A byte offset value of `0` indicates that the byte offset is either
    /// unknown or not relevant.
    #[must_use]
    #[inline]
    pub fn byte_offset(&self) -> usize {
        self.inner.byte_offset
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        self.inner.source()
    }
}

#[cfg(feature = "std")]
impl From<Error> for std::io::Error {
    fn from(error: Error) -> Self {
        if let ErrorKind::Io(error) = error.inner.kind {
            return error;
        }
        std::io::Error::new(std::io::ErrorKind::Other, error.to_string())
    }
}

struct ErrorImpl {
    kind: ErrorKind,
    byte_offset: usize,
}

impl Display for ErrorImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.byte_offset == 0 {
            Display::fmt(&self.kind, f)
        } else {
            write!(f, "{} at byte offset {}", self.kind, self.byte_offset)
        }
    }
}

impl fmt::Debug for ErrorImpl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &self.kind)
            .field("byte_offset", &self.byte_offset)
            .finish()
    }
}

impl error::Error for ErrorImpl {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        error::Error::source(&self.kind)
    }
}

/// All possible crate errors.
#[allow(clippy::module_name_repetitions)]
#[non_exhaustive]
pub enum ErrorKind {
    /// General decoding error.
    ///
    /// Usually the error is due to mismatching types (e.g. a struct was expecting an u64 but the data had a string).
    Decode(String),
    /// End of file was encountered while parsing a value.
    EofWhileParsingValue,
    /// Unparsed trailing data was detected
    TrailingData,
    #[cfg(feature = "std")]
    /// An I/O error.
    Io(std::io::Error),
    /// General encoding error.
    Encode(String),
    /// An unsupported type was used during serialization.
    UnsupportedType,
    /// Data is not well formed.
    NotWellFormed,
    /// Length argument is not a valid number
    ///
    /// The length could be greater than `usize::MAX`.
    InvalidLen,
    /// Invalid UTF-8
    InvalidUtf8Error(Utf8Error),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Decode(str) | ErrorKind::Encode(str) => f.write_str(str),
            ErrorKind::EofWhileParsingValue => f.write_str("eof while parsing value"),
            ErrorKind::TrailingData => f.write_str("trailing data error"),
            ErrorKind::UnsupportedType => f.write_str("unsupported type"),
            #[cfg(feature = "std")]
            ErrorKind::Io(source) => Display::fmt(source, f),
            ErrorKind::NotWellFormed => f.write_str("data is not well formed"),
            ErrorKind::InvalidLen => f.write_str("invalid length argument"),
            ErrorKind::InvalidUtf8Error(source) => Display::fmt(source, f),
        }
    }
}

impl fmt::Debug for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Decode(str) | ErrorKind::Encode(str) => f.write_str(str),
            ErrorKind::EofWhileParsingValue => f.write_str("eof while parsing value"),
            ErrorKind::TrailingData => f.write_str("trailing data error"),
            ErrorKind::UnsupportedType => f.write_str("unsupported type"),
            #[cfg(feature = "std")]
            ErrorKind::Io(source) => fmt::Debug::fmt(source, f),
            ErrorKind::NotWellFormed => f.write_str("data is not well formed"),
            ErrorKind::InvalidLen => f.write_str("invalid length argument"),
            ErrorKind::InvalidUtf8Error(source) => Display::fmt(source, f),
        }
    }
}

impl error::Error for ErrorKind {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match self {
            ErrorKind::Decode(_)
            | ErrorKind::EofWhileParsingValue
            | ErrorKind::TrailingData
            | ErrorKind::Encode(_)
            | ErrorKind::UnsupportedType
            | ErrorKind::NotWellFormed
            | ErrorKind::InvalidLen => None,
            #[cfg(feature = "std")]
            ErrorKind::Io(source) => Some(source),
            ErrorKind::InvalidUtf8Error(source) => Some(source),
        }
    }
}
