//! [Read] trait and helpers to read bytes for the deserializer.

use crate::error::{Error, ErrorKind, Result};
use core::ops::Deref;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::io;

/// A reference to borrowed data.
///
/// The variant determines if the slice comes from a long lived source (e.g. an
/// existing byte array) or if it comes from a temporary buffer.
///
/// In the deserializer code, the different variants determine which visitor
/// method to call (e.g. `visit_borrowed_str` vs. `visit_str`).  Each variant
/// has a different lifetime which is what the compiler uses to ensure the data
/// will live long enough.
#[derive(Debug)]
pub enum Ref<'a, 'b, T>
where
    T: 'static + ?Sized,
{
    /// Reference from the original source of data.
    Source(&'a T),
    /// Reference from the given data buffer.
    Buffer(&'b T),
}

impl<T> Deref for Ref<'_, '_, T>
where
    T: 'static + ?Sized,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match *self {
            Ref::Source(s) => s,
            Ref::Buffer(b) => b,
        }
    }
}

/// Trait used by the [`de::Deserializer`][crate::de::Deserializer] to read bytes.
pub trait Read<'a> {
    /// Consumes and returns the next read byte.
    fn next(&mut self) -> Option<Result<u8>>;

    /// Returns the next byte but does not consume.
    ///
    /// Repeated peeks (with no [`next()`][Read::next] call) should return the same byte.
    fn peek(&mut self) -> Option<Result<u8>>;

    /// Returns the position in the stream of bytes.
    fn byte_offset(&self) -> usize;
}

/// A wrapper to implement this crate's [Read] trait for [`std::io::Read`] trait implementations.
#[cfg(feature = "std")]
#[derive(Debug)]
#[expect(clippy::module_name_repetitions)]
pub struct IoRead<R>
where
    R: io::Read,
{
    iter: io::Bytes<R>,
    peeked_byte: Option<u8>,
    byte_offset: usize,
}

#[cfg(feature = "std")]
impl<R> IoRead<R>
where
    R: io::Read,
{
    /// Instantiates a new reader.
    pub fn new(reader: R) -> Self {
        IoRead {
            iter: reader.bytes(),
            peeked_byte: None,
            byte_offset: 0,
        }
    }
}

#[cfg(feature = "std")]
impl<R> Read<'_> for IoRead<R>
where
    R: io::Read,
{
    #[inline]
    fn next(&mut self) -> Option<Result<u8>> {
        match self.peeked_byte.take() {
            Some(b) => {
                self.byte_offset += 1;
                Some(Ok(b))
            }
            None => match self.iter.next() {
                Some(Ok(b)) => {
                    self.byte_offset += 1;
                    Some(Ok(b))
                }
                Some(Err(err)) => Some(Err(Error::new(ErrorKind::Io(err), self.byte_offset()))),
                None => None,
            },
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<Result<u8>> {
        match self.peeked_byte {
            Some(b) => Some(Ok(b)),
            None => match self.iter.next() {
                Some(Ok(b)) => {
                    self.peeked_byte = Some(b);
                    Some(Ok(b))
                }
                Some(Err(err)) => Some(Err(Error::new(ErrorKind::Io(err), self.byte_offset()))),
                None => None,
            },
        }
    }

    #[inline]
    fn byte_offset(&self) -> usize {
        self.byte_offset
    }
}

/// A wrapper to implement this crate's [Read] trait for byte slices.
#[derive(Debug)]
#[expect(clippy::module_name_repetitions)]
pub struct SliceRead<'a> {
    slice: &'a [u8],
    byte_offset: usize,
}

impl<'a> SliceRead<'a> {
    /// Instantiates a new reader.
    #[must_use]
    pub fn new(slice: &'a [u8]) -> Self {
        SliceRead {
            slice,
            byte_offset: 0,
        }
    }
}

impl<'a> Read<'a> for SliceRead<'a> {
    #[inline]
    fn next(&mut self) -> Option<Result<u8>> {
        if self.byte_offset < self.slice.len() {
            let b = self.slice[self.byte_offset];
            self.byte_offset += 1;
            Some(Ok(b))
        } else {
            None
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<Result<u8>> {
        if self.byte_offset < self.slice.len() {
            Some(Ok(self.slice[self.byte_offset]))
        } else {
            None
        }
    }

    #[inline]
    fn byte_offset(&self) -> usize {
        self.byte_offset
    }
}
