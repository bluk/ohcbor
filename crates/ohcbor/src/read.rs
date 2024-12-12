//! [Read] trait and helpers to read bytes for the decoder.

use crate::{
    buf::Buffer,
    error::{Error, ErrorKind, Result},
};
use core::ops::Deref;

#[cfg(feature = "std")]
use std::io;

/// A reference to borrowed data.
///
/// The variant determines if the slice comes from a long lived source (e.g. an
/// existing byte array) or if it comes from a temporary buffer.
///
/// In the decoder code, the different variants determine which visitor method
/// to call (e.g. `visit_borrowed_str` vs. `visit_str`).  Each variant has a
/// different lifetime which is what the compiler uses to ensure the data will
/// live long enough.
#[derive(Debug)]
pub enum Ref<'a, 'b, B> {
    /// Reference from the original source of data.
    Source(&'a [u8]),
    /// Reference from the given data buffer.
    Buffer(&'b B),
}

impl<B> Deref for Ref<'_, '_, B>
where
    B: Buffer,
{
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match *self {
            Ref::Source(s) => s,
            Ref::Buffer(b) => b.as_slice(),
        }
    }
}

/// Trait used by the [`decode::Decoder`][crate::decode::Decoder] to read bytes.
pub trait Read<'a> {
    /// Consumes and returns the next read byte.
    fn next(&mut self) -> Option<Result<u8>>;

    /// Returns the next byte but does not consume.
    ///
    /// Repeated peeks (with no [`next()`][Read::next] call) should return the same byte.
    fn peek(&mut self) -> Option<Result<u8>>;

    /// Returns the position in the stream of bytes.
    fn byte_offset(&self) -> usize;

    /// Reads an exact number of bytes by either putting them into the buffer or
    /// by returning a borrowed byte slice.
    ///
    /// If all of the data is already read and available to borrowed against,
    /// the returned result could be a reference to the original underlying
    /// data.
    ///
    /// If the data is not already available and needs to be buffered, the data
    /// could be added to the given buffer parameter and a borrowed slice from
    /// the buffer could be returned.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn read_exact<'b, B>(&mut self, len: usize, buf: &'b mut B) -> Result<Ref<'a, 'b, B>>
    where
        B: Buffer;

    /// Parses the argument as a length.
    ///
    /// Useful for byte strings, text strings, arrays, and maps.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn parse_len(&mut self, init_byte: u8) -> Result<Option<usize>>;
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
impl<'a, R> Read<'a> for IoRead<R>
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

    fn read_exact<'b, B>(&mut self, len: usize, buf: &'b mut B) -> Result<Ref<'a, 'b, B>>
    where
        B: Buffer,
    {
        debug_assert!(buf.is_empty());

        buf.reserve(len);

        for _ in 0..len {
            buf.push(self.next().ok_or_else(|| {
                Error::new(ErrorKind::EofWhileParsingValue, self.byte_offset())
            })??);
        }

        Ok(Ref::Buffer(buf))
    }

    fn parse_len(&mut self, init_byte: u8) -> Result<Option<usize>> {
        macro_rules! parse_next {
            () => {
                self.next().ok_or_else(|| {
                    Error::new(ErrorKind::EofWhileParsingValue, self.byte_offset())
                })??
            };
        }

        let arg_val = init_byte & 0b0001_1111;
        let len: usize = match arg_val {
            0..24 => usize::from(arg_val),
            24 => {
                let val = parse_next!();
                usize::from(val)
            }
            25 => {
                let val = u16::from_be_bytes([parse_next!(), parse_next!()]);
                usize::from(val)
            }
            26 => {
                let val = u32::from_be_bytes([
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                ]);
                usize::try_from(val)
                    .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
            }
            27 => {
                let val = u64::from_be_bytes([
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                    parse_next!(),
                ]);
                usize::try_from(val)
                    .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
            }
            28..=30 => {
                return Err(Error::new(ErrorKind::NotWellFormed, self.byte_offset() - 1));
            }
            31 => {
                // Indefinite length
                return Ok(None);
            }
            _ => {
                unreachable!()
            }
        };

        Ok(Some(len))
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

    fn read_exact<'b, B>(&mut self, len: usize, _buf: &'b mut B) -> Result<Ref<'a, 'b, B>>
    where
        B: Buffer,
    {
        let start_idx = self.byte_offset;
        self.byte_offset += len;

        let slice_len = self.slice.len();
        if slice_len < self.byte_offset {
            self.byte_offset = slice_len;
            return Err(Error::new(
                ErrorKind::EofWhileParsingValue,
                self.byte_offset(),
            ));
        }

        Ok(Ref::Source(&self.slice[start_idx..self.byte_offset]))
    }

    fn parse_len(&mut self, init_byte: u8) -> Result<Option<usize>> {
        let arg_val = init_byte & 0b0001_1111;
        let len: usize = match arg_val {
            0..24 => usize::from(arg_val),
            24 => {
                let val = self.next().ok_or_else(|| {
                    Error::new(ErrorKind::EofWhileParsingValue, self.byte_offset())
                })??;
                usize::from(val)
            }
            25 => {
                if self.byte_offset + 1 < self.slice.len() {
                    let val = u16::from_be_bytes([
                        self.slice[self.byte_offset],
                        self.slice[self.byte_offset + 1],
                    ]);
                    self.byte_offset += 2;
                    usize::from(val)
                } else {
                    self.byte_offset = self.slice.len();
                    return Err(Error::new(
                        ErrorKind::EofWhileParsingValue,
                        self.byte_offset(),
                    ));
                }
            }
            26 => {
                if self.byte_offset + 3 < self.slice.len() {
                    let val = u32::from_be_bytes([
                        self.slice[self.byte_offset],
                        self.slice[self.byte_offset + 1],
                        self.slice[self.byte_offset + 2],
                        self.slice[self.byte_offset + 3],
                    ]);
                    self.byte_offset += 4;

                    usize::try_from(val)
                        .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
                } else {
                    self.byte_offset = self.slice.len();
                    return Err(Error::new(
                        ErrorKind::EofWhileParsingValue,
                        self.byte_offset(),
                    ));
                }
            }
            27 => {
                if self.byte_offset + 7 < self.slice.len() {
                    let val = u64::from_be_bytes([
                        self.slice[self.byte_offset],
                        self.slice[self.byte_offset + 1],
                        self.slice[self.byte_offset + 2],
                        self.slice[self.byte_offset + 3],
                        self.slice[self.byte_offset + 4],
                        self.slice[self.byte_offset + 5],
                        self.slice[self.byte_offset + 6],
                        self.slice[self.byte_offset + 7],
                    ]);
                    self.byte_offset += 8;

                    usize::try_from(val)
                        .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
                } else {
                    self.byte_offset = self.slice.len();
                    return Err(Error::new(
                        ErrorKind::EofWhileParsingValue,
                        self.byte_offset(),
                    ));
                }
            }
            28..=30 => {
                return Err(Error::new(ErrorKind::NotWellFormed, self.byte_offset() - 1));
            }
            31 => {
                // Indefinite length
                return Ok(None);
            }
            _ => {
                unreachable!()
            }
        };

        Ok(Some(len))
    }
}
