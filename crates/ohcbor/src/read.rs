//! [Read] trait and helpers to read bytes for the decoder.

use crate::{
    buf::Buffer,
    error::{Error, ErrorKind, Result},
    ADDTL_INFO_MASK,
};
use core::{array, ops::Deref};

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

    /// Reads an exact number of bytes and returns the result in a fixed sized array.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn read_arr<const SIZE: usize>(&mut self) -> Result<[u8; SIZE]>;

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

    /// Parses the next 2 bytes as a `u16`.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn parse_u16(&mut self) -> Result<u16> {
        Ok(u16::from_be_bytes(self.read_arr()?))
    }

    /// Parses the next 4 bytes as a `u32`.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn parse_u32(&mut self) -> Result<u32> {
        Ok(u32::from_be_bytes(self.read_arr()?))
    }

    /// Parses the next 8 bytes as a `u64`.
    ///
    /// # Errors
    ///
    /// Errors include:
    ///
    /// - malformatted input
    /// - end of file
    fn parse_u64(&mut self) -> Result<u64> {
        Ok(u64::from_be_bytes(self.read_arr()?))
    }
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

    fn parse_next(&mut self) -> Result<u8> {
        self.next()
            .ok_or_else(|| Error::new(ErrorKind::EofWhileParsingValue, self.byte_offset()))?
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

    fn read_arr<const SIZE: usize>(&mut self) -> Result<[u8; SIZE]> {
        let mut arr = [0u8; SIZE];
        for v in &mut arr {
            *v = self.parse_next()?;
        }
        Ok(arr)
    }

    fn parse_len(&mut self, init_byte: u8) -> Result<Option<usize>> {
        let addtl_info = init_byte & ADDTL_INFO_MASK;
        let len: usize = match addtl_info {
            0..24 => usize::from(addtl_info),
            24 => {
                let val = self.parse_next()?;
                usize::from(val)
            }
            25 => {
                let val = self.parse_u16()?;
                usize::from(val)
            }
            26 => {
                let val = self.parse_u32()?;
                usize::try_from(val)
                    .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
            }
            27 => {
                let val = self.parse_u64()?;
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

    fn read_arr<const SIZE: usize>(&mut self) -> Result<[u8; SIZE]> {
        if self.slice.len() < self.byte_offset + SIZE {
            self.byte_offset = self.slice.len();
            return Err(Error::new(
                ErrorKind::EofWhileParsingValue,
                self.byte_offset(),
            ));
        }

        let val: [u8; SIZE] = array::from_fn(|offset| self.slice[self.byte_offset + offset]);
        self.byte_offset += SIZE;
        Ok(val)
    }

    fn parse_len(&mut self, init_byte: u8) -> Result<Option<usize>> {
        let addtl_info = init_byte & ADDTL_INFO_MASK;
        let len: usize = match addtl_info {
            0..24 => usize::from(addtl_info),
            24 => {
                let val = self.next().ok_or_else(|| {
                    Error::new(ErrorKind::EofWhileParsingValue, self.byte_offset())
                })??;
                usize::from(val)
            }
            25 => {
                let val = self.parse_u16()?;
                usize::from(val)
            }
            26 => {
                let val = self.parse_u32()?;
                usize::try_from(val)
                    .map_err(|_| Error::new(ErrorKind::InvalidLen, self.byte_offset()))?
            }
            27 => {
                let val = self.parse_u64()?;
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
