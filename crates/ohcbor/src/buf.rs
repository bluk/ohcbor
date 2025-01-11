//! Buffer for storing data.

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

/// Buffer for storing data.
///
/// The trait is an abstraction to allow different types of buffers to exist. In
/// some cases, a `Vec<u8>` could be used to allow a dynamically sized buffer,
/// but in other cases, a fixed size buffer may be used.
pub trait Buffer {
    /// Error type for the buffer
    type Error;

    /// Clears the existing contents of the buffer.
    fn clear(&mut self);

    /// Returns true if the buffer is empty, false otherwise.
    fn is_empty(&self) -> bool;

    /// Reserves additional capacity.
    fn reserve(&mut self, additional: usize);

    /// Appends a value to the buffer.
    ///
    /// # Errors
    ///
    /// Returns an error if data cannot be pushed to the buffer.
    fn push(&mut self, value: u8) -> Result<(), Self::Error>;

    /// Returns the buffer as a slice.
    fn as_slice(&self) -> &[u8];
}

impl<B> Buffer for &mut B
where
    B: Buffer,
{
    type Error = B::Error;

    #[inline]
    fn clear(&mut self) {
        (**self).clear();
    }

    #[inline]
    fn is_empty(&self) -> bool {
        (**self).is_empty()
    }

    #[inline]
    fn reserve(&mut self, additional: usize) {
        (**self).reserve(additional);
    }

    #[inline]
    fn push(&mut self, value: u8) -> Result<(), B::Error> {
        (**self).push(value)
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        (**self).as_slice()
    }
}

#[cfg(any(feature = "alloc", feature = "std"))]
impl Buffer for Vec<u8> {
    type Error = core::convert::Infallible;

    #[inline]
    fn clear(&mut self) {
        Vec::clear(self);
    }

    #[inline]
    fn is_empty(&self) -> bool {
        Vec::is_empty(self)
    }

    #[inline]
    fn reserve(&mut self, additional: usize) {
        Vec::reserve(self, additional);
    }

    #[inline]
    fn push(&mut self, value: u8) -> Result<(), Self::Error> {
        Vec::push(self, value);
        Ok(())
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        Vec::as_slice(self)
    }
}

/// Error when the slice buffer has run out of space.
#[derive(Debug, Clone, Copy)]
pub struct SliceBufError;

/// Buffer represented by a fixed sized slice.
#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub struct SliceBuf<'a> {
    reserved: &'a mut [u8],
    len: usize,
}

impl Buffer for SliceBuf<'_> {
    type Error = SliceBufError;

    #[inline]
    fn clear(&mut self) {
        self.len = 0;
    }

    #[inline]
    fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    fn reserve(&mut self, _additional: usize) {
        // Do nothing
    }

    #[inline]
    fn push(&mut self, value: u8) -> Result<(), Self::Error> {
        if self.len == self.reserved.len() {
            return Err(SliceBufError);
        }

        self.reserved[self.len] = value;
        self.len += 1;
        Ok(())
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        &self.reserved[..self.len]
    }
}
