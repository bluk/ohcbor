//! Buffer for storing data.

/// Buffer for storing data.
///
/// The trait is an abstraction to allow different types of buffers to exist. In
/// some cases, a `Vec<u8>` could be used to allow a dynamically sized buffer,
/// but in other cases, a fixed size buffer may be used.
pub trait Buffer {
    /// Clears the existing contents of the buffer.
    fn clear(&mut self);

    /// Returns true if the buffer is empty, false otherwise.
    fn is_empty(&self) -> bool;

    /// Reserves additional capacity.
    fn reserve(&mut self, additional: usize);

    /// Appends a value to the buffer.
    fn push(&mut self, value: u8);

    /// Returns the buffer as a slice.
    fn as_slice(&self) -> &[u8];
}

impl<B> Buffer for &mut B
where
    B: Buffer,
{
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
    fn push(&mut self, value: u8) {
        (**self).push(value);
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        (**self).as_slice()
    }
}

impl Buffer for Vec<u8> {
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
    fn push(&mut self, value: u8) {
        Vec::push(self, value);
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        Vec::as_slice(self)
    }
}

struct SliceBuf<'a> {
    reserved: &'a mut [u8],
    len: usize,
}

impl Buffer for SliceBuf<'_> {
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
    fn push(&mut self, value: u8) {
        if self.len == self.reserved.len() {
            todo!()
        }

        self.reserved[self.len] = value;
        self.len += 1;
    }

    #[inline]
    fn as_slice(&self) -> &[u8] {
        &self.reserved[..self.len]
    }
}
