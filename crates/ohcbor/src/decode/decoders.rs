use crate::{
    buf::Buffer,
    decode::{ArrAccess, DecodeSeed, Decoder, Error, MapAccess, Visitor},
    error::ErrorKind,
    read::{Read, Ref},
    IB_ARRAY_MIN, IB_BOOL_FALSE, IB_BOOL_TRUE, IB_BYTE_STR_MIN, IB_MAP_MIN, IB_NULL, IB_SINT_MIN,
    IB_TAG_MIN, IB_TEXT_STR_MIN, IB_UINT_MIN,
};

pub(crate) struct DecoderImpl<R, B> {
    read: R,
    /// Temporary buffer used to reduce allocations made
    buf: B,
}

impl<R, B> DecoderImpl<R, B> {
    /// Constructs a decoder from a readable source.
    pub(crate) fn new(read: R, buf: B) -> Self {
        DecoderImpl { read, buf }
    }
}

impl<'de, R, B> DecoderImpl<R, B>
where
    R: Read<'de>,
{
    /// Should be called after a value from the source is decoded to
    /// validate that the entire source was read.
    ///
    /// If trailing data is expected, do not call this method.
    ///
    /// # Errors
    ///
    /// An error is returned if there are unconsumed bytes in the readable
    /// source.
    pub(crate) fn end(&mut self) -> Result<(), crate::Error> {
        match self.read.peek() {
            Some(r) => r.and(Err(crate::Error::new(
                ErrorKind::TrailingData,
                self.read.byte_offset(),
            ))),
            None => Ok(()),
        }
    }

    #[inline]
    fn parse_peek(&mut self) -> Result<u8, crate::Error> {
        self.read.peek().ok_or_else(|| {
            crate::Error::new(ErrorKind::EofWhileParsingValue, self.read.byte_offset())
        })?
    }

    #[inline]
    fn parse_next(&mut self) -> Result<u8, crate::Error> {
        self.read.next().ok_or_else(|| {
            crate::Error::new(ErrorKind::EofWhileParsingValue, self.read.byte_offset())
        })?
    }
}

impl<'de, R, B> Decoder<'de> for &mut DecoderImpl<R, B>
where
    R: Read<'de>,
    B: Buffer,
{
    type Error = crate::Error;

    /// Decode a single data item.
    ///
    /// A data item can contain nested data items (e.g. an array or a map).
    #[expect(clippy::too_many_lines)]
    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let init_byte = self.parse_next()?;

        match init_byte {
            IB_UINT_MIN..IB_SINT_MIN => {
                let arg_val = init_byte & 0b0001_1111;
                match arg_val {
                    0..24 => visitor.visit_u8(arg_val),
                    24 => {
                        let val = self.parse_next()?;
                        visitor.visit_u8(val)
                    }
                    25 => {
                        let val = u16::from_be_bytes([self.parse_next()?, self.parse_next()?]);
                        visitor.visit_u16(val)
                    }
                    26 => {
                        let val = u32::from_be_bytes([
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                        ]);
                        visitor.visit_u32(val)
                    }
                    27 => {
                        let val = u64::from_be_bytes([
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                        ]);
                        visitor.visit_u64(val)
                    }
                    28..=31 => Err(Error::malformed(self.read.byte_offset())),
                    _ => {
                        unreachable!()
                    }
                }
            }
            IB_SINT_MIN..IB_BYTE_STR_MIN => {
                let arg_val = init_byte & 0b0001_1111;
                match arg_val {
                    0..24 => {
                        let arg_val =
                            i8::try_from(arg_val).expect("argument should be less than 24");
                        visitor.visit_i8(-1 - arg_val)
                    }
                    24 => {
                        let val = self.parse_next()?;

                        if let Some(val) = i8::try_from(val)
                            .ok()
                            .and_then(|val| (-1i8).checked_sub(val))
                        {
                            visitor.visit_i8(val)
                        } else {
                            visitor.visit_i16(-1 - i16::from(val))
                        }
                    }
                    25 => {
                        let val = u16::from_be_bytes([self.parse_next()?, self.parse_next()?]);

                        if let Some(val) = i16::try_from(val)
                            .ok()
                            .and_then(|val| (-1i16).checked_sub(val))
                        {
                            visitor.visit_i16(val)
                        } else {
                            visitor.visit_i32(-1 - i32::from(val))
                        }
                    }
                    26 => {
                        let val = u32::from_be_bytes([
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                        ]);

                        if let Some(val) = i32::try_from(val)
                            .ok()
                            .and_then(|val| (-1i32).checked_sub(val))
                        {
                            visitor.visit_i32(val)
                        } else {
                            visitor.visit_i64(-1 - i64::from(val))
                        }
                    }
                    27 => {
                        let val = u64::from_be_bytes([
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                            self.parse_next()?,
                        ]);

                        if let Some(val) = i64::try_from(val)
                            .ok()
                            .and_then(|val| (-1i64).checked_sub(val))
                        {
                            visitor.visit_i64(val)
                        } else {
                            visitor.visit_i128(-1 - i128::from(val))
                        }
                    }
                    28..=31 => Err(Error::malformed(self.read.byte_offset())),
                    _ => {
                        unreachable!()
                    }
                }
            }
            IB_BYTE_STR_MIN..IB_TEXT_STR_MIN => {
                let len = self.read.parse_len(init_byte)?.unwrap();

                self.buf.clear();
                match self.read.read_exact(len, &mut self.buf)? {
                    Ref::Source(bytes) => visitor.visit_borrowed_bytes(bytes),
                    Ref::Buffer(bytes) => visitor.visit_bytes(bytes.as_slice()),
                }
            }
            IB_TEXT_STR_MIN..IB_ARRAY_MIN => {
                let len = self.read.parse_len(init_byte)?.unwrap();

                self.buf.clear();
                match self.read.read_exact(len, &mut self.buf)? {
                    Ref::Source(bytes) => match core::str::from_utf8(bytes) {
                        Ok(s) => visitor.visit_borrowed_str(s),
                        Err(e) => Err(crate::Error::new(
                            ErrorKind::InvalidUtf8Error(e),
                            self.read.byte_offset(),
                        )),
                    },
                    Ref::Buffer(bytes) => match core::str::from_utf8(bytes.as_slice()) {
                        Ok(s) => visitor.visit_str(s),
                        Err(e) => Err(crate::Error::new(
                            ErrorKind::InvalidUtf8Error(e),
                            self.read.byte_offset(),
                        )),
                    },
                }
            }
            IB_ARRAY_MIN..IB_MAP_MIN => {
                let len = self.read.parse_len(init_byte)?;

                visitor.visit_arr(ArrAccessImpl {
                    de: self,
                    len,
                    count: 0,
                })
            }
            IB_MAP_MIN..IB_TAG_MIN => {
                let len = self.read.parse_len(init_byte)?;

                visitor.visit_map(MapAccessImpl {
                    de: self,
                    len,
                    count: 0,
                })
            }
            IB_BOOL_FALSE => visitor.visit_bool(false),
            IB_BOOL_TRUE => visitor.visit_bool(true),
            IB_NULL => visitor.visit_null(),
            _ => todo!(),
        }
    }

    fn decode_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        match init_byte {
            IB_NULL => {
                self.parse_next()?;
                visitor.visit_null()
            }
            _ => visitor.visit_some(self),
        }
    }
}

struct ArrAccessImpl<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
    /// Expected number of items. If `None`, the list has an indefinite length
    len: Option<usize>,
    /// Number of elements already decoded
    count: usize,
}

impl<'de, 'a, R: Read<'de> + 'a, B> ArrAccess<'de> for ArrAccessImpl<'a, R, B>
where
    B: Buffer,
{
    type Error = crate::Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        if let Some(len) = self.len {
            if len == self.count {
                return Ok(None);
            }
            self.count += 1;
        } else {
            // Indefinite length
            todo!()
        }

        Ok(Some(seed.decode(&mut *self.de)?))
    }

    /// Returns the number of elements remaining in the sequence, if known.
    #[inline]
    fn size_hint(&self) -> Option<usize> {
        self.len
    }
}

struct MapAccessImpl<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
    /// Expected number of pair of items. If `None`, the list has an indefinite length
    len: Option<usize>,
    /// Number of elements already decoded
    count: usize,
}

impl<'de, 'a, R: Read<'de> + 'a, B> MapAccess<'de> for MapAccessImpl<'a, R, B>
where
    B: Buffer,
{
    type Error = crate::Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DecodeSeed<'de>,
    {
        if let Some(len) = self.len {
            if len == self.count {
                return Ok(None);
            }
            self.count += 1;

            seed.decode(MapKey { de: &mut *self.de }).map(Some)
        } else {
            // Indefinite length
            todo!()
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DecodeSeed<'de>,
    {
        seed.decode(&mut *self.de)
    }

    fn size_hint(&self) -> Option<usize> {
        self.len
    }
}

struct MapKey<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
}

impl<'de, R, B> Decoder<'de> for MapKey<'_, R, B>
where
    R: Read<'de>,
    B: Buffer,
{
    type Error = crate::Error;

    #[inline]
    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.decode_any(visitor)
    }

    fn decode_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.de.decode_option(visitor)
    }
}
