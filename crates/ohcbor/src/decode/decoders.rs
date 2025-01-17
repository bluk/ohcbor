use crate::{
    buf::Buffer,
    decode::{
        ArrAccess, DecodeSeed, Decoder, Error, IndefiniteLenItemAccess, MapAccess, Unexpected,
        Visitor,
    },
    error::ErrorKind,
    read::{Read, Ref},
    Simple, ADDTL_INFO_MASK, BREAK_CODE, IB_FP_SIMPLE_MASK, IB_MASK_ARRAY, IB_MASK_BYTE_STR,
    IB_MASK_MAP, IB_MASK_NEG_INT, IB_MASK_TAG, IB_MASK_TEXT_STR, IB_MASK_UINT,
};

const IB_UINT_ONE_BYTE: u8 = IB_MASK_UINT | 24;
const IB_UINT_TWO_BYTES: u8 = IB_MASK_UINT | 25;
const IB_UINT_FOUR_BYTES: u8 = IB_MASK_UINT | 26;
const IB_UINT_EIGHT_BYTES: u8 = IB_MASK_UINT | 27;
const IB_UINT_RESERVED_MIN: u8 = IB_MASK_UINT | 28;
const IB_UINT_RESERVED_MAX: u8 = IB_MASK_UINT | 30;
const IB_UINT_NO_ARG: u8 = IB_MASK_UINT | 31;

const IB_NEG_INT_ONE_BYTE: u8 = IB_MASK_NEG_INT | 24;
const IB_NEG_INT_TWO_BYTES: u8 = IB_MASK_NEG_INT | 25;
const IB_NEG_INT_FOUR_BYTES: u8 = IB_MASK_NEG_INT | 26;
const IB_NEG_INT_EIGHT_BYTES: u8 = IB_MASK_NEG_INT | 27;
const IB_NEG_INT_RESERVED_MIN: u8 = IB_MASK_NEG_INT | 28;
const IB_NEG_INT_RESERVED_MAX: u8 = IB_MASK_NEG_INT | 30;
const IB_NEG_INT_NO_ARG: u8 = IB_MASK_NEG_INT | 31;

const IB_BYTE_STR_ONE_BYTE: u8 = IB_MASK_BYTE_STR | 24;
const IB_BYTE_STR_TWO_BYTES: u8 = IB_MASK_BYTE_STR | 25;
const IB_BYTE_STR_FOUR_BYTES: u8 = IB_MASK_BYTE_STR | 26;
const IB_BYTE_STR_EIGHT_BYTES: u8 = IB_MASK_BYTE_STR | 27;
const IB_BYTE_STR_RESERVED_MIN: u8 = IB_MASK_BYTE_STR | 28;
const IB_BYTE_STR_RESERVED_MAX: u8 = IB_MASK_BYTE_STR | 30;
const IB_BYTE_STR_NO_ARG: u8 = IB_MASK_BYTE_STR | 31;

const IB_TEXT_STR_ONE_BYTE: u8 = IB_MASK_TEXT_STR | 24;
const IB_TEXT_STR_TWO_BYTES: u8 = IB_MASK_TEXT_STR | 25;
const IB_TEXT_STR_FOUR_BYTES: u8 = IB_MASK_TEXT_STR | 26;
const IB_TEXT_STR_EIGHT_BYTES: u8 = IB_MASK_TEXT_STR | 27;
const IB_TEXT_STR_RESERVED_MIN: u8 = IB_MASK_TEXT_STR | 28;
const IB_TEXT_STR_RESERVED_MAX: u8 = IB_MASK_TEXT_STR | 30;
const IB_TEXT_STR_NO_ARG: u8 = IB_MASK_TEXT_STR | 31;

const IB_ARRAY_ONE_BYTE: u8 = IB_MASK_ARRAY | 24;
const IB_ARRAY_TWO_BYTES: u8 = IB_MASK_ARRAY | 25;
const IB_ARRAY_FOUR_BYTES: u8 = IB_MASK_ARRAY | 26;
const IB_ARRAY_EIGHT_BYTES: u8 = IB_MASK_ARRAY | 27;
const IB_ARRAY_RESERVED_MIN: u8 = IB_MASK_ARRAY | 28;
const IB_ARRAY_RESERVED_MAX: u8 = IB_MASK_ARRAY | 30;
const IB_ARRAY_NO_ARG: u8 = IB_MASK_ARRAY | 31;

const IB_MAP_ONE_BYTE: u8 = IB_MASK_MAP | 24;
const IB_MAP_TWO_BYTES: u8 = IB_MASK_MAP | 25;
const IB_MAP_FOUR_BYTES: u8 = IB_MASK_MAP | 26;
const IB_MAP_EIGHT_BYTES: u8 = IB_MASK_MAP | 27;
const IB_MAP_RESERVED_MIN: u8 = IB_MASK_MAP | 28;
const IB_MAP_RESERVED_MAX: u8 = IB_MASK_MAP | 30;
const IB_MAP_NO_ARG: u8 = IB_MASK_MAP | 31;

const IB_TAG_ONE_BYTE: u8 = IB_MASK_TAG | 24;
const IB_TAG_TWO_BYTES: u8 = IB_MASK_TAG | 25;
const IB_TAG_FOUR_BYTES: u8 = IB_MASK_TAG | 26;
const IB_TAG_EIGHT_BYTES: u8 = IB_MASK_TAG | 27;
const IB_TAG_RESERVED_MIN: u8 = IB_MASK_TAG | 28;
const IB_TAG_RESERVED_MAX: u8 = IB_MASK_TAG | 30;
const IB_TAG_NO_ARG: u8 = IB_MASK_TAG | 31;

const IB_SIMPLE_ONE_BYTE: u8 = IB_FP_SIMPLE_MASK | 24;
const IB_FP16: u8 = IB_FP_SIMPLE_MASK | 25;
const IB_FP32: u8 = IB_FP_SIMPLE_MASK | 26;
const IB_FP64: u8 = IB_FP_SIMPLE_MASK | 27;
const IB_FP_SIMPLE_RESERVED_MIN: u8 = IB_FP_SIMPLE_MASK | 28;
const IB_FP_SIMPLE_RESERVED_MAX: u8 = IB_FP_SIMPLE_MASK | 30;

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
            Some(r) => r.and(Err(crate::Error::new(ErrorKind::TrailingData))),
            None => Ok(()),
        }
    }

    fn on_end_remaining(&mut self, remaining: Option<usize>) -> Result<(), crate::Error> {
        if let Some(remaining) = remaining {
            if 0 < remaining {
                Err(crate::Error::new(ErrorKind::TrailingData))
            } else {
                Ok(())
            }
        } else {
            match self.parse_peek()? {
                BREAK_CODE => {
                    self.parse_next()?;
                    Ok(())
                }
                _ => Err(crate::Error::new(ErrorKind::TrailingData)),
            }
        }
    }

    #[inline]
    fn parse_peek(&mut self) -> Result<u8, crate::Error> {
        self.read
            .peek()
            .ok_or_else(|| crate::Error::new(ErrorKind::EofWhileParsingValue))?
    }

    #[inline]
    fn parse_next(&mut self) -> Result<u8, crate::Error> {
        self.read
            .next()
            .ok_or_else(|| crate::Error::new(ErrorKind::EofWhileParsingValue))?
    }
}

impl<'de, R, B> DecoderImpl<R, B>
where
    R: Read<'de>,
    B: Buffer,
{
    fn parse_byte_str<V>(&mut self, visitor: V, len: usize) -> Result<V::Value, crate::Error>
    where
        V: Visitor<'de>,
    {
        self.buf.clear();
        match self.read.read_exact(len, &mut self.buf)? {
            Ref::Source(bytes) => visitor.visit_borrowed_bytes(bytes),
            Ref::Buffer(bytes) => visitor.visit_bytes(bytes.as_slice()),
        }
    }

    fn parse_text_str<V>(&mut self, visitor: V, len: usize) -> Result<V::Value, crate::Error>
    where
        V: Visitor<'de>,
    {
        self.buf.clear();
        match self.read.read_exact(len, &mut self.buf)? {
            Ref::Source(bytes) => match core::str::from_utf8(bytes) {
                Ok(s) => visitor.visit_borrowed_str(s),
                Err(e) => Err(crate::Error::new(ErrorKind::InvalidUtf8Error(e))),
            },
            Ref::Buffer(bytes) => match core::str::from_utf8(bytes.as_slice()) {
                Ok(s) => visitor.visit_str(s),
                Err(e) => Err(crate::Error::new(ErrorKind::InvalidUtf8Error(e))),
            },
        }
    }

    fn parse_array<V>(
        &mut self,
        visitor: V,
        mut remaining: Option<usize>,
    ) -> Result<V::Value, crate::Error>
    where
        V: Visitor<'de>,
    {
        let ret = visitor.visit_arr(ArrAccessImpl {
            de: self,
            remaining: &mut remaining,
        });

        match (ret, self.on_end_remaining(remaining)) {
            (Ok(ret), Ok(())) => Ok(ret),
            (Err(err), _) | (_, Err(err)) => Err(err),
        }
    }

    fn parse_map<V>(
        &mut self,
        visitor: V,
        mut remaining: Option<usize>,
    ) -> Result<V::Value, crate::Error>
    where
        V: Visitor<'de>,
    {
        let ret = visitor.visit_map(MapAccessImpl {
            de: self,
            remaining: &mut remaining,
        });

        match (ret, self.on_end_remaining(remaining)) {
            (Ok(ret), Ok(())) => Ok(ret),
            (Err(err), _) | (_, Err(err)) => Err(err),
        }
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
            IB_MASK_UINT..IB_UINT_ONE_BYTE => visitor.visit_u8(init_byte & ADDTL_INFO_MASK),
            IB_UINT_ONE_BYTE => {
                let val = self.parse_next()?;
                visitor.visit_u8(val)
            }
            IB_UINT_TWO_BYTES => {
                let val = self.read.parse_u16()?;
                visitor.visit_u16(val)
            }
            IB_UINT_FOUR_BYTES => {
                let val = self.read.parse_u32()?;
                visitor.visit_u32(val)
            }
            IB_UINT_EIGHT_BYTES => {
                let val = self.read.parse_u64()?;
                visitor.visit_u64(val)
            }
            IB_UINT_RESERVED_MIN..=IB_UINT_RESERVED_MAX | IB_UINT_NO_ARG => Err(Error::malformed()),
            IB_MASK_NEG_INT..IB_NEG_INT_ONE_BYTE => {
                let arg_val = i8::try_from(init_byte & ADDTL_INFO_MASK)
                    .expect("argument should be less than 24");
                visitor.visit_i8(-1 - arg_val)
            }
            IB_NEG_INT_ONE_BYTE => {
                let val = self.parse_next()?;
                visitor.visit_i16(-1 - i16::from(val))
            }
            IB_NEG_INT_TWO_BYTES => {
                let val = self.read.parse_u16()?;
                visitor.visit_i32(-1 - i32::from(val))
            }
            IB_NEG_INT_FOUR_BYTES => {
                let val = self.read.parse_u32()?;
                visitor.visit_i64(-1 - i64::from(val))
            }
            IB_NEG_INT_EIGHT_BYTES => {
                let val = self.read.parse_u64()?;

                if let Some(val) = i64::try_from(val)
                    .ok()
                    .and_then(|val| (-1i64).checked_sub(val))
                {
                    visitor.visit_i64(val)
                } else {
                    visitor.visit_i128(-1 - i128::from(val))
                }
            }
            IB_NEG_INT_RESERVED_MIN..=IB_NEG_INT_RESERVED_MAX | IB_NEG_INT_NO_ARG => {
                Err(Error::malformed())
            }
            IB_MASK_BYTE_STR..IB_BYTE_STR_ONE_BYTE => {
                let len = usize::from(init_byte & ADDTL_INFO_MASK);
                self.parse_byte_str(visitor, len)
            }
            IB_BYTE_STR_ONE_BYTE => {
                let len = usize::from(self.parse_next()?);
                self.parse_byte_str(visitor, len)
            }
            IB_BYTE_STR_TWO_BYTES => {
                let len = usize::from(self.read.parse_u16()?);
                self.parse_byte_str(visitor, len)
            }
            IB_BYTE_STR_FOUR_BYTES => {
                let len = self.read.parse_u32()?;
                let len = usize::try_from(len).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_byte_str(visitor, len)
            }
            IB_BYTE_STR_EIGHT_BYTES => {
                let len = self.read.parse_u64()?;
                let len = usize::try_from(len).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_byte_str(visitor, len)
            }
            IB_BYTE_STR_RESERVED_MIN..=IB_BYTE_STR_RESERVED_MAX => Err(Error::malformed()),
            IB_BYTE_STR_NO_ARG => {
                let ret = visitor.visit_indefinite_len_bytes(IndefiniteItemAccessImpl { de: self });

                match (ret, self.on_end_remaining(None)) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            IB_MASK_TEXT_STR..IB_TEXT_STR_ONE_BYTE => {
                let len = usize::from(init_byte & ADDTL_INFO_MASK);
                self.parse_text_str(visitor, len)
            }
            IB_TEXT_STR_ONE_BYTE => {
                let len = usize::from(self.parse_next()?);
                self.parse_text_str(visitor, len)
            }
            IB_TEXT_STR_TWO_BYTES => {
                let len = usize::from(self.read.parse_u16()?);
                self.parse_text_str(visitor, len)
            }
            IB_TEXT_STR_FOUR_BYTES => {
                let len = self.read.parse_u32()?;
                let len = usize::try_from(len).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_text_str(visitor, len)
            }
            IB_TEXT_STR_EIGHT_BYTES => {
                let len = self.read.parse_u64()?;
                let len = usize::try_from(len).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_text_str(visitor, len)
            }
            IB_TEXT_STR_RESERVED_MIN..=IB_TEXT_STR_RESERVED_MAX => Err(Error::malformed()),
            IB_TEXT_STR_NO_ARG => {
                let ret = visitor.visit_indefinite_len_str(IndefiniteItemAccessImpl { de: self });

                match (ret, self.on_end_remaining(None)) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            IB_MASK_ARRAY..IB_ARRAY_ONE_BYTE => {
                let remaining = usize::from(init_byte & ADDTL_INFO_MASK);
                self.parse_array(visitor, Some(remaining))
            }
            IB_ARRAY_ONE_BYTE => {
                let remaining = usize::from(self.parse_next()?);
                self.parse_array(visitor, Some(remaining))
            }
            IB_ARRAY_TWO_BYTES => {
                let remaining = usize::from(self.read.parse_u16()?);
                self.parse_array(visitor, Some(remaining))
            }
            IB_ARRAY_FOUR_BYTES => {
                let remaining = self.read.parse_u32()?;
                let remaining = usize::try_from(remaining).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_array(visitor, Some(remaining))
            }
            IB_ARRAY_EIGHT_BYTES => {
                let remaining = self.read.parse_u64()?;
                let remaining = usize::try_from(remaining).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_array(visitor, Some(remaining))
            }
            IB_ARRAY_RESERVED_MIN..=IB_ARRAY_RESERVED_MAX => Err(Error::malformed()),
            IB_ARRAY_NO_ARG => self.parse_array(visitor, None),
            IB_MASK_MAP..IB_MAP_ONE_BYTE => {
                let remaining = usize::from(init_byte & ADDTL_INFO_MASK);
                self.parse_map(visitor, Some(remaining))
            }
            IB_MAP_ONE_BYTE => {
                let remaining = usize::from(self.parse_next()?);
                self.parse_map(visitor, Some(remaining))
            }
            IB_MAP_TWO_BYTES => {
                let remaining = usize::from(self.read.parse_u16()?);
                self.parse_map(visitor, Some(remaining))
            }
            IB_MAP_FOUR_BYTES => {
                let remaining = self.read.parse_u32()?;
                let remaining = usize::try_from(remaining).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_map(visitor, Some(remaining))
            }
            IB_MAP_EIGHT_BYTES => {
                let remaining = self.read.parse_u64()?;
                let remaining = usize::try_from(remaining).map_err(|_| {
                    Self::Error::invalid_value(Unexpected::Int, &"value is larger than usize::MAX")
                })?;
                self.parse_map(visitor, Some(remaining))
            }
            IB_MAP_RESERVED_MIN..=IB_MAP_RESERVED_MAX => Err(Error::malformed()),
            IB_MAP_NO_ARG => self.parse_map(visitor, None),
            IB_MASK_TAG..IB_TAG_ONE_BYTE => {
                let tag_num = u64::from(init_byte & ADDTL_INFO_MASK);
                visitor.visit_tag(tag_num, self)
            }
            IB_TAG_ONE_BYTE => {
                let tag_num = u64::from(self.parse_next()?);
                visitor.visit_tag(tag_num, self)
            }
            IB_TAG_TWO_BYTES => {
                let tag_num = u64::from(self.read.parse_u16()?);
                visitor.visit_tag(tag_num, self)
            }
            IB_TAG_FOUR_BYTES => {
                let tag_num = u64::from(self.read.parse_u32()?);
                visitor.visit_tag(tag_num, self)
            }
            IB_TAG_EIGHT_BYTES => {
                let tag_num = self.read.parse_u64()?;
                visitor.visit_tag(tag_num, self)
            }
            IB_TAG_RESERVED_MIN..=IB_TAG_RESERVED_MAX | IB_TAG_NO_ARG => Err(Error::malformed()),
            IB_FP_SIMPLE_MASK..IB_SIMPLE_ONE_BYTE => {
                let v = Simple::from(init_byte & ADDTL_INFO_MASK);
                if v.is_null() {
                    visitor.visit_none()
                } else {
                    visitor.visit_simple(v)
                }
            }
            IB_SIMPLE_ONE_BYTE => {
                let val = self.parse_next()?;
                if val < 32 {
                    Err(Error::malformed())
                } else {
                    visitor.visit_simple(Simple::from(val))
                }
            }
            IB_FP16 => {
                // Should add a "f16" feature to add "visit_f16" method
                // to Visitor and to implement f16 support for
                // Decode/Encode.
                let val = half::f16::from_be_bytes(self.read.read_arr()?);
                visitor.visit_f32(f32::from(val))
            }
            IB_FP32 => {
                let val = f32::from_be_bytes(self.read.read_arr()?);
                visitor.visit_f32(val)
            }
            IB_FP64 => {
                let val = f64::from_be_bytes(self.read.read_arr()?);
                visitor.visit_f64(val)
            }
            IB_FP_SIMPLE_RESERVED_MIN..=IB_FP_SIMPLE_RESERVED_MAX | BREAK_CODE => {
                Err(Error::malformed())
            }
        }
    }
}

struct IndefiniteItemAccessImpl<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
}

impl<'de, 'a, R: Read<'de> + 'a, B> IndefiniteLenItemAccess<'de>
    for IndefiniteItemAccessImpl<'a, R, B>
where
    B: Buffer,
{
    type Error = crate::Error;

    fn next_chunk_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        if let Ok(BREAK_CODE) = self.de.parse_peek() {
            return Ok(None);
        }

        Ok(Some(seed.decode(&mut *self.de)?))
    }
}

struct ArrAccessImpl<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
    /// Expected number of items remaining. If `None`, the list has an indefinite length
    remaining: &'a mut Option<usize>,
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
        if let Some(remaining) = &mut self.remaining {
            if *remaining == 0 {
                return Ok(None);
            }
            *remaining -= 1;
        } else if let Ok(BREAK_CODE) = self.de.parse_peek() {
            return Ok(None);
        }

        Ok(Some(seed.decode(&mut *self.de)?))
    }

    #[inline]
    fn size_hint(&self) -> Option<usize> {
        *self.remaining
    }
}

struct MapAccessImpl<'a, R, B> {
    de: &'a mut DecoderImpl<R, B>,
    /// Expected number of pair of items remaining. If `None`, the list has an indefinite length
    remaining: &'a mut Option<usize>,
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
        if let Some(remaining) = &mut self.remaining {
            if *remaining == 0 {
                return Ok(None);
            }
            *remaining -= 1;
        } else if let Ok(BREAK_CODE) = self.de.parse_peek() {
            return Ok(None);
        }

        seed.decode(&mut *self.de).map(Some)
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DecodeSeed<'de>,
    {
        seed.decode(&mut *self.de)
    }

    #[inline]
    fn size_hint(&self) -> Option<usize> {
        *self.remaining
    }
}

#[cfg(test)]
mod tests {
    use crate::{from_slice, value::Value, Result, Simple, Tag};

    #[cfg(feature = "std")]
    use crate::from_reader;

    #[cfg(any(feature = "std", feature = "alloc"))]
    use crate::ByteString;

    use hex_literal::hex;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::{
        collections::BTreeMap,
        string::{String, ToString},
        vec,
        vec::Vec,
    };
    #[cfg(feature = "std")]
    use std::{collections::BTreeMap, string::String, vec, vec::Vec};

    macro_rules! assert_decode_u64_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i64>(&input)?,
                "decoding i64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<i128>(&input)?,
                "decoding i128 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u64>(&input)?,
                "decoding u64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u128>(&input)?,
                "decoding u128 {input:X?}"
            );
        };
    }

    macro_rules! assert_decode_u32_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i32>(&input)?,
                "decoding i32 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u32>(&input)?,
                "decoding u32 {input:X?}"
            );
            assert_decode_u64_val!($expected, $input);
        };
    }

    macro_rules! assert_decode_u16_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i16>(&input)?,
                "decoding i16 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u16>(&input)?,
                "decoding u16 {input:X?}"
            );
            assert_decode_u32_val!($expected, $input);
        };
    }

    macro_rules! assert_decode_u8_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i8>(&input)?,
                "decoding i8 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u8>(&input)?,
                "decoding u8 {input:X?}"
            );
            assert_decode_u16_val!($expected, $input);
        };
    }

    macro_rules! assert_decode_i64_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i64>(&input)?,
                "decoding i64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<i128>(&input)?,
                "decoding i128 {input:X?}"
            );
        };
    }

    macro_rules! assert_decode_i32_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i32>(&input)?,
                "decoding i32 {input:X?}"
            );
            assert_decode_i64_val!($expected, $input);
        };
    }

    macro_rules! assert_decode_i16_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i16>(&input)?,
                "decoding i16 {input:X?}"
            );
            assert_decode_i32_val!($expected, $input);
        };
    }

    macro_rules! assert_decode_i8_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i8>(&input)?,
                "decoding i8 {input:X?}"
            );
            assert_decode_i16_val!($expected, $input);
        };
    }

    #[test]
    fn test_decode_0() -> Result<()> {
        let input = hex!("00");
        assert_decode_u8_val!(0, input);
        Ok(())
    }

    #[test]
    fn test_decode_01() -> Result<()> {
        let input = hex!("01");
        assert_decode_u8_val!(1, input);
        Ok(())
    }

    #[test]
    fn test_decode_10() -> Result<()> {
        let input = hex!("0A");
        assert_decode_u8_val!(10, input);
        Ok(())
    }

    #[test]
    fn test_decode_23() -> Result<()> {
        let input = hex!("17");
        assert_decode_u8_val!(23, input);
        Ok(())
    }

    #[test]
    fn test_decode_24() -> Result<()> {
        let input = hex!("18 18");
        assert_decode_u8_val!(24, input);
        Ok(())
    }

    #[test]
    fn test_decode_25() -> Result<()> {
        let input = hex!("18 19");
        assert_decode_u8_val!(25, input);
        Ok(())
    }

    #[test]
    fn test_decode_100() -> Result<()> {
        let input = hex!("18 64");
        assert_decode_u8_val!(100, input);
        Ok(())
    }

    #[test]
    fn test_decode_1_000() -> Result<()> {
        let input = hex!("19 03 e8");
        assert_decode_u16_val!(1_000, input);
        Ok(())
    }

    #[test]
    fn test_decode_1_000_000() -> Result<()> {
        let input = hex!("1a 00 0f 42 40");
        assert_decode_u32_val!(1_000_000, input);
        Ok(())
    }

    #[test]
    fn test_decode_1_000_000_000_000() -> Result<()> {
        let input = hex!("1b 00 00 00 e8 d4 a5 10 00");
        assert_decode_u64_val!(1_000_000_000_000, input);
        Ok(())
    }

    #[test]
    fn test_decode_18_446_744_073_709_551_615() -> Result<()> {
        let input = hex!("1b ff ff ff ff ff ff ff ff");
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<i128>(&input)?,
            "decoding i128 {input:X?}"
        );
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<u64>(&input)?,
            "decoding u64 {input:X?}"
        );
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<u128>(&input)?,
            "decoding u128 {input:X?}"
        );
        Ok(())
    }

    #[test]
    fn test_decode_18_446_744_073_709_551_616() -> Result<()> {
        let input = hex!("c2 49 01 00 00 00 00 00 00 00 00");
        let expected: &[u8] = &[0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(from_slice::<Tag<&[u8]>>(&input)?, Tag::new(2, expected),);
        Ok(())
    }

    #[test]
    fn test_decode_neg_18_446_744_073_709_551_616() -> Result<()> {
        let input = hex!("3b ff ff ff ff ff ff ff ff");
        assert_eq!(
            -18_446_744_073_709_551_616,
            from_slice::<i128>(&input)?,
            "decoding i128 {input:X?}"
        );
        Ok(())
    }

    #[test]
    fn test_decode_neg_18_446_744_073_709_551_617() -> Result<()> {
        let input = hex!("c3 49 01 00 00 00 00 00 00 00 00");
        let expected: &[u8] = &[0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        assert_eq!(from_slice::<Tag<&[u8]>>(&input)?, Tag::new(3, expected),);
        Ok(())
    }

    #[test]
    fn test_decode_neg_1() -> Result<()> {
        let input = hex!("20");
        assert_decode_i8_val!(-1, input);
        Ok(())
    }

    #[test]
    fn test_decode_neg_10() -> Result<()> {
        let input = hex!("29");
        assert_decode_i8_val!(-10, input);
        Ok(())
    }

    #[test]
    fn test_decode_neg_100() -> Result<()> {
        let input = hex!("38 63");
        assert_decode_i8_val!(-100, input);
        Ok(())
    }

    #[test]
    fn test_decode_neg_1000() -> Result<()> {
        let input = hex!("39 03 e7");
        assert_decode_i16_val!(-1000, input);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_zero_fp() -> Result<()> {
        let input = hex!("f9 00 00");
        assert_eq!(0.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_neg_zero_fp() -> Result<()> {
        let input = hex!("f9 80 00");
        assert_eq!(-0.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_one_fp() -> Result<()> {
        let input = hex!("f9 3c 00");
        assert_eq!(1.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_one_dot_one_fp() -> Result<()> {
        let input = hex!("fb 3f f1 99 99 99 99 99 9a");
        assert_eq!(1.1, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_one_dot_five_fp() -> Result<()> {
        let input = hex!("f9 3e 00");
        assert_eq!(1.5, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_65504_fp() -> Result<()> {
        let input = hex!("f9 7b ff");
        assert_eq!(65504.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_100000_fp() -> Result<()> {
        let input = hex!("fa47c35000");
        assert_eq!(100_000.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_long_fp() -> Result<()> {
        let input = hex!("fa7f7fffff");
        assert_eq!(3.402_823_466_385_288_6e38f64, from_slice::<f64>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_big_exp_fp() -> Result<()> {
        let input = hex!("fb7e37e43c8800759c");
        assert_eq!(1.0e300, from_slice::<f64>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_neg_exp_fp() -> Result<()> {
        let input = hex!("f90001");
        assert_eq!(5.960_464_477_539_063e-8, from_slice::<f64>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_pos_decimal_fp() -> Result<()> {
        let input = hex!("f90400");
        assert_eq!(0.000_061_035_156_25, from_slice::<f64>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_neg_fp() -> Result<()> {
        let input = hex!("f9c400");
        assert_eq!(-4.0, from_slice::<f32>(&input)?);
        Ok(())
    }

    #[allow(clippy::float_cmp)]
    #[test]
    fn test_decode_neg_64_fp() -> Result<()> {
        let input = hex!("fbc010666666666666");
        assert_eq!(-4.1, from_slice::<f64>(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_f16_infinity() -> Result<()> {
        let input = hex!("f97c00");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_infinite());
        assert!(actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_f16_nan() -> Result<()> {
        let input = hex!("f97e00");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_nan());
        Ok(())
    }

    #[test]
    fn test_decode_f16_neg_infinity() -> Result<()> {
        let input = hex!("f9fc00");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_infinite());
        assert!(!actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_f32_infinity() -> Result<()> {
        let input = hex!("fa7f800000");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_infinite());
        assert!(actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_f32_nan() -> Result<()> {
        let input = hex!("fa7fc00000");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_nan());
        Ok(())
    }

    #[test]
    fn test_decode_f32_neg_infinity() -> Result<()> {
        let input = hex!("faff800000");
        let actual = from_slice::<f32>(&input)?;
        assert!(actual.is_infinite());
        assert!(!actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_f64_infinity() -> Result<()> {
        let input = hex!("fb7ff0000000000000");
        let actual = from_slice::<f64>(&input)?;
        assert!(actual.is_infinite());
        assert!(actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_f64_nan() -> Result<()> {
        let input = hex!("fb7ff8000000000000");
        let actual = from_slice::<f64>(&input)?;
        assert!(actual.is_nan());
        Ok(())
    }

    #[test]
    fn test_decode_f64_neg_infinity() -> Result<()> {
        let input = hex!("fbfff0000000000000");
        let actual = from_slice::<f64>(&input)?;
        assert!(actual.is_infinite());
        assert!(!actual.is_sign_positive());
        Ok(())
    }

    #[test]
    fn test_decode_bool_false() -> Result<()> {
        let input = hex!("f4");
        assert!(!from_slice::<bool>(&input)?);
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(20));
        Ok(())
    }

    #[test]
    fn test_decode_bool_true() -> Result<()> {
        let input = hex!("f5");
        assert!(from_slice::<bool>(&input)?);
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(21));
        Ok(())
    }

    #[test]
    fn test_decode_null() -> Result<()> {
        let input = hex!("f6");
        assert_eq!(from_slice::<Option<i8>>(&input)?, None);
        Ok(())
    }

    #[test]
    fn test_decode_undefined() -> Result<()> {
        let input = hex!("f7");
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(23));
        Ok(())
    }

    #[test]
    fn test_decode_simple_16() -> Result<()> {
        let input = hex!("f0");
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(16));
        Ok(())
    }

    #[test]
    fn test_decode_simple_255() -> Result<()> {
        let input = hex!("f8 ff");
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(255));
        Ok(())
    }

    #[test]
    fn test_decode_tag_date() -> Result<()> {
        let input = hex!("c0 74 32 30 31 33 2d 30 33 2d 32 31 54 32 30 3a 30 34 3a 30 30 5a");
        assert_eq!(
            from_slice::<Tag<&str>>(&input)?,
            Tag::new(0, "2013-03-21T20:04:00Z")
        );
        Ok(())
    }

    #[test]
    fn test_decode_tag_epoch_time_int() -> Result<()> {
        let input = hex!("c1 1a 51 4b 67 b0");
        assert_eq!(from_slice::<Tag<u32>>(&input)?, Tag::new(1, 1_363_896_240));
        Ok(())
    }

    #[test]
    fn test_decode_tag_epoch_time_fp() -> Result<()> {
        let input = hex!("c1 fb 41 d4 52 d9 ec 20 00 00");
        assert_eq!(
            from_slice::<Tag<f32>>(&input)?,
            Tag::new(1, 1_363_896_240.5)
        );
        Ok(())
    }

    #[test]
    fn test_decode_tag_base16() -> Result<()> {
        let input = hex!("d7 44 01 02 03 04");
        let expected: &[u8] = &[0x01, 0x02, 0x03, 0x04];
        assert_eq!(from_slice::<Tag<&[u8]>>(&input)?, Tag::new(23, expected));
        Ok(())
    }

    #[test]
    fn test_decode_tag_encoded_cbor() -> Result<()> {
        let input = hex!("d8 18 45 64 49 45 54 46");
        let expected: &[u8] = &[0x64, 0x49, 0x45, 0x54, 0x46];
        assert_eq!(from_slice::<Tag<&[u8]>>(&input)?, Tag::new(24, expected));
        Ok(())
    }

    #[test]
    fn test_decode_tag_uri() -> Result<()> {
        let input =
            hex!("d8 20 76 68 74 74 70 3a 2f 2f 77 77 77 2e 65 78 61 6d 70 6c 65 2e 63 6f 6d");
        assert_eq!(
            from_slice::<Tag<&str>>(&input)?,
            Tag::new(32, "http://www.example.com")
        );
        Ok(())
    }

    #[test]
    fn test_decode_empty_byte_str() -> Result<()> {
        let input = hex!("40");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[]);
        assert_eq!(*from_slice::<ByteString>(&input)?, &[]);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_empty_byte_str() -> Result<()> {
        use crate::ByteString;

        let input = hex!("40");
        assert_eq!(*from_reader::<_, ByteString>(&input[..])?, &[]);
        Ok(())
    }

    #[test]
    fn test_decode_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[0x01, 0x02, 0x03, 0x04]);
        assert_eq!(
            *from_slice::<ByteString>(&input)?,
            &[0x01, 0x02, 0x03, 0x04]
        );
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(
            *from_reader::<_, ByteString>(&input[..])?,
            &[0x01, 0x02, 0x03, 0x04]
        );
        Ok(())
    }

    #[test]
    fn test_decode_empty_text_str() -> Result<()> {
        let input = hex!("60");
        assert_eq!(from_slice::<&str>(&input)?, "");
        assert_eq!(from_slice::<String>(&input)?, String::new());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_empty_text() -> Result<()> {
        let input = hex!("60");
        assert_eq!(from_reader::<_, String>(&input[..])?, String::new());
        Ok(())
    }

    #[test]
    fn test_decode_text_str_a() -> Result<()> {
        let input = hex!("61 61");
        assert_eq!(from_slice::<&str>(&input)?, "a");
        assert_eq!(from_slice::<String>(&input)?, "a".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_a() -> Result<()> {
        let input = hex!("61 61");
        assert_eq!(from_reader::<_, String>(&input[..])?, "a".to_string());
        Ok(())
    }

    #[test]
    fn test_decode_text_str_ietf() -> Result<()> {
        let input = hex!("64 49 45 54 46");
        assert_eq!(from_slice::<&str>(&input)?, "IETF");
        assert_eq!(from_slice::<String>(&input)?, "IETF".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_ietf() -> Result<()> {
        let input = hex!("64 49 45 54 46");
        assert_eq!(from_reader::<_, String>(&input[..])?, "IETF".to_string());
        Ok(())
    }

    #[test]
    fn test_decode_text_str_escaped() -> Result<()> {
        let input = hex!("62 22 5c");
        assert_eq!(from_slice::<&str>(&input)?, "\"\\");
        assert_eq!(from_slice::<String>(&input)?, "\"\\".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_escaped() -> Result<()> {
        let input = hex!("62 22 5c");
        assert_eq!(from_reader::<_, String>(&input[..])?, "\"\\".to_string());
        Ok(())
    }

    #[test]
    fn test_decode_text_str_unicode() -> Result<()> {
        let input = hex!("62 c3 bc");
        assert_eq!(from_slice::<&str>(&input)?, "\u{00fc}");
        assert_eq!(from_slice::<String>(&input)?, "\u{00fc}".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_unicode() -> Result<()> {
        let input = hex!("62 c3 bc");
        assert_eq!(
            from_reader::<_, String>(&input[..])?,
            "\u{00fc}".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_decode_text_str_unicode_2() -> Result<()> {
        let input = hex!("63 e6 b0 b4");
        assert_eq!(from_slice::<&str>(&input)?, "\u{6c34}");
        assert_eq!(from_slice::<String>(&input)?, "\u{6c34}".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_unicode_2() -> Result<()> {
        let input = hex!("63 e6 b0 b4");
        assert_eq!(
            from_reader::<_, String>(&input[..])?,
            "\u{6c34}".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_decode_text_str_unicode_3() -> Result<()> {
        let input = hex!("64 f0 90 85 91");
        let expected = String::from_utf16(&[0xD800, 0xDD51]).unwrap();
        assert_eq!(from_slice::<&str>(&input)?, expected);
        assert_eq!(from_slice::<String>(&input)?, expected);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_text_str_unicode_3() -> Result<()> {
        let input = hex!("64 f0 90 85 91");
        let expected = String::from_utf16(&[0xD800, 0xDD51]).unwrap();
        assert_eq!(from_reader::<_, String>(&input[..])?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_empty_arr() -> Result<()> {
        let input = hex!("80");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, &[]);
        Ok(())
    }

    #[test]
    fn test_decode_arr() -> Result<()> {
        let input = hex!("83 01 02 03");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, &[1, 2, 3]);
        Ok(())
    }

    #[test]
    fn test_decode_nested_arr() -> Result<()> {
        let input = hex!("83 01 82 02 03 82 04 05");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_nested_arr_value() -> Result<()> {
        let input = hex!("83 01 82 02 03 82 04 05");
        let expected: Value = Value::Array(vec![
            1.into(),
            Value::Array(vec![2.into(), 3.into()]),
            Value::Array(vec![4.into(), 5.into()]),
        ]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_arr_len_greater_than_24() -> Result<()> {
        let input = hex!("98 19 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 18 18 19");
        assert_eq!(
            from_slice::<Vec<u8>>(&input)?,
            &[
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25
            ],
        );
        Ok(())
    }

    #[test]
    fn test_decode_empty_map() -> Result<()> {
        let input = hex!("a0");
        assert_eq!(
            from_slice::<BTreeMap<String, String>>(&input)?,
            BTreeMap::default()
        );
        Ok(())
    }

    #[test]
    fn test_decode_map() -> Result<()> {
        let input = hex!("a2 01 02 03 04");
        let expected = BTreeMap::from([(1, 2), (3, 4)]);
        assert_eq!(from_slice::<BTreeMap<u8, u8>>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_map_value() -> Result<()> {
        let input = hex!("a2 61 61 01 61 62 82 02 03");
        let m: BTreeMap<Value, Value> = [
            ("a".into(), 1.into()),
            ("b".into(), Value::Array(vec![2.into(), 3.into()])),
        ]
        .into_iter()
        .collect();
        let expected: Value = Value::Map(m);
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_arr_value() -> Result<()> {
        let input = hex!("82 61 61 a1 61 62 61 63");
        let m: BTreeMap<Value, Value> = [("b".into(), "c".into())].into_iter().collect();
        let expected: Value = Value::Array(vec!["a".into(), Value::Map(m)]);
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_map_value_letters() -> Result<()> {
        let input = hex!("a5 61 61 61 41 61 62 61 42 61 63 61 43 61 64 61 44 61 65 61 45");
        let m: BTreeMap<Value, Value> = [
            ("a".into(), "A".into()),
            ("b".into(), "B".into()),
            ("c".into(), "C".into()),
            ("d".into(), "D".into()),
            ("e".into(), "E".into()),
        ]
        .into_iter()
        .collect();
        let expected: Value = Value::Map(m);
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_byte_str() -> Result<()> {
        let input = hex!("5f42010243030405ff");
        assert_eq!(
            *from_slice::<ByteString>(&input)?,
            &[0x01, 0x02, 0x03, 0x04, 0x05]
        );
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_indefinite_byte_str() -> Result<()> {
        let input = hex!("5f42010243030405ff");
        assert_eq!(
            *from_reader::<_, ByteString>(&input[..])?,
            &[0x01, 0x02, 0x03, 0x04, 0x05]
        );
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_text_str() -> Result<()> {
        let input = hex!("7f657374726561646d696e67ff");
        assert_eq!(from_slice::<String>(&input)?, "streaming");
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_indefinite_text_str() -> Result<()> {
        let input = hex!("7f657374726561646d696e67ff");
        assert_eq!(from_reader::<_, String>(&input[..])?, "streaming");
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_empty_arr() -> Result<()> {
        let input = hex!("9f ff");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, &[]);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_nested_indefinite_arr() -> Result<()> {
        let input = hex!("9f 01 82 02 03 9f 04 05 ff ff");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_nested_arr() -> Result<()> {
        let input = hex!("9f 01 82 02 03 82 04 05 ff");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_nested_indefinite_arr() -> Result<()> {
        let input = hex!("83 01 82 02 03 9f 04 05 ff");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_nested_indefinite_arr_2() -> Result<()> {
        let input = hex!("83 01 9f 02 03 ff 82 04 05");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite() -> Result<()> {
        let input = hex!("9f 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 18 18 19 ff");
        assert_eq!(
            from_slice::<Vec<u8>>(&input)?,
            &[
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25
            ]
        );
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_map_value() -> Result<()> {
        let input = hex!("bf61610161629f0203ffff");
        let m: BTreeMap<Value, Value> = [
            ("a".into(), 1.into()),
            ("b".into(), Value::Array(vec![2.into(), 3.into()])),
        ]
        .into_iter()
        .collect();
        let expected: Value = Value::Map(m);
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_arr_value() -> Result<()> {
        let input = hex!("826161bf61626163ff");
        let m: BTreeMap<Value, Value> = [("b".into(), "c".into())].into_iter().collect();
        let expected: Value = Value::Array(vec!["a".into(), Value::Map(m)]);
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_indefinite_map_value_2() -> Result<()> {
        let input = hex!("bf6346756ef563416d7421ff");
        let expected = Value::Map(
            [
                ("Fun".into(), true.into()),
                ("Amt".into(), Value::from(-2i32)),
            ]
            .into_iter()
            .collect(),
        );
        assert_eq!(from_slice::<Value>(&input)?, expected);
        Ok(())
    }

    #[test]
    fn test_decode_option() -> Result<()> {
        let input = hex!("f5");
        assert_eq!(from_slice::<Option<bool>>(&input)?, Some(true));
        Ok(())
    }
}
