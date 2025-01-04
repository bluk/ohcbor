use crate::{
    buf::Buffer,
    decode::{ArrAccess, DecodeSeed, Decoder, Error, MapAccess, Visitor},
    error::ErrorKind,
    read::{Read, Ref},
    Simple, ADDTL_INFO_MASK, BREAK_CODE, IB_ARRAY_MIN, IB_BYTE_STR_MIN, IB_FP_SIMPLE_MIN,
    IB_MAP_MIN, IB_NEG_INT_MIN, IB_TAG_MIN, IB_TEXT_STR_MIN, IB_UINT_MIN,
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

    fn on_end_arr(&mut self, remaining: Option<usize>) -> Result<(), crate::Error> {
        if let Some(remaining) = remaining {
            if 0 < remaining {
                Err(crate::Error::new(
                    ErrorKind::TrailingData,
                    self.read.byte_offset(),
                ))
            } else {
                Ok(())
            }
        } else {
            match self.parse_peek()? {
                BREAK_CODE => {
                    self.parse_next()?;
                    Ok(())
                }
                _ => Err(crate::Error::new(
                    ErrorKind::TrailingData,
                    self.read.byte_offset(),
                )),
            }
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
            IB_UINT_MIN..IB_NEG_INT_MIN => {
                let addtl_info = init_byte & ADDTL_INFO_MASK;
                match addtl_info {
                    0..24 => visitor.visit_u8(addtl_info),
                    24 => {
                        let val = self.parse_next()?;
                        visitor.visit_u8(val)
                    }
                    25 => {
                        let val = self.read.parse_u16()?;
                        visitor.visit_u16(val)
                    }
                    26 => {
                        let val = self.read.parse_u32()?;
                        visitor.visit_u32(val)
                    }
                    27 => {
                        let val = self.read.parse_u64()?;
                        visitor.visit_u64(val)
                    }
                    28..=31 => Err(Error::malformed(self.read.byte_offset())),
                    _ => {
                        unreachable!()
                    }
                }
            }
            IB_NEG_INT_MIN..IB_BYTE_STR_MIN => {
                let addtl_info = init_byte & ADDTL_INFO_MASK;
                match addtl_info {
                    0..24 => {
                        let arg_val =
                            i8::try_from(addtl_info).expect("argument should be less than 24");
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
                        let val = self.read.parse_u16()?;

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
                        let val = self.read.parse_u32()?;

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
                let mut remaining = self.read.parse_len(init_byte)?;

                let ret = visitor.visit_arr(ArrAccessImpl {
                    de: self,
                    remaining: &mut remaining,
                });

                match (ret, self.on_end_arr(remaining)) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            IB_MAP_MIN..IB_TAG_MIN => {
                let len = self.read.parse_len(init_byte)?;

                visitor.visit_map(MapAccessImpl {
                    de: self,
                    len,
                    count: 0,
                })
            }
            IB_TAG_MIN..IB_FP_SIMPLE_MIN => {
                let arg_val = init_byte & ADDTL_INFO_MASK;
                let tag_num = match arg_val {
                    0..24 => u64::from(arg_val),
                    24 => {
                        let val = self.parse_next()?;
                        u64::from(val)
                    }
                    25 => {
                        let val = self.read.parse_u16()?;
                        u64::from(val)
                    }
                    26 => {
                        let val = self.read.parse_u32()?;
                        u64::from(val)
                    }
                    27 => self.read.parse_u64()?,
                    28..=31 => return Err(Error::malformed(self.read.byte_offset())),
                    _ => {
                        unreachable!()
                    }
                };

                visitor.visit_tag(tag_num, self)
            }
            IB_FP_SIMPLE_MIN..=0xff => {
                let arg_val = init_byte & ADDTL_INFO_MASK;
                match arg_val {
                    0..24 => visitor.visit_simple(Simple::from(arg_val)),
                    24 => {
                        let val = self.parse_next()?;
                        if val < 32 {
                            Err(Error::malformed(self.read.byte_offset()))
                        } else {
                            visitor.visit_simple(Simple::from(val))
                        }
                    }
                    25 => {
                        todo!()
                    }
                    26 => {
                        let val = f32::from_be_bytes(self.read.read_arr()?);
                        visitor.visit_f32(val)
                    }
                    27 => {
                        let val = f64::from_be_bytes(self.read.read_arr()?);
                        visitor.visit_f64(val)
                    }
                    28..=31 => Err(Error::malformed(self.read.byte_offset())),
                    _ => {
                        unreachable!()
                    }
                }
            }
        }
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
}

#[cfg(test)]
mod tests {
    use crate::{from_slice, Result, Simple, Tag};

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
        assert_eq!(from_slice::<Simple>(&input)?, Simple::new(22));
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
    fn test_decode_option() -> Result<()> {
        let input = hex!("f5");
        assert_eq!(from_slice::<Option<bool>>(&input)?, Some(true));
        Ok(())
    }
}
