//! Deserializes CBOR data.

use crate::error::{Error, ErrorKind, Result};
use crate::read::{self, Read, Ref};
use serde::de::{self};

use core::str;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::{io, vec::Vec};

/// Deserializes an instance of `T` from the bytes of an [`io::Read`] type.
///
/// The entire [`io::Read`] source is consumed, and it is an error if there is
/// trailing data.
///
/// # Errors
///
/// Deserialization can fail if the data is not valid, if the data cannot cannot
/// be deserialized into an instance of `T`, if there is trailing data, and
/// other IO errors.
#[cfg(feature = "std")]
pub fn from_reader<R, T>(r: R) -> Result<T>
where
    R: io::Read,
    T: de::DeserializeOwned,
{
    let mut de = Deserializer::new(read::IoRead::new(r));
    let value = T::deserialize(&mut de)?;
    de.end()?;
    Ok(value)
}

/// Deserializes an instance of `T` from a slice of bytes.
///
/// The entire slice of bytes is consumed, and it is an error if there is
/// trailing data.
///
/// # Errors
///
/// Deserialization can fail if the data is not valid, if the data cannot
/// be deserialized into an instance of `T`, if there is trailing data, and
/// other IO errors.
pub fn from_slice<'a, T>(s: &'a [u8]) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    let mut de = Deserializer::new(read::SliceRead::new(s));
    let value = T::deserialize(&mut de)?;
    de.end()?;
    Ok(value)
}

#[derive(Debug)]
/// A `CBOR` Deserializer for types which implement [Deserialize][serde::de::Deserialize].
pub struct Deserializer<R> {
    read: R,
    /// Temporary buffer used to reduce allocations made
    buf: Vec<u8>,
}

impl<'a, R> Deserializer<R>
where
    R: Read<'a>,
{
    /// Constructs a Deserializer from a readable source.
    pub fn new(read: R) -> Self {
        Deserializer {
            read,
            buf: Vec::default(),
        }
    }

    /// Should be called after a value from the source is deserialized to
    /// validate that the entire source was read.
    ///
    /// If trailing data is expected, do not call this method.
    ///
    /// # Errors
    ///
    /// An error is returned if there are unconsumed bytes in the readable
    /// source.
    pub fn end(&mut self) -> Result<()> {
        match self.read.peek() {
            Some(r) => r.and(Err(Error::new(
                ErrorKind::TrailingData,
                self.read.byte_offset(),
            ))),
            None => Ok(()),
        }
    }

    #[inline]
    fn parse_peek(&mut self) -> Result<u8> {
        self.read
            .peek()
            .ok_or_else(|| Error::new(ErrorKind::EofWhileParsingValue, self.read.byte_offset()))?
    }

    #[inline]
    fn parse_next(&mut self) -> Result<u8> {
        self.read
            .next()
            .ok_or_else(|| Error::new(ErrorKind::EofWhileParsingValue, self.read.byte_offset()))?
    }
}

#[cfg(feature = "std")]
impl<R> Deserializer<read::IoRead<R>>
where
    R: io::Read,
{
    /// Constructs a Deserializer from an [`std::io::Read`][std::io::Read] source.
    #[must_use]
    pub fn from_reader(reader: R) -> Self {
        Deserializer::new(read::IoRead::new(reader))
    }
}

impl<'a> Deserializer<read::SliceRead<'a>> {
    /// Constructs a Deserializer from a `&[u8]`.
    #[must_use]
    pub fn from_slice(bytes: &'a [u8]) -> Self {
        Deserializer::new(read::SliceRead::new(bytes))
    }
}

macro_rules! forward_deserialize_signed_integer {
    ($method:ident) => {
        #[inline]
        fn $method<V>(self, visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            self.deserialize_i128(visitor)
        }
    };
}

macro_rules! forward_deserialize_unsigned_integer {
    ($method:ident) => {
        #[inline]
        fn $method<V>(self, visitor: V) -> Result<V::Value>
        where
            V: de::Visitor<'de>,
        {
            self.deserialize_u128(visitor)
        }
    };
}

impl<'de, R: Read<'de>> de::Deserializer<'de> for &mut Deserializer<R> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        todo!()
    }

    serde::forward_to_deserialize_any! {
        f32 f64 unit unit_struct

        char

        struct enum identifier ignored_any
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_next()?;

        match init_byte {
            0b1111_0100 => visitor.visit_bool(false),
            0b1111_0101 => visitor.visit_bool(true),
            _ => todo!(),
        }
    }

    forward_deserialize_signed_integer!(deserialize_i8);
    forward_deserialize_signed_integer!(deserialize_i16);
    forward_deserialize_signed_integer!(deserialize_i32);
    forward_deserialize_signed_integer!(deserialize_i64);

    #[expect(clippy::too_many_lines)]
    fn deserialize_i128<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        let is_neg = match init_byte & 0b1110_0000 {
            0b0000_0000 => false,
            0b0010_0000 => true,
            _ => {
                // Error
                todo!()
            }
        };

        self.parse_next()?;

        let arg_val = init_byte & 0b0001_1111;
        match arg_val {
            0..24 => {
                if is_neg {
                    let arg_val = i8::try_from(arg_val).expect("argument should be less than 24");
                    debug_assert!(arg_val < 24);
                    visitor.visit_i8(-1 - arg_val)
                } else {
                    visitor.visit_u8(arg_val)
                }
            }
            24 => {
                let val = self.parse_next()?;

                if is_neg {
                    if let Some(val) = i8::try_from(val)
                        .ok()
                        .and_then(|val| (-1i8).checked_sub(val))
                    {
                        visitor.visit_i8(val)
                    } else {
                        visitor.visit_i16(-1 - i16::from(val))
                    }
                } else {
                    visitor.visit_u8(val)
                }
            }
            25 => {
                let val = u16::from_be_bytes([self.parse_next()?, self.parse_next()?]);

                if is_neg {
                    if let Some(val) = i16::try_from(val)
                        .ok()
                        .and_then(|val| (-1i16).checked_sub(val))
                    {
                        visitor.visit_i16(val)
                    } else {
                        visitor.visit_i32(-1 - i32::from(val))
                    }
                } else {
                    visitor.visit_u16(val)
                }
            }
            26 => {
                let val = u32::from_be_bytes([
                    self.parse_next()?,
                    self.parse_next()?,
                    self.parse_next()?,
                    self.parse_next()?,
                ]);

                if is_neg {
                    if let Some(val) = i32::try_from(val)
                        .ok()
                        .and_then(|val| (-1i32).checked_sub(val))
                    {
                        visitor.visit_i32(val)
                    } else {
                        visitor.visit_i64(-1 - i64::from(val))
                    }
                } else {
                    visitor.visit_u32(val)
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

                if is_neg {
                    if let Some(val) = i64::try_from(val)
                        .ok()
                        .and_then(|val| (-1i64).checked_sub(val))
                    {
                        visitor.visit_i64(val)
                    } else {
                        visitor.visit_i128(-1 - i128::from(val))
                    }
                } else {
                    visitor.visit_u64(val)
                }
            }
            28..=30 => {
                todo!()
            }
            31 => {
                todo!()
            }
            _ => {
                todo!()
            }
        }
    }

    forward_deserialize_unsigned_integer!(deserialize_u8);
    forward_deserialize_unsigned_integer!(deserialize_u16);
    forward_deserialize_unsigned_integer!(deserialize_u32);
    forward_deserialize_unsigned_integer!(deserialize_u64);

    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // The implementation should be the same as i28 for this data model
        self.deserialize_i128(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        match init_byte & 0b1110_0000 {
            0b0100_0000 => {}
            _ => {
                // Error
                todo!()
            }
        };
        self.parse_next()?;

        let len = self.read.parse_len(init_byte)?.unwrap();

        self.buf.clear();
        match self.read.read_exact(len, &mut self.buf)? {
            Ref::Source(bytes) => visitor.visit_borrowed_bytes(bytes),
            Ref::Buffer(bytes) => visitor.visit_bytes(bytes),
        }
    }

    #[inline]
    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        match init_byte & 0b1110_0000 {
            0b0110_0000 => {}
            _ => {
                // Error
                todo!()
            }
        };
        self.parse_next()?;

        let len = self.read.parse_len(init_byte)?.unwrap();

        self.buf.clear();
        match self.read.read_exact(len, &mut self.buf)? {
            Ref::Source(bytes) => match str::from_utf8(bytes) {
                Ok(s) => visitor.visit_borrowed_str(s),
                Err(e) => Err(Error::new(
                    ErrorKind::InvalidUtf8Error(e),
                    self.read.byte_offset(),
                )),
            },
            Ref::Buffer(bytes) => match str::from_utf8(bytes) {
                Ok(s) => visitor.visit_str(s),
                Err(e) => Err(Error::new(
                    ErrorKind::InvalidUtf8Error(e),
                    self.read.byte_offset(),
                )),
            },
        }
    }

    #[inline]
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        // The implementation should be the same as str for this data model
        self.deserialize_str(visitor)
    }

    #[inline]
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_next()?;

        match init_byte {
            0b1111_0110 => visitor.visit_none(),
            _ => visitor.visit_some(self),
        }
    }

    #[inline]
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        match init_byte & 0b1110_0000 {
            0b1000_0000 => {}
            _ => {
                // Error
                todo!()
            }
        };

        self.parse_next()?;

        let len = self.read.parse_len(init_byte)?;

        visitor.visit_seq(SeqAccess {
            de: self,
            len,
            count: 0,
        })
    }

    #[inline]
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    #[inline]
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        let init_byte = self.parse_peek()?;

        match init_byte & 0b1110_0000 {
            0b1010_0000 => {}
            _ => {
                // Error
                todo!()
            }
        };

        self.parse_next()?;

        let len = self.read.parse_len(init_byte)?;

        visitor.visit_map(MapAccess {
            de: self,
            len,
            count: 0,
        })
    }

    #[inline]
    fn is_human_readable(&self) -> bool {
        false
    }
}

struct SeqAccess<'a, R> {
    de: &'a mut Deserializer<R>,
    /// Expected number of items. If `None`, the list has an indefinite length
    len: Option<usize>,
    /// Number of elements already deserialized
    count: usize,
}

impl<'de, 'a, R: Read<'de> + 'a> de::SeqAccess<'de> for SeqAccess<'a, R> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: de::DeserializeSeed<'de>,
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

        Ok(Some(seed.deserialize(&mut *self.de)?))
    }

    /// Returns the number of elements remaining in the sequence, if known.
    #[inline]
    fn size_hint(&self) -> Option<usize> {
        self.len
    }
}

struct MapAccess<'a, R> {
    de: &'a mut Deserializer<R>,
    len: Option<usize>,
    count: usize,
}

impl<'de, 'a, R: Read<'de> + 'a> de::MapAccess<'de> for MapAccess<'a, R> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: de::DeserializeSeed<'de>,
    {
        if let Some(len) = self.len {
            if len == self.count {
                return Ok(None);
            }
            self.count += 1;

            seed.deserialize(MapKey { de: &mut *self.de }).map(Some)
        } else {
            // Indefinite length
            todo!()
        }
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: de::DeserializeSeed<'de>,
    {
        seed.deserialize(&mut *self.de)
    }

    fn size_hint(&self) -> Option<usize> {
        self.len
    }
}

struct MapKey<'a, R> {
    de: &'a mut Deserializer<R>,
}

impl<'de, R> de::Deserializer<'de> for MapKey<'_, R>
where
    R: Read<'de>,
{
    type Error = Error;

    #[inline]
    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.de.deserialize_any(visitor)
    }

    #[inline]
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    #[inline]
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.de.deserialize_str(visitor)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.de.deserialize_string(visitor)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: de::Visitor<'de>,
    {
        self.de.deserialize_u8(visitor)
    }

    serde::forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u16 u32 u64 f32 f64 unit unit_struct seq tuple tuple_struct map
        char bytes byte_buf enum struct identifier ignored_any
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hex_literal::hex;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::{
        collections::BTreeMap,
        string::{String, ToString},
        vec,
    };
    #[cfg(feature = "std")]
    use std::{collections::BTreeMap, string::String, vec};

    macro_rules! assert_deser_u64_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i64>(&input)?,
                "deserializing i64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<i128>(&input)?,
                "deserializing i128 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u64>(&input)?,
                "deserializing u64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u128>(&input)?,
                "deserializing u128 {input:X?}"
            );
        };
    }

    macro_rules! assert_deser_u32_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i32>(&input)?,
                "deserializing i32 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u32>(&input)?,
                "deserializing u32 {input:X?}"
            );
            assert_deser_u64_val!($expected, $input);
        };
    }

    macro_rules! assert_deser_u16_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i16>(&input)?,
                "deserializing i16 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u16>(&input)?,
                "deserializing u16 {input:X?}"
            );
            assert_deser_u32_val!($expected, $input);
        };
    }

    macro_rules! assert_deser_u8_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i8>(&input)?,
                "deserializing i8 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<u8>(&input)?,
                "deserializing u8 {input:X?}"
            );
            assert_deser_u16_val!($expected, $input);
        };
    }

    macro_rules! assert_deser_i64_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i64>(&input)?,
                "deserializing i64 {input:X?}"
            );
            assert_eq!(
                $expected,
                from_slice::<i128>(&input)?,
                "deserializing i128 {input:X?}"
            );
        };
    }

    macro_rules! assert_deser_i32_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i32>(&input)?,
                "deserializing i32 {input:X?}"
            );
            assert_deser_i64_val!($expected, $input);
        };
    }

    macro_rules! assert_deser_i16_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i16>(&input)?,
                "deserializing i16 {input:X?}"
            );
            assert_deser_i32_val!($expected, $input);
        };
    }

    macro_rules! assert_deser_i8_val {
        ($expected:literal, $input:expr) => {
            let input = $input;
            assert_eq!(
                $expected,
                from_slice::<i8>(&input)?,
                "deserializing i8 {input:X?}"
            );
            assert_deser_i16_val!($expected, $input);
        };
    }

    #[test]
    fn test_deserialize_0() -> Result<()> {
        let input = hex!("00");
        assert_deser_u8_val!(0, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_01() -> Result<()> {
        let input = hex!("01");
        assert_deser_u8_val!(1, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_10() -> Result<()> {
        let input = hex!("0A");
        assert_deser_u8_val!(10, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_23() -> Result<()> {
        let input = hex!("17");
        assert_deser_u8_val!(23, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_24() -> Result<()> {
        let input = hex!("18 18");
        assert_deser_u8_val!(24, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_25() -> Result<()> {
        let input = hex!("18 19");
        assert_deser_u8_val!(25, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_100() -> Result<()> {
        let input = hex!("18 64");
        assert_deser_u8_val!(100, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_1_000() -> Result<()> {
        let input = hex!("19 03 e8");
        assert_deser_u16_val!(1_000, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_1_000_000() -> Result<()> {
        let input = hex!("1a 00 0f 42 40");
        assert_deser_u32_val!(1_000_000, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_1_000_000_000_000() -> Result<()> {
        let input = hex!("1b 00 00 00 e8 d4 a5 10 00");
        assert_deser_u64_val!(1_000_000_000_000, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_18_446_744_073_709_551_615() -> Result<()> {
        let input = hex!("1b ff ff ff ff ff ff ff ff");
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<i128>(&input)?,
            "deserializing i128 {input:X?}"
        );
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<u64>(&input)?,
            "deserializing u64 {input:X?}"
        );
        assert_eq!(
            18_446_744_073_709_551_615,
            from_slice::<u128>(&input)?,
            "deserializing u128 {input:X?}"
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_neg_18_446_744_073_709_551_616() -> Result<()> {
        let input = hex!("3b ff ff ff ff ff ff ff ff");
        // assert_deser_i128_val!(-18_446_744_073_709_551_616, input);
        assert_eq!(
            -18_446_744_073_709_551_616,
            from_slice::<i128>(&input)?,
            "deserializing i128 {input:X?}"
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_neg_1() -> Result<()> {
        let input = hex!("20");
        assert_deser_i8_val!(-1, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_neg_10() -> Result<()> {
        let input = hex!("29");
        assert_deser_i8_val!(-10, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_neg_100() -> Result<()> {
        let input = hex!("38 63");
        assert_deser_i8_val!(-100, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_neg_1000() -> Result<()> {
        let input = hex!("39 03 e7");
        assert_deser_i16_val!(-1000, input);
        Ok(())
    }

    #[test]
    fn test_deserialize_empty_byte_str() -> Result<()> {
        let input = hex!("40");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[]);
        assert_eq!(from_slice::<serde_bytes::ByteBuf>(&input)?, vec![]);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_empty_byte_str() -> Result<()> {
        let input = hex!("40");
        assert_eq!(from_reader::<_, serde_bytes::ByteBuf>(&input[..])?, vec![]);
        Ok(())
    }

    #[test]
    fn test_deserialize_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[0x01, 0x02, 0x03, 0x04]);
        assert_eq!(
            from_slice::<serde_bytes::ByteBuf>(&input)?,
            vec![0x01, 0x02, 0x03, 0x04]
        );
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(
            from_reader::<_, serde_bytes::ByteBuf>(&input[..])?,
            vec![0x01, 0x02, 0x03, 0x04]
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_empty_text_str() -> Result<()> {
        let input = hex!("60");
        assert_eq!(from_slice::<&str>(&input)?, "");
        assert_eq!(from_slice::<String>(&input)?, String::new());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_empty_text() -> Result<()> {
        let input = hex!("60");
        assert_eq!(from_reader::<_, String>(&input[..])?, String::new());
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_a() -> Result<()> {
        let input = hex!("61 61");
        assert_eq!(from_slice::<&str>(&input)?, "a");
        assert_eq!(from_slice::<String>(&input)?, "a".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_a() -> Result<()> {
        let input = hex!("61 61");
        assert_eq!(from_reader::<_, String>(&input[..])?, "a".to_string());
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_ietf() -> Result<()> {
        let input = hex!("64 49 45 54 46");
        assert_eq!(from_slice::<&str>(&input)?, "IETF");
        assert_eq!(from_slice::<String>(&input)?, "IETF".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_ietf() -> Result<()> {
        let input = hex!("64 49 45 54 46");
        assert_eq!(from_reader::<_, String>(&input[..])?, "IETF".to_string());
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_escaped() -> Result<()> {
        let input = hex!("62 22 5c");
        assert_eq!(from_slice::<&str>(&input)?, "\"\\");
        assert_eq!(from_slice::<String>(&input)?, "\"\\".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_escaped() -> Result<()> {
        let input = hex!("62 22 5c");
        assert_eq!(from_reader::<_, String>(&input[..])?, "\"\\".to_string());
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_unicode() -> Result<()> {
        let input = hex!("62 c3 bc");
        assert_eq!(from_slice::<&str>(&input)?, "\u{00fc}");
        assert_eq!(from_slice::<String>(&input)?, "\u{00fc}".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_unicode() -> Result<()> {
        let input = hex!("62 c3 bc");
        assert_eq!(
            from_reader::<_, String>(&input[..])?,
            "\u{00fc}".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_unicode_2() -> Result<()> {
        let input = hex!("63 e6 b0 b4");
        assert_eq!(from_slice::<&str>(&input)?, "\u{6c34}");
        assert_eq!(from_slice::<String>(&input)?, "\u{6c34}".to_string());
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_unicode_2() -> Result<()> {
        let input = hex!("63 e6 b0 b4");
        assert_eq!(
            from_reader::<_, String>(&input[..])?,
            "\u{6c34}".to_string()
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_text_str_unicode_3() -> Result<()> {
        let input = hex!("64 f0 90 85 91");
        let expected = String::from_utf16(&[0xD800, 0xDD51]).unwrap();
        assert_eq!(from_slice::<&str>(&input)?, expected);
        assert_eq!(from_slice::<String>(&input)?, expected);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_deserialize_reader_text_str_unicode_3() -> Result<()> {
        let input = hex!("64 f0 90 85 91");
        let expected = String::from_utf16(&[0xD800, 0xDD51]).unwrap();
        assert_eq!(from_reader::<_, String>(&input[..])?, expected);
        Ok(())
    }

    #[test]
    fn test_deserialize_bool_false() -> Result<()> {
        let input = hex!("f4");
        assert!(!from_slice::<bool>(&input)?);
        Ok(())
    }

    #[test]
    fn test_deserialize_bool_true() -> Result<()> {
        let input = hex!("f5");
        assert!(from_slice::<bool>(&input)?);
        Ok(())
    }

    #[test]
    fn test_deserialize_null() -> Result<()> {
        let input = hex!("f6");
        assert_eq!(from_slice::<Option<i8>>(&input)?, None);
        Ok(())
    }

    #[test]
    fn test_deserialize_empty_arr() -> Result<()> {
        let input = hex!("80");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, vec![]);
        Ok(())
    }

    #[test]
    fn test_deserialize_arr() -> Result<()> {
        let input = hex!("83 01 02 03");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, vec![1, 2, 3]);
        Ok(())
    }

    #[test]
    fn test_deserialize_nested_arr() -> Result<()> {
        let input = hex!("83 01 82 02 03 82 04 05");
        let expected: (u8, Vec<u8>, Vec<u8>) = (1, vec![2, 3], vec![4, 5]);
        assert_eq!(expected, from_slice(&input)?);
        Ok(())
    }

    #[test]
    fn test_deserialize_arr_len_greater_than_24() -> Result<()> {
        let input = hex!("98 19 01 02 03 04 05 06 07 08 09 0a 0b 0c 0d 0e 0f 10 11 12 13 14 15 16 17 18 18 18 19");
        assert_eq!(
            vec![
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25
            ],
            from_slice::<Vec<u8>>(&input)?
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_empty_map() -> Result<()> {
        let input = hex!("a0");
        assert_eq!(
            from_slice::<BTreeMap<String, String>>(&input)?,
            BTreeMap::default()
        );
        Ok(())
    }

    #[test]
    fn test_deserialize_map() -> Result<()> {
        let input = hex!("a2 01 02 03 04");
        let expected = BTreeMap::from([(1, 2), (3, 4)]);
        assert_eq!(from_slice::<BTreeMap<u8, u8>>(&input)?, expected);
        Ok(())
    }
}
