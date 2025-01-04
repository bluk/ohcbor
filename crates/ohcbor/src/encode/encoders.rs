use crate::{
    encode::{self, Encode},
    error::{Error, Result},
    tag,
    write::Write,
    Simple, IB_ARRAY_MIN, IB_BYTE_STR_MIN, IB_FP_SIMPLE_MIN, IB_MAP_MIN, IB_NEG_INT_MIN,
    IB_TAG_MIN, IB_TEXT_STR_MIN,
};

/// A CBOR Encoder for types which implement [`Encode`].
#[derive(Debug)]
pub struct Encoder<W> {
    writer: W,
}

impl<W> Encoder<W>
where
    W: Write,
{
    /// Constructs a `Encoder` with a [Write] target.
    pub fn new(writer: W) -> Self {
        Encoder { writer }
    }
}

impl<W> Encoder<W>
where
    W: Write,
{
    /// Returns the inner writer.
    ///
    /// Useful when the encoder is done and the writer is needed to write other data.
    #[inline]
    pub fn into_inner(self) -> W {
        self.writer
    }

    fn write_init_byte_len(&mut self, ty: u8, len: usize) -> Result<()> {
        if len < 24 {
            let len = u8::try_from(len).expect("length is greater than 24");
            self.writer.write_all(&[ty | len])?;
        } else if let Ok(len) = u8::try_from(len) {
            self.writer.write_all(&[ty | 24])?;
            self.writer.write_all(&len.to_be_bytes())?;
        } else if let Ok(len) = u16::try_from(len) {
            self.writer.write_all(&[ty | 25])?;
            self.writer.write_all(&len.to_be_bytes())?;
        } else if let Ok(len) = u32::try_from(len) {
            self.writer.write_all(&[ty | 26])?;
            self.writer.write_all(&len.to_be_bytes())?;
        } else if let Ok(len) = u64::try_from(len) {
            self.writer.write_all(&[ty | 27])?;
            self.writer.write_all(&len.to_be_bytes())?;
        } else {
            self.writer.write_all(&[ty | 31])?;

            todo!()
        }

        Ok(())
    }
}

impl<'a, W> encode::Encoder for &'a mut Encoder<W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    type EncodeArr = EncodeArr<'a, W>;
    type EncodeMap = EncodeMap<'a, W>;

    #[inline]
    fn encode_i8(self, value: i8) -> Result<()> {
        if 0 <= value {
            return self.encode_u8(u8::try_from(value).expect("value should be positive"));
        }

        let value = value + 1;
        let value = value.abs();

        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[IB_NEG_INT_MIN | value])
        } else {
            let value = u8::try_from(value).expect("value is greater than u8::MAX");
            self.writer.write_all(&[IB_NEG_INT_MIN | 24, value])
        }
    }

    #[inline]
    fn encode_i16(self, value: i16) -> Result<()> {
        if 0 <= value {
            return self.encode_u16(u16::try_from(value).expect("value should be positive"));
        }

        let value = value + 1;
        let value = value.abs();

        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[IB_NEG_INT_MIN | value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 24, value])
        } else {
            let value = u16::try_from(value).expect("value is greater than u16::MAX");
            self.writer.write_all(&[IB_NEG_INT_MIN | 25])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_i32(self, value: i32) -> Result<()> {
        if 0 <= value {
            return self.encode_u32(u32::try_from(value).expect("value should be positive"));
        }

        let value = value + 1;
        let value = value.abs();

        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[IB_NEG_INT_MIN | value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            let value = u32::try_from(value).expect("value is greater than u32::MAX");
            self.writer.write_all(&[IB_NEG_INT_MIN | 26])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_i64(self, value: i64) -> Result<()> {
        if 0 <= value {
            return self.encode_u64(u64::try_from(value).expect("value should be positive"));
        }

        let value = value + 1;
        let value = value.abs();

        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[IB_NEG_INT_MIN | value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u32::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 26])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            let value = u64::try_from(value).expect("value is greater than u64::MAX");
            self.writer.write_all(&[IB_NEG_INT_MIN | 27])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_i128(self, value: i128) -> Result<()> {
        if 0 <= value {
            return self.encode_u16(u16::try_from(value).expect("value should be positive"));
        }

        let value = value + 1;
        let value = value.abs();

        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[IB_NEG_INT_MIN | value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u32::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 26])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u64::try_from(value) {
            self.writer.write_all(&[IB_NEG_INT_MIN | 27])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            todo!()
        }
    }

    #[inline]
    fn encode_u8(self, value: u8) -> Result<()> {
        if value < 24 {
            self.writer.write_all(&[value])
        } else {
            self.writer.write_all(&[24, value])
        }
    }

    #[inline]
    fn encode_u16(self, value: u16) -> Result<()> {
        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[24, value])
        } else {
            self.writer.write_all(&[25])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_u32(self, value: u32) -> Result<()> {
        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            self.writer.write_all(&[26])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_u64(self, value: u64) -> Result<()> {
        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u32::try_from(value) {
            self.writer.write_all(&[26])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            self.writer.write_all(&[27])?;
            self.writer.write_all(&value.to_be_bytes())
        }
    }

    #[inline]
    fn encode_u128(self, value: u128) -> Result<()> {
        if value < 24 {
            let value = u8::try_from(value).expect("value is greater than 24");
            self.writer.write_all(&[value])
        } else if let Ok(value) = u8::try_from(value) {
            self.writer.write_all(&[24, value])
        } else if let Ok(value) = u16::try_from(value) {
            self.writer.write_all(&[25])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u32::try_from(value) {
            self.writer.write_all(&[26])?;
            self.writer.write_all(&value.to_be_bytes())
        } else if let Ok(value) = u64::try_from(value) {
            self.writer.write_all(&[27])?;
            self.writer.write_all(&value.to_be_bytes())
        } else {
            todo!()
        }
    }

    #[inline]
    fn encode_str(self, value: &str) -> Result<()> {
        self.write_init_byte_len(IB_TEXT_STR_MIN, value.len())?;
        self.writer.write_all(value.as_bytes())
    }

    #[inline]
    fn encode_bytes(self, value: &[u8]) -> Result<()> {
        self.write_init_byte_len(IB_BYTE_STR_MIN, value.len())?;
        self.writer.write_all(value)
    }

    #[inline]
    fn encode_arr(self, len: Option<usize>) -> Result<Self::EncodeArr> {
        if let Some(len) = len {
            self.write_init_byte_len(IB_ARRAY_MIN, len)?;
        } else {
            self.writer.write_all(&[IB_ARRAY_MIN | 31])?;
        }
        Ok(EncodeArr::new(self, len))
    }

    #[inline]
    fn encode_map(self, len: Option<usize>) -> Result<Self::EncodeMap> {
        if let Some(len) = len {
            self.write_init_byte_len(IB_MAP_MIN, len)?;
        } else {
            self.writer.write_all(&[IB_MAP_MIN | 31])?;
        }
        Ok(EncodeMap::new(self, len))
    }

    fn encode_tag<T>(self, tag_num: tag::Num, v: &T) -> Result<()>
    where
        T: ?Sized + Encode,
    {
        if tag_num < 24 {
            let tag_num = u8::try_from(tag_num).expect("value is greater than 24");
            self.writer.write_all(&[IB_TAG_MIN | tag_num])?;
        } else if let Ok(tag_num) = u8::try_from(tag_num) {
            self.writer.write_all(&[IB_TAG_MIN | 24, tag_num])?;
        } else if let Ok(tag_num) = u16::try_from(tag_num) {
            self.writer.write_all(&[IB_TAG_MIN | 25])?;
            self.writer.write_all(&tag_num.to_be_bytes())?;
        } else if let Ok(tag_num) = u32::try_from(tag_num) {
            self.writer.write_all(&[IB_TAG_MIN | 26])?;
            self.writer.write_all(&tag_num.to_be_bytes())?;
        } else {
            self.writer.write_all(&[IB_TAG_MIN | 27])?;
            self.writer.write_all(&tag_num.to_be_bytes())?;
        }

        v.encode(self)
    }

    #[inline]
    fn encode_simple<I>(self, v: I) -> Result<()>
    where
        I: Into<Simple>,
    {
        let v = u8::from(v.into());
        if v < 24 {
            self.writer.write_all(&[IB_FP_SIMPLE_MIN | v])
        } else if (24..32).contains(&v) {
            todo!()
        } else {
            self.writer.write_all(&[IB_FP_SIMPLE_MIN | 24, v])
        }
    }

    #[inline]
    fn encode_f32(self, value: f32) -> Result<()> {
        self.writer.write_all(&[IB_FP_SIMPLE_MIN | 26])?;
        self.writer.write_all(&value.to_be_bytes())
    }

    #[inline]
    fn encode_f64(self, value: f64) -> Result<()> {
        self.writer.write_all(&[IB_FP_SIMPLE_MIN | 27])?;
        self.writer.write_all(&value.to_be_bytes())
    }
}

/// Encoder for writing map data.
#[doc(hidden)]
#[derive(Debug)]
pub struct EncodeArr<'a, W> {
    enc: &'a mut Encoder<W>,
    remaining: Option<usize>,
}

impl<'a, W> EncodeArr<'a, W>
where
    W: Write,
{
    #[inline]
    fn new(enc: &'a mut Encoder<W>, remaining: Option<usize>) -> Self {
        Self { enc, remaining }
    }
}

impl<W> encode::EncodeArr for EncodeArr<'_, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn encode_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Encode,
    {
        if let Some(rem) = self.remaining {
            self.remaining = Some(rem - 1);
        }
        value.encode(&mut *self.enc)
    }

    #[inline]
    fn end(self) -> Result<()> {
        if let Some(remaining) = self.remaining {
            if 0 < remaining {
                todo!()
            }
        } else {
            todo!()
            // End with marker
        }

        Ok(())
    }
}

/// Encoder for writing map data.
#[doc(hidden)]
#[derive(Debug)]
pub struct EncodeMap<'a, W> {
    enc: &'a mut Encoder<W>,
    remaining: Option<usize>,
    encoded_key: bool,
}

impl<'a, W> EncodeMap<'a, W>
where
    W: Write,
{
    #[inline]
    fn new(enc: &'a mut Encoder<W>, remaining: Option<usize>) -> Self {
        Self {
            enc,
            remaining,
            encoded_key: false,
        }
    }
}

impl<W> encode::EncodeMap for EncodeMap<'_, W>
where
    W: Write,
{
    type Ok = ();
    type Error = Error;

    #[inline]
    fn encode_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Encode,
    {
        if self.encoded_key {
            todo!();
        }
        self.encoded_key = true;
        key.encode(&mut *self.enc)
    }

    #[inline]
    fn encode_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Encode,
    {
        if !self.encoded_key {
            todo!()
        }
        self.encoded_key = false;
        if let Some(rem) = self.remaining {
            self.remaining = Some(rem - 1);
        }
        value.encode(&mut *self.enc)
    }

    #[inline]
    fn end(self) -> Result<()> {
        if let Some(remaining) = self.remaining {
            if 0 < remaining {
                todo!()
            }
        } else {
            todo!()
            // End with marker
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{from_slice, tag, Simple, Tag};

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::{collections::BTreeMap, string::String, vec::Vec};
    #[cfg(feature = "std")]
    use std::{collections::BTreeMap, string::String, vec::Vec};

    #[cfg(any(feature = "alloc", feature = "std"))]
    use crate::{to_vec, ByteString};

    use proptest::prelude::*;

    prop_compose! {
        fn tag()(num in any::<tag::Num>(), content in any::<u64>()) -> Tag<u64> {
            Tag::new(num, content)
        }
    }

    proptest::proptest! {
        #[test]
        fn test_u8(v in 0..=u8::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_u16(v in 0..=u16::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_u32(v in 0..=u32::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_u64(v in 0..=u64::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_usize(v in 0..=usize::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_i8(v in i8::MIN..=i8::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_i16(v in i16::MIN..=i16::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_i32(v in i32::MIN..=i32::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_i64(v in i64::MIN..=i64::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_isize(v in isize::MIN..=isize::MAX) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[cfg(any(feature = "alloc", feature = "std"))]
        #[test]
        fn test_bytes(v in prop::collection::vec(u8::MIN..=u8::MAX, 0..256).prop_map(ByteString::from)) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<ByteString>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_str(v in ".*") {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<&str>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_arr(v in prop::collection::vec(i64::MIN..=i64::MAX, 0..256)) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<Vec<i64>>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_map(v in prop::collection::btree_map(".*", ".*", 0..256)) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<BTreeMap<String, String>>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_tag(v in tag()) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<Tag<u64>>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_simple_value_less_than_24(v in ((0..=23u8).prop_map(Simple::new))) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<Simple>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[test]
        fn test_simple_value_greater_than_31(v in ((32..=u8::MAX).prop_map(Simple::new))) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<Simple>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[allow(clippy::float_cmp)]
        #[test]
        fn test_f32(v in any::<f32>()) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<f32>(&output)?;
            assert_eq!(v, decoded_v);
        }

        #[allow(clippy::float_cmp)]
        #[test]
        fn test_f64(v in any::<f64>()) {
            let output = to_vec(&v)?;
            let decoded_v = from_slice::<f64>(&output)?;
            assert_eq!(v, decoded_v);
        }
    }
}
