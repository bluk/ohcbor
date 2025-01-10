//! Represents CBOR data.

use core::{fmt::Display, num::TryFromIntError};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, collections::BTreeMap, fmt, str, str::FromStr, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::{boxed::Box, collections::BTreeMap, fmt, str, str::FromStr, string::String, vec::Vec};

use ordered_float::OrderedFloat;

use crate::{
    decode::{
        Decode, DecodeOwned, DecodeSeed, Decoder, IndefiniteLenItemAccess, Unexpected, Visitor,
    },
    encode::{Encode, Encoder},
    error::Error,
    ByteString, Simple, Tag,
};

/// Integer value
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Int {
    /// Positive value
    Pos(u64),
    /// Negative value
    ///
    /// # Important
    ///
    /// To calculate the real value: (-1 - u64 value)
    Neg(u64),
}

impl Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Pos(v) => Display::fmt(v, f),
            Self::Neg(v) => {
                let v: i128 = -1 - i128::from(*v);
                Display::fmt(&v, f)
            }
        }
    }
}

impl From<u8> for Int {
    fn from(v: u8) -> Self {
        Int::Pos(u64::from(v))
    }
}

impl From<u16> for Int {
    fn from(v: u16) -> Self {
        Int::Pos(u64::from(v))
    }
}

impl From<u32> for Int {
    fn from(v: u32) -> Self {
        Int::Pos(u64::from(v))
    }
}

impl From<u64> for Int {
    fn from(v: u64) -> Self {
        Int::Pos(v)
    }
}

impl TryFrom<u128> for Int {
    type Error = TryFromIntError;

    fn try_from(v: u128) -> Result<Self, Self::Error> {
        u64::try_from(v).map(Int::Pos)
    }
}

impl TryFrom<usize> for Int {
    type Error = TryFromIntError;

    fn try_from(v: usize) -> Result<Self, Self::Error> {
        u64::try_from(v).map(Int::Pos)
    }
}

impl From<i8> for Int {
    fn from(v: i8) -> Self {
        if v < 0 {
            let v = v + 1;
            Int::Neg(u64::try_from(v.abs()).expect("value should be positive"))
        } else {
            Int::Pos(u64::try_from(v).expect("int is not positive"))
        }
    }
}

impl From<i16> for Int {
    fn from(v: i16) -> Self {
        if v < 0 {
            let v = v + 1;
            Int::Neg(u64::try_from(v.abs()).expect("value should be positive"))
        } else {
            Int::Pos(u64::try_from(v).expect("int is not positive"))
        }
    }
}

impl From<i32> for Int {
    fn from(v: i32) -> Self {
        if v < 0 {
            let v = v + 1;
            Int::Neg(u64::try_from(v.abs()).expect("value should be positive"))
        } else {
            Int::Pos(u64::try_from(v).expect("int is not positive"))
        }
    }
}

impl From<i64> for Int {
    fn from(v: i64) -> Self {
        if v < 0 {
            let v = v + 1;
            Int::Neg(u64::try_from(v.abs()).expect("value should be positive"))
        } else {
            Int::Pos(u64::try_from(v).expect("int is not positive"))
        }
    }
}

impl TryFrom<i128> for Int {
    type Error = TryFromIntError;

    fn try_from(v: i128) -> Result<Self, Self::Error> {
        if v < 0 {
            let v = v + 1;
            u64::try_from(v.abs()).map(Int::Neg)
        } else {
            u64::try_from(v).map(Int::Pos)
        }
    }
}

impl TryFrom<isize> for Int {
    type Error = TryFromIntError;

    fn try_from(v: isize) -> Result<Self, Self::Error> {
        if v < 0 {
            let v = v + 1;
            u64::try_from(v.abs()).map(Int::Neg)
        } else {
            u64::try_from(v).map(Int::Pos)
        }
    }
}

impl Encode for Int {
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self {
            Int::Pos(v) => encoder.encode_u64(*v),
            Int::Neg(v) => {
                let v = i128::from(*v);
                let v = -1 - v;
                encoder.encode_i128(v)
            }
        }
    }
}

/// Newtype struct for a float
///
/// `OrderedFloat` is used internally but is not guaranteed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Float(OrderedFloat<f64>);

impl Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl From<f32> for Float {
    fn from(v: f32) -> Self {
        Float(OrderedFloat(f64::from(v)))
    }
}

impl From<f64> for Float {
    fn from(v: f64) -> Self {
        Float(OrderedFloat(v))
    }
}

impl From<Float> for f64 {
    fn from(value: Float) -> Self {
        f64::from(value.0)
    }
}

/// Represents a CBOR item.
///
/// It is useful when the structure of data is unknown.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    /// Integer
    Int(Int),
    /// A byte string
    ByteStr(ByteString),
    /// A UTF-8 string
    TextStr(String),
    /// Array of values
    Array(Vec<Value>),
    /// Map or dictionary of values
    Map(BTreeMap<Value, Value>),
    /// Tag with a number and content
    Tag(Tag<Box<Value>>),
    /// Simple value
    Simple(Simple),
    /// Float value
    Float(Float),
}

impl Value {
    /// If the value is a byte string, returns a reference to the underlying value.
    #[must_use]
    pub fn as_byte_str(&self) -> Option<&ByteString> {
        match self {
            Value::ByteStr(b) => Some(b),
            _ => None,
        }
    }

    /// If the value is a byte string, returns a mutable reference to the underlying value.
    #[must_use]
    pub fn as_byte_str_mut(&mut self) -> Option<&mut ByteString> {
        match self {
            Value::ByteStr(ref mut b) => Some(b),
            _ => None,
        }
    }

    /// If the value is a UTF-8 string, returns a reference to the underlying value.
    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::ByteStr(b) => str::from_utf8(b.as_slice()).ok(),
            _ => None,
        }
    }

    /// If the value is a UTF-8 string, returns a mutable reference to the underlying value.
    #[must_use]
    pub fn as_str_mut(&mut self) -> Option<&mut str> {
        match self {
            Value::ByteStr(ref mut b) => str::from_utf8_mut(b.as_mut_slice()).ok(),
            _ => None,
        }
    }

    /// If the value is an integer, returns a reference to the underlying value.
    #[must_use]
    pub fn as_int(&self) -> Option<&Int> {
        match self {
            Value::Int(n) => Some(n),
            _ => None,
        }
    }

    /// If the value is a positive integer, returns the underlying value.
    #[must_use]
    pub fn as_u64(&self) -> Option<u64> {
        match self {
            Value::Int(Int::Pos(n)) => Some(*n),
            _ => None,
        }
    }

    /// If the value is a negative integer, returns the underlying value.
    #[must_use]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int(Int::Neg(n)) => {
                let v = i128::from(*n);
                let v = -1 - v;
                i64::try_from(v).ok()
            }
            _ => None,
        }
    }

    /// If the value is an array, returns a reference to the underlying value.
    #[must_use]
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Value::Array(ref l) => Some(l),
            _ => None,
        }
    }

    /// If the value is an array, returns a mutable reference to the underlying value.
    #[must_use]
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Value::Array(ref mut l) => Some(l),
            _ => None,
        }
    }

    /// If the value is a map, returns a reference to the underlying value.
    #[must_use]
    pub fn as_map(&self) -> Option<&BTreeMap<Value, Value>> {
        match self {
            Value::Map(d) => Some(d),
            _ => None,
        }
    }

    /// If the value is a map, returns a mutable reference to the underlying value.
    #[must_use]
    pub fn as_map_mut(&mut self) -> Option<&mut BTreeMap<Value, Value>> {
        match self {
            Value::Map(ref mut d) => Some(d),
            _ => None,
        }
    }

    /// Returns true if the value is a byte string.
    #[must_use]
    pub fn is_byte_str(&self) -> bool {
        self.as_byte_str().is_some()
    }

    /// Returns true if the value is a UTF-8 string.
    ///
    /// Note that the value could be a byte string but not a UTF-8 string.
    #[must_use]
    pub fn is_string(&self) -> bool {
        self.as_str().is_some()
    }

    /// Returns true if the value is a an [u64].
    ///
    /// Note that the value could be a [i64].
    #[must_use]
    pub fn is_u64(&self) -> bool {
        self.as_u64().is_some()
    }

    /// Returns true if the value is a an [i64].
    ///
    /// Note that the value could be a [u64].
    #[must_use]
    pub fn is_i64(&self) -> bool {
        self.as_i64().is_some()
    }

    /// Returns true if the value is an array.
    #[must_use]
    pub fn is_array(&self) -> bool {
        self.as_array().is_some()
    }

    /// Returns true if the value is a map.
    #[must_use]
    pub fn is_map(&self) -> bool {
        self.as_map().is_some()
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct DebugByteStr<'a>(&'a ByteString);

        impl fmt::Debug for DebugByteStr<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match core::str::from_utf8(self.0) {
                    Ok(key) => f.debug_tuple("ByteStr").field(&key).finish(),
                    Err(_) => f.debug_tuple("ByteStr").field(&self.0).finish(),
                }
            }
        }

        struct DebugTextStr<'a>(&'a str);

        impl fmt::Debug for DebugTextStr<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_tuple("TextStr").field(&self.0).finish()
            }
        }

        match self {
            Value::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Value::ByteStr(arg0) => fmt::Debug::fmt(&DebugByteStr(arg0), f),
            Value::TextStr(arg0) => fmt::Debug::fmt(&DebugTextStr(arg0), f),
            Value::Array(arg0) => f.debug_tuple("Array").field(arg0).finish(),
            Value::Map(arg0) => f.debug_tuple("Map").field(&arg0).finish(),
            Value::Tag(arg0) => f.debug_tuple("Tag").field(arg0).finish(),
            Value::Simple(arg0) => f.debug_tuple("Simple").field(arg0).finish(),
            Value::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
        }
    }
}

impl From<u8> for Value {
    fn from(v: u8) -> Self {
        Value::Int(Int::Pos(u64::from(v)))
    }
}

impl From<u16> for Value {
    fn from(v: u16) -> Self {
        Value::Int(Int::Pos(u64::from(v)))
    }
}

impl From<u32> for Value {
    fn from(v: u32) -> Self {
        Value::Int(Int::Pos(u64::from(v)))
    }
}

impl From<u64> for Value {
    fn from(v: u64) -> Self {
        Value::Int(Int::Pos(v))
    }
}

impl TryFrom<u128> for Value {
    type Error = TryFromIntError;

    fn try_from(v: u128) -> Result<Self, Self::Error> {
        Int::try_from(v).map(Value::Int)
    }
}

impl TryFrom<usize> for Value {
    type Error = TryFromIntError;

    fn try_from(v: usize) -> Result<Self, Self::Error> {
        Int::try_from(v).map(Value::Int)
    }
}

impl From<i8> for Value {
    fn from(v: i8) -> Self {
        Value::Int(Int::from(v))
    }
}

impl From<i16> for Value {
    fn from(v: i16) -> Self {
        Value::Int(Int::from(v))
    }
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Value::Int(Int::from(v))
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Int(Int::from(v))
    }
}

impl TryFrom<i128> for Value {
    type Error = TryFromIntError;

    fn try_from(v: i128) -> Result<Self, Self::Error> {
        Int::try_from(v).map(Value::Int)
    }
}

impl TryFrom<isize> for Value {
    type Error = TryFromIntError;

    fn try_from(v: isize) -> Result<Self, Self::Error> {
        Int::try_from(v).map(Value::Int)
    }
}

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Value::TextStr(String::from(s)))
    }
}

impl<'a> From<&'a str> for Value {
    fn from(other: &'a str) -> Value {
        Value::TextStr(String::from(other))
    }
}

impl From<String> for Value {
    fn from(other: String) -> Value {
        Value::TextStr(other)
    }
}

impl<'a> From<&'a [u8]> for Value {
    fn from(other: &'a [u8]) -> Value {
        Value::ByteStr(ByteString::from(other))
    }
}

impl From<Vec<u8>> for Value {
    fn from(other: Vec<u8>) -> Value {
        Value::ByteStr(ByteString::from(other))
    }
}

impl From<ByteString> for Value {
    fn from(other: ByteString) -> Value {
        Value::ByteStr(other)
    }
}

impl<K: Into<Value>, V: Into<Value>> From<BTreeMap<K, V>> for Value {
    fn from(other: BTreeMap<K, V>) -> Value {
        Value::Map(
            other
                .into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
    }
}

impl From<Simple> for Value {
    fn from(other: Simple) -> Value {
        Value::Simple(other)
    }
}

impl From<f32> for Value {
    fn from(v: f32) -> Self {
        Value::Float(Float::from(v))
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Float(Float::from(v))
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        if v {
            Value::Simple(Simple::TRUE)
        } else {
            Value::Simple(Simple::FALSE)
        }
    }
}

impl<'de> Decode<'de> for Value {
    #[allow(clippy::too_many_lines)]
    #[inline]
    fn decode<T>(decoder: T) -> Result<Value, T::Error>
    where
        T: Decoder<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {
            type Value = Value;

            #[inline]
            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("any CBOR value")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                if let Ok(v) = u64::try_from(v) {
                    Ok(Value::Int(Int::Pos(v)))
                } else {
                    let v = v + 1;
                    let v = v.abs();
                    let v = u64::try_from(v).expect("value should be positive");
                    Ok(Value::Int(Int::Neg(v)))
                }
            }

            fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                if let Ok(v) = u64::try_from(v) {
                    Ok(Value::Int(Int::Pos(v)))
                } else {
                    let v = v + 1;
                    let v = v.abs();
                    let v = u64::try_from(v).map_err(|_| {
                        crate::decode::Error::invalid_value(
                            Unexpected::NegInt,
                            &"value greater than or equal to -2^64",
                        )
                    })?;
                    Ok(Value::Int(Int::Neg(v)))
                }
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::Int(Int::Pos(v)))
            }

            fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                if let Ok(v) = u64::try_from(v) {
                    Ok(Value::Int(Int::Pos(v)))
                } else {
                    Err(crate::decode::Error::invalid_value(Unexpected::Int, &self))
                }
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::ByteStr(ByteString::from(v)))
            }

            fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                self.visit_bytes(v)
            }

            fn visit_indefinite_len_bytes<A>(self, mut b: A) -> Result<Self::Value, A::Error>
            where
                A: IndefiniteLenItemAccess<'de>,
            {
                struct Bytes(Vec<u8>);

                impl<'de> DecodeSeed<'de> for &mut Bytes {
                    type Value = ();

                    fn decode<D>(self, decoder: D) -> Result<Self::Value, D::Error>
                    where
                        D: Decoder<'de>,
                    {
                        struct BytesVisitor<'a>(&'a mut Vec<u8>);

                        impl Visitor<'_> for BytesVisitor<'_> {
                            type Value = ();

                            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                                f.write_str("a byte string")
                            }

                            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                            where
                                E: crate::decode::Error,
                            {
                                self.0.extend_from_slice(v);
                                Ok(())
                            }
                        }

                        decoder.decode_any(BytesVisitor(&mut self.0))?;

                        Ok(())
                    }
                }

                let mut bytes = Bytes(Vec::new());

                while b.next_chunk_seed(&mut bytes)?.is_some() {}

                Ok(Value::ByteStr(ByteString::from(bytes.0)))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::TextStr(String::from(v)))
            }

            fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                self.visit_str(v)
            }

            fn visit_indefinite_len_str<A>(self, mut a: A) -> Result<Self::Value, A::Error>
            where
                A: IndefiniteLenItemAccess<'de>,
            {
                struct S(String);

                impl<'de> DecodeSeed<'de> for &mut S {
                    type Value = ();

                    fn decode<D>(self, decoder: D) -> Result<Self::Value, D::Error>
                    where
                        D: Decoder<'de>,
                    {
                        struct SVisitor<'a>(&'a mut String);

                        impl Visitor<'_> for SVisitor<'_> {
                            type Value = ();

                            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                                f.write_str("a text string")
                            }

                            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                            where
                                E: crate::decode::Error,
                            {
                                self.0.push_str(v);
                                Ok(())
                            }
                        }

                        decoder.decode_any(SVisitor(&mut self.0))?;

                        Ok(())
                    }
                }

                let mut s = S(String::new());

                while a.next_chunk_seed(&mut s)?.is_some() {}

                Ok(Value::TextStr(s.0))
            }

            fn visit_arr<A>(self, mut v: A) -> Result<Self::Value, A::Error>
            where
                A: crate::decode::ArrAccess<'de>,
            {
                let mut arr = Vec::new();
                if let Some(size_hint) = v.size_hint() {
                    arr.reserve(size_hint);
                }
                while let Some(elem) = v.next_element()? {
                    arr.push(elem);
                }
                Ok(Value::Array(arr))
            }

            fn visit_map<A>(self, mut v: A) -> Result<Self::Value, A::Error>
            where
                A: crate::decode::MapAccess<'de>,
            {
                let mut dict = BTreeMap::new();
                while let Some((key, value)) = v.next_entry()? {
                    dict.insert(key, value);
                }
                Ok(Value::Map(dict))
            }

            fn visit_tag<D>(
                self,
                tag_num: crate::tag::Num,
                decoder: D,
            ) -> Result<Self::Value, D::Error>
            where
                D: Decoder<'de>,
            {
                Ok(Value::Tag(Tag::new(
                    tag_num,
                    Box::new(Value::decode(decoder)?),
                )))
            }

            fn visit_simple<E>(self, v: Simple) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::Simple(v))
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::Float(Float(OrderedFloat(v))))
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(Value::Simple(Simple::NULL))
            }
        }

        decoder.decode_any(ValueVisitor)
    }
}

impl Encode for Value {
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self {
            Value::Int(int) => int.encode(encoder),
            Value::ByteStr(byte_string) => byte_string.encode(encoder),
            Value::TextStr(text_str) => text_str.encode(encoder),
            Value::Array(vec) => vec.encode(encoder),
            Value::Map(btree_map) => btree_map.encode(encoder),
            Value::Tag(tag) => tag.encode(encoder),
            Value::Simple(simple) => simple.encode(encoder),
            Value::Float(f) => f.0.encode(encoder),
        }
    }
}

mod dec;
mod enc;
mod index;

pub use index::Index;

impl Value {
    /// Used to get a reference to a value with an index.
    #[inline]
    pub fn get<I: Index>(&self, index: I) -> Option<&Value> {
        index.index(self)
    }

    /// Used to get a mutable reference to a value with an index.
    #[inline]
    pub fn get_mut<I: Index>(&mut self, index: I) -> Option<&mut Value> {
        index.index_mut(self)
    }
}

/// Decodes an instance of `T` from a [Value].
///
/// # Errors
///
/// Decoding can fail if the data is not valid, if the data cannot cannot be decoded
/// into an instance of `T`, and other IO errors.
#[allow(clippy::module_name_repetitions)]
#[inline]
pub fn from_value<T>(value: Value) -> Result<T, Error>
where
    T: DecodeOwned,
{
    T::decode(value)
}

/// Encodes an instance of `T` into a [Value].
///
/// # Errors
///
/// Encoding can fail if `T`'s implementation of [`Encode`] decides to fail or
/// if `T` contains unsupported types for encoding.
#[allow(clippy::module_name_repetitions)]
#[inline]
pub fn to_value<T>(value: &T) -> Result<Value, Error>
where
    T: ?Sized + Encode,
{
    value.encode(enc::Encoder)
}

#[cfg(test)]
mod tests {
    use crate::{from_slice, tag, value::Value, Simple, Tag};

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
    #[cfg(feature = "std")]
    use std::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};

    #[cfg(any(feature = "alloc", feature = "std"))]
    use crate::{to_vec, ByteString};

    use proptest::prelude::*;

    /// Maximum negative integer
    const NEG_INT_MIN: i128 = -2i128.pow(64);

    prop_compose! {
        fn tag()(num in any::<tag::Num>(), content in any::<u64>()) -> Tag<u64> {
            Tag::new(num, content)
        }
    }

    proptest::proptest! {
        #[test]
        fn test_u8(v in 0..=u8::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }


        #[test]
        fn test_u16(v in 0..=u16::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_u32(v in 0..=u32::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_u64(v in 0..=u64::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_u128(v in 0..=u128::from(u64::MAX)) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.try_into().unwrap());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_usize(v in 0..=usize::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.try_into().unwrap());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_i8(v in i8::MIN..=i8::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_i16(v in i16::MIN..=i16::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_i32(v in i32::MIN..=i32::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_i64(v in i64::MIN..=i64::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_i128(v in NEG_INT_MIN..=i128::from(u64::MAX)) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.try_into().unwrap());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_isize(v in isize::MIN..=isize::MAX) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.try_into().unwrap());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }


        #[cfg(any(feature = "alloc", feature = "std"))]
        #[test]
        fn test_bytes(v in prop::collection::vec(u8::MIN..=u8::MAX, 0..256).prop_map(ByteString::from)) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.clone().into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<ByteString>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_str(v in ".*") {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.clone().into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<&str>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_arr(v in prop::collection::vec(i64::MIN..=i64::MAX, 0..256)) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , Value::Array(v.iter().map(|v| Value::from(*v)).collect::<Vec<_>>()));

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<Vec<i64>>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_map(v in prop::collection::btree_map(".*", ".*", 0..256)) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.clone().into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<BTreeMap<String, String>>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_tag(v in tag()) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            let cloned_tag = Tag::new(v.num(), Box::new(Value::from(*v.content())));
            assert_eq!(decoded_value , Value::Tag(cloned_tag));

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<Tag<u64>>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_simple_value_less_than_24(v in ((0..=23u8).prop_map(Simple::new))) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<Simple>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[test]
        fn test_simple_value_greater_than_31(v in ((32..=u8::MAX).prop_map(Simple::new))) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice::<Simple>(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[allow(clippy::float_cmp)]
        #[test]
        fn test_f32(v in any::<f32>()) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }

        #[allow(clippy::float_cmp)]
        #[test]
        fn test_f64(v in any::<f64>()) {
            let output = to_vec(&v)?;
            let decoded_value = from_slice::<Value>(&output)?;
            assert_eq!(decoded_value , v.into());

            let value_output = to_vec(&decoded_value)?;
            let decoded_v = from_slice(&value_output)?;
            assert_eq!(v , decoded_v);
        }
    }
}
