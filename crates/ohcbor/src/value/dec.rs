//! Decodes from a [Value].

use core::slice;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{collections::BTreeMap, vec};
#[cfg(feature = "std")]
use std::{collections::BTreeMap, vec};

use crate::{
    decode::{ArrAccess, DecodeSeed, Decoder, Error as _, IntoDecoder, MapAccess, Visitor},
    error::Error,
    value::{Int, Value},
    Tag, NEG_INT_MIN,
};

impl<'de> Decoder<'de> for Value {
    type Error = Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::ByteStr(s) => visitor.visit_bytes(&s.into_vec()),
            Value::Int(n) => match n {
                Int::Pos(v) => visitor.visit_u64(v),
                Int::Neg(v) => visitor.visit_i64(v),
                Int::NegMin => visitor.visit_i128(NEG_INT_MIN),
            },
            Value::Array(a) => {
                let len = a.len();

                let mut decoder = ArrayDecoder {
                    iter: a.into_iter(),
                };
                let seq = visitor.visit_arr(&mut decoder)?;
                if decoder.iter.len() == 0 {
                    Ok(seq)
                } else {
                    Err(Error::invalid_length(
                        len,
                        &"expected more elements to be consumed in array",
                    ))
                }
            }
            Value::Map(d) => {
                let len = d.len();
                let mut decoder = MapDecoder {
                    iter: d.into_iter(),
                    value: None,
                };
                let map = visitor.visit_map(&mut decoder)?;
                if decoder.iter.len() == 0 {
                    Ok(map)
                } else {
                    Err(Error::invalid_length(
                        len,
                        &"expected more elements to be consumed in dict",
                    ))
                }
            }
            Value::TextStr(s) => visitor.visit_str(&s),
            Value::Tag(tag) => {
                let Tag { num, content } = tag;
                visitor.visit_tag(num, content.into_decoder())
            }
            Value::Simple(simple) => visitor.visit_simple(simple),
            Value::Float(float) => visitor.visit_f64(f64::from(float.0)),
        }
    }
}

impl IntoDecoder<'_, Error> for Value {
    type Decoder = Self;

    fn into_decoder(self) -> Self::Decoder {
        self
    }
}

struct ArrayDecoder {
    iter: vec::IntoIter<Value>,
}

impl<'de> ArrAccess<'de> for ArrayDecoder {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DecodeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => seed.decode(value).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

struct MapDecoder {
    iter: <BTreeMap<Value, Value> as IntoIterator>::IntoIter,
    value: Option<Value>,
}

impl<'de> MapAccess<'de> for MapDecoder {
    type Error = Error;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DecodeSeed<'de>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.decode(key).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Error>
    where
        T: DecodeSeed<'de>,
    {
        match self.value.take() {
            Some(value) => seed.decode(value),
            None => Err(Error::custom("value is missing")),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

impl<'de> Decoder<'de> for &'de Value {
    type Error = Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self {
            Value::ByteStr(bytes) => visitor.visit_borrowed_bytes(bytes),
            Value::Int(n) => match n {
                Int::Pos(v) => visitor.visit_u64(*v),
                Int::Neg(v) => visitor.visit_i64(*v),
                Int::NegMin => visitor.visit_i128(NEG_INT_MIN),
            },
            Value::Array(a) => {
                let len = a.len();

                let mut decoder = ArrayRefDecoder { iter: a.iter() };

                let arr = visitor.visit_arr(&mut decoder)?;
                if decoder.iter.len() == 0 {
                    Ok(arr)
                } else {
                    Err(Error::invalid_length(
                        len,
                        &"expected more elements to be consumed in array",
                    ))
                }
            }
            Value::Map(m) => {
                let len = m.len();
                let mut decoder = MapRefDecoder {
                    iter: m.iter(),
                    value: None,
                };

                let map = visitor.visit_map(&mut decoder)?;
                if decoder.iter.len() == 0 {
                    Ok(map)
                } else {
                    Err(Error::invalid_length(
                        len,
                        &"expected more elements to be consumed in map",
                    ))
                }
            }
            Value::TextStr(s) => visitor.visit_borrowed_str(s),
            Value::Tag(tag) => {
                // Would prefer not to clone the content
                visitor.visit_tag(tag.num(), tag.content().clone().into_decoder())
            }
            Value::Simple(simple) => visitor.visit_simple(*simple),
            Value::Float(float) => visitor.visit_f64(f64::from(float.0)),
        }
    }
}

struct ArrayRefDecoder<'a> {
    iter: slice::Iter<'a, Value>,
}

impl<'a> ArrAccess<'a> for ArrayRefDecoder<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DecodeSeed<'a>,
    {
        match self.iter.next() {
            Some(value) => seed.decode(value).map(Some),
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}

struct MapRefDecoder<'a> {
    iter: <&'a BTreeMap<Value, Value> as IntoIterator>::IntoIter,
    value: Option<&'a Value>,
}

impl<'a> MapAccess<'a> for MapRefDecoder<'a> {
    type Error = Error;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Error>
    where
        T: DecodeSeed<'a>,
    {
        match self.iter.next() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.decode(key).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Error>
    where
        T: DecodeSeed<'a>,
    {
        match self.value.take() {
            Some(value) => seed.decode(value),
            None => Err(Error::custom("value is missing")),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        match self.iter.size_hint() {
            (lower, Some(upper)) if lower == upper => Some(upper),
            _ => None,
        }
    }
}
