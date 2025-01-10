//! Encodes into a [Value].

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};
#[cfg(feature = "std")]
use std::{boxed::Box, collections::BTreeMap, string::String, vec::Vec};

use ordered_float::OrderedFloat;

use super::{Float, Int, Value};
use crate::{
    encode::{self, Error as _},
    error::Error,
    ByteString, Simple, Tag, NEG_INT_MIN,
};

pub(super) struct Encoder;

impl encode::Encoder for Encoder {
    type Ok = Value;
    type Error = Error;

    type EncodeArr = EncodeArr;

    type EncodeMap = EncodeMap;

    fn encode_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        self.encode_i64(i64::from(v))
    }

    fn encode_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        self.encode_i64(i64::from(v))
    }

    fn encode_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        self.encode_i64(i64::from(v))
    }

    fn encode_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        if v < 0 {
            Ok(Value::Int(Int::Neg(v)))
        } else {
            Ok(Value::Int(Int::Pos(
                u64::try_from(v).expect("int is not positive"),
            )))
        }
    }

    fn encode_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        if v == NEG_INT_MIN {
            Ok(Value::Int(Int::NegMin))
        } else if let Ok(v) = u64::try_from(v) {
            Ok(Value::Int(Int::Pos(v)))
        } else if let Ok(v) = i64::try_from(v) {
            Ok(Value::Int(Int::Neg(v)))
        } else {
            Err(Self::Error::invalid_value(v))
        }
    }

    fn encode_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        self.encode_u64(u64::from(v))
    }

    fn encode_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        self.encode_u64(u64::from(v))
    }

    fn encode_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        self.encode_u64(u64::from(v))
    }

    fn encode_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Int(Int::Pos(v)))
    }

    fn encode_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        if let Ok(v) = u64::try_from(v) {
            Ok(Value::Int(Int::Pos(v)))
        } else {
            Err(Self::Error::invalid_value(v))
        }
    }

    fn encode_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::TextStr(String::from(v)))
    }

    fn encode_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Ok(Value::ByteStr(ByteString::from(v)))
    }

    fn encode_arr(self, len: Option<u64>) -> Result<Self::EncodeArr, Self::Error> {
        Ok(EncodeArr {
            arr: Vec::with_capacity(
                usize::try_from(len.unwrap_or(0))
                    .map_err(|_| Self::Error::custom("len is greater than usize::MAX"))?,
            ),
        })
    }

    fn encode_map(self, len: Option<u64>) -> Result<Self::EncodeMap, Self::Error> {
        let _ = usize::try_from(len.unwrap_or(0))
            .map_err(|_| Self::Error::custom("len is greater than usize::MAX"))?;
        Ok(EncodeMap {
            map: BTreeMap::new(),
            current_key: None,
        })
    }

    fn encode_tag<T>(self, tag_num: u64, v: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + encode::Encode,
    {
        let v = super::to_value(v)?;
        Ok(Value::Tag(Tag::new(tag_num, Box::new(v))))
    }

    fn encode_simple<I>(self, v: I) -> Result<Self::Ok, Self::Error>
    where
        I: Into<Simple>,
    {
        Ok(Value::Simple(v.into()))
    }

    fn encode_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        self.encode_f64(f64::from(v))
    }

    fn encode_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Float(Float(OrderedFloat(v))))
    }

    fn encode_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Simple(Simple::NULL))
    }
}

pub(super) struct EncodeArr {
    arr: Vec<Value>,
}

impl encode::EncodeArr for EncodeArr {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn encode_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + encode::Encode,
    {
        self.arr.push(super::to_value(value)?);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Array(self.arr))
    }
}

pub(super) struct EncodeMap {
    map: BTreeMap<Value, Value>,
    current_key: Option<Value>,
}

impl encode::EncodeMap for EncodeMap {
    type Ok = Value;
    type Error = Error;

    #[inline]
    fn encode_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + encode::Encode,
    {
        if self.current_key.is_some() {
            return Err(Error::custom("key without value"));
        }
        self.current_key = Some(key.encode(Encoder)?);
        Ok(())
    }

    #[inline]
    fn encode_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + encode::Encode,
    {
        let key = self
            .current_key
            .take()
            .ok_or_else(|| Error::custom("value without key"))?;
        let value = super::to_value(value)?;
        self.map.insert(key, value);
        Ok(())
    }

    #[inline]
    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::Map(self.map))
    }
}
