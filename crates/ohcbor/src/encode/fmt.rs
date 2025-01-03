use core::fmt;

use crate::{
    encode::{Encoder, Error, Impossible},
    Simple,
};

impl Error for fmt::Error {
    fn custom<T: fmt::Display>(_msg: T) -> Self {
        fmt::Error
    }
}

macro_rules! fmt_primitives {
    ($($f:ident: $t:ty,)*) => {
        $(
            fn $f(self, v: $t) -> fmt::Result {
                fmt::Display::fmt(&v, self)
            }
        )*
    };
}

impl Encoder for &mut fmt::Formatter<'_> {
    type Ok = ();
    type Error = fmt::Error;
    type EncodeArr = Impossible<(), fmt::Error>;
    type EncodeMap = Impossible<(), fmt::Error>;

    fmt_primitives! {
        encode_i8: i8,
        encode_i16: i16,
        encode_i32: i32,
        encode_i64: i64,
        encode_i128: i128,
        encode_u8: u8,
        encode_u16: u16,
        encode_u32: u32,
        encode_u64: u64,
        encode_u128: u128,
        encode_f32: f32,
        encode_f64: f64,
        encode_str: &str,
    }

    fn encode_bytes(self, _v: &[u8]) -> fmt::Result {
        Err(fmt::Error)
    }

    fn encode_arr(self, _len: Option<usize>) -> Result<Self::EncodeArr, fmt::Error> {
        Err(fmt::Error)
    }

    fn encode_map(self, _len: Option<usize>) -> Result<Self::EncodeMap, fmt::Error> {
        Err(fmt::Error)
    }

    fn encode_tag<T>(self, _tag_num: u64, _v: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + super::Encode,
    {
        Err(fmt::Error)
    }

    fn encode_simple<I>(self, _v: I) -> Result<Self::Ok, Self::Error>
    where
        I: Into<Simple>,
    {
        Err(fmt::Error)
    }

    fn collect_str<T>(self, value: &T) -> fmt::Result
    where
        T: ?Sized + fmt::Display,
    {
        fmt::Display::fmt(value, self)
    }
}
