use core::{
    cell::{Cell, RefCell},
    cmp::Reverse,
    fmt,
    marker::PhantomData,
    num::{self, NonZero, Saturating},
};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    collections::{BTreeMap, BTreeSet, BinaryHeap, LinkedList, VecDeque},
    string::String,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    hash::{BuildHasher, Hash},
    string::String,
    sync::{Mutex, RwLock},
    vec::Vec,
};

#[cfg(any(feature = "alloc", feature = "std"))]
use crate::decode::{size_hint, MapAccess};
use crate::{
    decode::{
        seed::InPlaceSeed,
        value::{
            ArrAccessDecoder, BorrowedBytesDecoder, BorrowedStrDecoder,
            IndefiniteLenBytesAccessDecoder, IndefiniteLenStringAccessDecoder, MapAccessDecoder,
            TagDecoder,
        },
        ArrAccess, Decode, DecodeSeed, Decoder, Error, IndefiniteLenItemAccess, IntoDecoder,
        Unexpected, Visitor,
    },
    Simple,
};

impl<'de> Decode<'de> for bool {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct BoolVisitor;

        impl Visitor<'_> for BoolVisitor {
            type Value = bool;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a boolean")
            }

            fn visit_simple<E>(self, v: Simple) -> Result<Self::Value, E>
            where
                E: Error,
            {
                let Some(v) = v.as_bool() else {
                    return Err(Error::invalid_value(Unexpected::from(v), &self));
                };

                if v {
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
        }

        decoder.decode_any(BoolVisitor)
    }
}

macro_rules! impl_decode_num {
($primitive:ident, $nonzero:ident, $($method:ident!($($val:ident : $visit:ident)*);)*) => {
    impl_decode_num!($primitive, $($method!($($val : $visit)*);)*);

    impl<'de> Decode<'de> for num::$nonzero {
        fn decode<D>(decoder: D) -> core::result::Result<Self, D::Error>
        where
            D: Decoder<'de>,
        {
            struct NonZeroVisitor;

            impl<'de> Visitor<'de> for NonZeroVisitor {
                type Value = num::$nonzero;

                fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str(concat!("a nonzero ", stringify!($primitive)))
                }

                $($($method!(nonzero $primitive $val : $visit);)*)*
            }

            decoder.decode_any(NonZeroVisitor)
        }
    }

    impl<'de> Decode<'de> for Saturating<$primitive> {
        fn decode<D>(decoder: D) -> core::result::Result<Self, D::Error>
        where
            D: Decoder<'de>,
        {
            struct SaturatingVisitor;

            impl<'de> Visitor<'de> for SaturatingVisitor {
                type Value = Saturating<$primitive>;

                fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str("integer with support for saturating semantics")
                }

                $($($method!(saturating $primitive $val : $visit);)*)*
            }

            decoder.decode_any(SaturatingVisitor)
        }
    }
};

($primitive:ident, $($method:ident!($($val:ident : $visit:ident)*);)*) => {
    impl<'de> Decode<'de> for $primitive {
        #[inline]
        fn decode<D>(decoder: D) -> core::result::Result<Self, D::Error>
        where
            D: Decoder<'de>,
        {
            struct PrimitiveVisitor;

            impl<'de> Visitor<'de> for PrimitiveVisitor {
                type Value = $primitive;

                fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str(stringify!($primitive))
                }

                $($($method!($val : $visit);)*)*
            }

            decoder.decode_any(PrimitiveVisitor)
        }
    }
};
}

macro_rules! num_self {
    ($ty:ident : $visit:ident) => {
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(v)
        }
    };

    (nonzero $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            if let Some(nonzero) = Self::Value::new(v) {
                Ok(nonzero)
            } else {
                Err(Error::invalid_value(Unexpected::from(v), &self))
            }
        }
    };

    (saturating $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(Saturating(v))
        }
    };
}

macro_rules! num_as_self {
    ($ty:ident : $visit:ident) => {
        #[allow(
            clippy::cast_precision_loss,
            clippy::cast_possible_truncation,
            clippy::cast_lossless
        )]
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(v as Self::Value)
        }
    };

    (nonzero $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            if let Some(nonzero) = Self::Value::new($primitive::from(v)) {
                Ok(nonzero)
            } else {
                Err(Error::invalid_value(Unexpected::from(0), &self))
            }
        }
    };

    (saturating $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(Saturating($primitive::from(v)))
        }
    };
}

macro_rules! num_as_copysign_self {
    ($ty:ident : $visit:ident) => {
        #[allow(
            clippy::cast_precision_loss,
            clippy::cast_possible_truncation,
            clippy::cast_lossless
        )]
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            #[cfg(not(feature = "std"))]
            {
                Ok(v as Self::Value)
            }

            #[cfg(feature = "std")]
            {
                // Preserve sign of NaN. The `as` produces a nondeterministic sign.
                let sign = if v.is_sign_positive() { 1.0 } else { -1.0 };
                Ok((v as Self::Value).copysign(sign))
            }
        }
    };
}

macro_rules! int_to_int {
    ($ty:ident : $visit:ident) => {
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Self::Value::try_from(v).map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (nonzero $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            NonZero::try_from(v)
                .and_then(Self::Value::try_from)
                .map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (saturating $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            if v < 0 {
                Ok(Saturating(
                    $primitive::try_from(v).unwrap_or($primitive::MIN),
                ))
            } else {
                Ok(Saturating(
                    $primitive::try_from(v).unwrap_or($primitive::MAX),
                ))
            }
        }
    };
}

macro_rules! int_to_uint {
    ($ty:ident : $visit:ident) => {
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Self::Value::try_from(v).map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (nonzero $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            NonZero::try_from(v)
                .and_then(Self::Value::try_from)
                .map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (saturating $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            if v < 0 {
                Ok(Saturating(0))
            } else {
                Ok(Saturating(
                    $primitive::try_from(v).unwrap_or($primitive::MAX),
                ))
            }
        }
    };
}

macro_rules! uint_to_self {
    ($ty:ident : $visit:ident) => {
        #[inline]
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Self::Value::try_from(v).map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (nonzero $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            let nonzero_v = NonZero::try_from(v)
                .map_err(|_| Error::invalid_value(Unexpected::from(v), &self))?;
            Self::Value::try_from(nonzero_v)
                .map_err(|_| Error::invalid_value(Unexpected::from(v), &self))
        }
    };

    (saturating $primitive:ident $ty:ident : $visit:ident) => {
        fn $visit<E>(self, v: $ty) -> Result<Self::Value, E>
        where
            E: Error,
        {
            Ok(Saturating(
                $primitive::try_from(v).unwrap_or($primitive::MAX),
            ))
        }
    };
}

impl_decode_num! {
    u8, NonZeroU8,
    num_self!(u8:visit_u8);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    u16, NonZeroU16,
    num_self!(u16:visit_u16);
    num_as_self!(u8:visit_u8);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    u32, NonZeroU32,
    num_self!(u32:visit_u32);
    num_as_self!(u8:visit_u8 u16:visit_u16);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    u64, NonZeroU64,
    num_self!(u64:visit_u64);
    num_as_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u128:visit_u128);
}

impl_decode_num! {
    u128, NonZeroU128,
    num_self!(u128:visit_u128);
    num_as_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
}

impl_decode_num! {
    usize, NonZeroUsize,
    num_as_self!(u8:visit_u8 u16:visit_u16);
    int_to_uint!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    i8, NonZeroI8,
    num_self!(i8:visit_i8);
    int_to_int!(i16:visit_i16 i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    i16, NonZeroI16,
    num_self!(i16:visit_i16);
    num_as_self!(i8:visit_i8);
    int_to_int!(i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    i32, NonZeroI32,
    num_self!(i32:visit_i32);
    num_as_self!(i8:visit_i8 i16:visit_i16);
    int_to_int!(i64:visit_i64 i128:visit_i128);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    i64, NonZeroI64,
    num_self!(i64:visit_i64);
    num_as_self!(i8:visit_i8 i16:visit_i16 i32:visit_i32);
    int_to_int!(i128:visit_i128);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    i128, NonZeroI128,
    num_self!(i128:visit_i128);
    num_as_self!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    isize, NonZeroIsize,
    num_as_self!(i8:visit_i8 i16:visit_i16);
    int_to_int!(i32:visit_i32 i64:visit_i64 i128:visit_i128);
    uint_to_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64 u128:visit_u128);
}

impl_decode_num! {
    f32,
    num_self!(f32:visit_f32);
    num_as_copysign_self!(f64:visit_f64);
    num_as_self!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64);
    num_as_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64);
}

impl_decode_num! {
    f64,
    num_self!(f64:visit_f64);
    num_as_copysign_self!(f32:visit_f32);
    num_as_self!(i8:visit_i8 i16:visit_i16 i32:visit_i32 i64:visit_i64);
    num_as_self!(u8:visit_u8 u16:visit_u16 u32:visit_u32 u64:visit_u64);
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de> Decode<'de> for String {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct StringVisitor;

        impl<'de> Visitor<'de> for StringVisitor {
            type Value = String;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a string")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                core::str::from_utf8(v)
                    .map(ToOwned::to_owned)
                    .map_err(|_| Error::invalid_value(Unexpected::Bytes, &self))
            }

            fn visit_indefinite_len_bytes<A>(self, mut a: A) -> Result<Self::Value, A::Error>
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
                                E: Error,
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

                while a.next_chunk_seed(&mut bytes)?.is_some() {}

                String::from_utf8(bytes.0)
                    .map_err(|_| Error::invalid_value(Unexpected::Bytes, &self))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(v.to_owned())
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
                                E: Error,
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

                Ok(s.0)
            }
        }

        decoder.decode_any(StringVisitor)
    }

    #[allow(clippy::too_many_lines)]
    fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Decoder<'de>,
    {
        struct StringInPlaceVisitor<'a>(&'a mut String);

        impl<'de> Visitor<'de> for StringInPlaceVisitor<'_> {
            type Value = ();

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a string")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                match core::str::from_utf8(v) {
                    Ok(v) => {
                        self.0.clear();
                        self.0.push_str(v);
                        Ok(())
                    }
                    Err(_) => Err(Error::invalid_value(Unexpected::Bytes, &self)),
                }
            }

            fn visit_indefinite_len_bytes<A>(self, mut a: A) -> Result<Self::Value, A::Error>
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
                                E: Error,
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

                while a.next_chunk_seed(&mut bytes)?.is_some() {}

                let s = String::from_utf8(bytes.0)
                    .map_err(|_| Error::invalid_value(Unexpected::Bytes, &self))?;

                self.0.clear();
                self.0.push_str(&s);
                Ok(())
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                self.0.clear();
                self.0.push_str(v);
                Ok(())
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
                                E: Error,
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

                self.0.clear();
                self.0.push_str(&s.0);
                Ok(())
            }
        }

        decoder.decode_any(StringInPlaceVisitor(place))
    }
}

impl<'de: 'a, 'a> Decode<'de> for &'a str {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct StrVisitor;

        impl<'a> Visitor<'a> for StrVisitor {
            type Value = &'a str;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a borrowed string")
            }

            fn visit_borrowed_bytes<E>(self, v: &'a [u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                core::str::from_utf8(v).map_err(|_| Error::invalid_value(Unexpected::Bytes, &self))
            }

            fn visit_borrowed_str<E>(self, v: &'a str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(v)
            }
        }

        decoder.decode_any(StrVisitor)
    }
}

impl<'de: 'a, 'a> Decode<'de> for &'a [u8] {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct BytesVisitor;

        impl<'a> Visitor<'a> for BytesVisitor {
            type Value = &'a [u8];

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a borrowed byte array")
            }

            fn visit_borrowed_bytes<E>(self, v: &'a [u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(v)
            }

            fn visit_borrowed_str<E>(self, v: &'a str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(v.as_bytes())
            }
        }

        decoder.decode_any(BytesVisitor)
    }
}

macro_rules! forwarded_impl {
    (
        $(#[$attr:meta])*
        ($($id:ident),*), $ty:ty, $func:expr
    ) => {
        $(#[$attr])*
        impl<'de $(, $id : Decode<'de>,)*> Decode<'de> for $ty {
            fn decode<D>(decoder: D) -> Result<Self, D::Error>
            where
                D: Decoder<'de>,
            {
                Decode::decode(decoder).map($func)
            }
        }
    }
}

forwarded_impl! {
    (T), Reverse<T>, Reverse
}

impl<'de, T> Decode<'de> for Option<T>
where
    T: Decode<'de>,
{
    #[allow(clippy::too_many_lines)]
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct OptionsVisitor<T> {
            ty: PhantomData<T>,
        }

        impl<'a, T> Visitor<'a> for OptionsVisitor<T>
        where
            T: Decode<'a>,
        {
            type Value = Option<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("an option")
            }

            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_borrowed_bytes<E>(self, v: &'a [u8]) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(BorrowedBytesDecoder::new(v)).map(Some)
            }

            fn visit_indefinite_len_bytes<A>(self, a: A) -> Result<Self::Value, A::Error>
            where
                A: IndefiniteLenItemAccess<'a>,
            {
                T::decode(IndefiniteLenBytesAccessDecoder::new(a)).map(Some)
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_borrowed_str<E>(self, v: &'a str) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(BorrowedStrDecoder::new(v)).map(Some)
            }

            fn visit_indefinite_len_str<A>(self, a: A) -> Result<Self::Value, A::Error>
            where
                A: IndefiniteLenItemAccess<'a>,
            {
                T::decode(IndefiniteLenStringAccessDecoder::new(a)).map(Some)
            }

            fn visit_arr<A>(self, arr: A) -> Result<Self::Value, A::Error>
            where
                A: ArrAccess<'a>,
            {
                T::decode(ArrAccessDecoder::new(arr)).map(Some)
            }

            fn visit_map<A>(self, map: A) -> Result<Self::Value, A::Error>
            where
                A: MapAccess<'a>,
            {
                T::decode(MapAccessDecoder::new(map)).map(Some)
            }

            fn visit_tag<D>(self, tag_num: u64, decoder: D) -> Result<Self::Value, D::Error>
            where
                D: Decoder<'a>,
            {
                T::decode(TagDecoder::new(tag_num, decoder)).map(Some)
            }

            fn visit_simple<E>(self, v: Simple) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_f64<E>(self, v: f64) -> Result<Self::Value, E>
            where
                E: Error,
            {
                T::decode(v.into_decoder()).map(Some)
            }

            fn visit_none<E>(self) -> Result<Self::Value, E>
            where
                E: Error,
            {
                Ok(None)
            }
        }

        decoder.decode_any(OptionsVisitor { ty: PhantomData })
    }
}

macro_rules! arr_impl {
  (
      $(#[$attr:meta])*
      $ty:ident <T $(: $tbound1:ident $(+ $tbound2:ident)*)* $(, $typaram:ident : $bound1:ident $(+ $bound2:ident)*)*>,
      $access:ident,
      $clear:expr,
      $with_capacity:expr,
      $reserve:expr,
      $insert:expr
  ) => {
      $(#[$attr])*
      impl<'de, T $(, $typaram)*> Decode<'de> for $ty<T $(, $typaram)*>
      where
          T: Decode<'de> $(+ $tbound1 $(+ $tbound2)*)*,
          $($typaram: $bound1 $(+ $bound2)*,)*
      {
          fn decode<D>(decoder: D) -> Result<Self, D::Error>
          where
              D: Decoder<'de>,
          {
              struct SeqVisitor<T $(, $typaram)*> {
                  marker: PhantomData<$ty<T $(, $typaram)*>>,
              }

              impl<'de, T $(, $typaram)*> Visitor<'de> for SeqVisitor<T $(, $typaram)*>
              where
                  T: Decode<'de> $(+ $tbound1 $(+ $tbound2)*)*,
                  $($typaram: $bound1 $(+ $bound2)*,)*
              {
                  type Value = $ty<T $(, $typaram)*>;

                  fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                      f.write_str("an array")
                  }

                  #[inline]
                  fn visit_arr<A>(self, mut $access: A) -> Result<Self::Value, A::Error>
                  where
                      A: ArrAccess<'de>,
                  {
                      let mut values = $with_capacity;

                      while let Some(value) = $access.next_element()? {
                          $insert(&mut values, value);
                      }

                      Ok(values)
                  }
              }

              let visitor = SeqVisitor { marker: PhantomData };
              decoder.decode_any(visitor)
          }

          fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
          where
              D: Decoder<'de>,
          {
              struct SeqInPlaceVisitor<'a, T $(, $typaram)*>(&'a mut $ty<T $(, $typaram)*>);

              impl<'a, 'de, T $(, $typaram)*> Visitor<'de> for SeqInPlaceVisitor<'a, T $(, $typaram)*>
              where
                  T: Decode<'de> $(+ $tbound1 $(+ $tbound2)*)*,
                  $($typaram: $bound1 $(+ $bound2)*,)*
              {
                  type Value = ();

                  fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                      f.write_str("an array")
                  }

                  #[inline]
                  fn visit_arr<A>(mut self, mut $access: A) -> Result<Self::Value, A::Error>
                  where
                      A: ArrAccess<'de>,
                  {
                      $clear(&mut self.0);
                      $reserve(&mut self.0, size_hint::cautious::<T>($access.size_hint()));

                      // FIXME: try to overwrite old values here? (Vec, VecDeque, LinkedList)
                      while let Some(value) = $access.next_element()? {
                          $insert(&mut self.0, value);
                      }

                      Ok(())
                  }
              }

              decoder.decode_any(SeqInPlaceVisitor(place))
          }
      }
  }
}

// Dummy impl of reserve
#[cfg(any(feature = "std", feature = "alloc"))]
fn nop_reserve<T>(_seq: T, _n: usize) {}

arr_impl!(
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BinaryHeap<T: Ord>,
    arr,
    BinaryHeap::clear,
    BinaryHeap::with_capacity(size_hint::cautious::<T>(arr.size_hint())),
    BinaryHeap::reserve,
    BinaryHeap::push
);

arr_impl!(
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BTreeSet<T: Eq + Ord>,
    arr,
    BTreeSet::clear,
    BTreeSet::new(),
    nop_reserve,
    BTreeSet::insert
);

arr_impl!(
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    LinkedList<T>,
    arr,
    LinkedList::clear,
    LinkedList::new(),
    nop_reserve,
    LinkedList::push_back
);

arr_impl!(
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    HashSet<T: Eq + Hash, S: BuildHasher + Default>,
    arr,
    HashSet::clear,
    HashSet::with_capacity_and_hasher(size_hint::cautious::<T>(arr.size_hint()), S::default()),
    HashSet::reserve,
    HashSet::insert
);

arr_impl!(
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    VecDeque<T>,
    arr,
    VecDeque::clear,
    VecDeque::with_capacity(size_hint::cautious::<T>(arr.size_hint())),
    VecDeque::reserve,
    VecDeque::push_back
);

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, T> Decode<'de> for Vec<T>
where
    T: Decode<'de>,
{
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct VecVisitor<T> {
            marker: PhantomData<T>,
        }

        impl<'de, T> Visitor<'de> for VecVisitor<T>
        where
            T: Decode<'de>,
        {
            type Value = Vec<T>;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("an array")
            }

            fn visit_arr<A>(self, mut arr: A) -> Result<Self::Value, A::Error>
            where
                A: ArrAccess<'de>,
            {
                let capacity = size_hint::cautious::<T>(arr.size_hint());
                let mut values = Vec::<T>::with_capacity(capacity);

                while let Some(value) = arr.next_element()? {
                    values.push(value);
                }

                Ok(values)
            }
        }

        let visitor = VecVisitor {
            marker: PhantomData,
        };

        decoder.decode_any(visitor)
    }

    fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Decoder<'de>,
    {
        struct VecInPlaceVisitor<'a, T>(&'a mut Vec<T>);

        impl<'de, T> Visitor<'de> for VecInPlaceVisitor<'_, T>
        where
            T: Decode<'de>,
        {
            type Value = ();

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("an array")
            }

            fn visit_arr<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: ArrAccess<'de>,
            {
                let hint = size_hint::cautious::<T>(seq.size_hint());
                if let Some(additional) = hint.checked_sub(self.0.len()) {
                    self.0.reserve(additional);
                }

                for i in 0..self.0.len() {
                    let next = {
                        let next_place = InPlaceSeed(&mut self.0[i]);
                        seq.next_element_seed(next_place)?
                    };
                    if next.is_none() {
                        self.0.truncate(i);
                        return Ok(());
                    }
                }

                while let Some(value) = seq.next_element()? {
                    self.0.push(value);
                }

                Ok(())
            }
        }

        decoder.decode_any(VecInPlaceVisitor(place))
    }
}

struct ArrayVisitor<A> {
    marker: PhantomData<A>,
}
struct ArrayInPlaceVisitor<'a, A>(&'a mut A);

impl<A> ArrayVisitor<A> {
    fn new() -> Self {
        ArrayVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de, T> Visitor<'de> for ArrayVisitor<[T; 0]> {
    type Value = [T; 0];

    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("an empty array")
    }

    #[inline]
    fn visit_arr<A>(self, _: A) -> Result<Self::Value, A::Error>
    where
        A: ArrAccess<'de>,
    {
        Ok([])
    }
}

// Does not require T: Decode<'de>.
impl<'de, T> Decode<'de> for [T; 0] {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        decoder.decode_any(ArrayVisitor::<[T; 0]>::new())
    }
}

macro_rules! array_impls {
  ($($len:expr => ($($n:tt)+))+) => {
      $(
          impl<'de, T> Visitor<'de> for ArrayVisitor<[T; $len]>
          where
              T: Decode<'de>,
          {
              type Value = [T; $len];

              fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                  f.write_str(concat!("an array of length ", $len))
              }

              #[inline]
              fn visit_arr<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
              where
                  A: ArrAccess<'de>,
              {
                  Ok([$(
                      match seq.next_element()? {
                          Some(val) => val,
                          None => return Err(Error::invalid_length($n, &self)),
                      }
                  ),+])
              }
          }

          impl<'a, 'de, T> Visitor<'de> for ArrayInPlaceVisitor<'a, [T; $len]>
          where
              T: Decode<'de>,
          {
              type Value = ();

              fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                  f.write_str(concat!("an array of length ", $len))
              }

              #[inline]
              fn visit_arr<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
              where
                  A: ArrAccess<'de>,
              {
                  let mut fail_idx = None;
                  for (idx, dest) in self.0[..].iter_mut().enumerate() {
                      if seq.next_element_seed(InPlaceSeed(dest))?.is_none() {
                          fail_idx = Some(idx);
                          break;
                      }
                  }
                  if let Some(idx) = fail_idx {
                      return Err(A::Error::invalid_length(idx, &self));
                  }
                  Ok(())
              }
          }

          impl<'de, T> Decode<'de> for [T; $len]
          where
              T: Decode<'de>,
          {
              fn decode<D>(decoder: D) -> Result<Self, D::Error>
              where
                  D: Decoder<'de>,
              {
                  decoder.decode_any(ArrayVisitor::<[T; $len]>::new())
              }

              fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
              where
                  D: Decoder<'de>,
              {
                  decoder.decode_any(ArrayInPlaceVisitor(place))
              }
          }
      )+
  }
}

array_impls! {
  1 => (0)
  2 => (0 1)
  3 => (0 1 2)
  4 => (0 1 2 3)
  5 => (0 1 2 3 4)
  6 => (0 1 2 3 4 5)
  7 => (0 1 2 3 4 5 6)
  8 => (0 1 2 3 4 5 6 7)
  9 => (0 1 2 3 4 5 6 7 8)
  10 => (0 1 2 3 4 5 6 7 8 9)
  11 => (0 1 2 3 4 5 6 7 8 9 10)
  12 => (0 1 2 3 4 5 6 7 8 9 10 11)
  13 => (0 1 2 3 4 5 6 7 8 9 10 11 12)
  14 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13)
  15 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)
  16 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
  17 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
  18 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17)
  19 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
  20 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
  21 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
  22 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
  23 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)
  24 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23)
  25 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24)
  26 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
  27 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26)
  28 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
  29 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28)
  30 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
  31 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
  32 => (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
}

macro_rules! tuple_impls {
  ($($len:tt => ($($n:tt $name:ident)+))+) => {
      $(
          #[cfg_attr(docsrs, doc(hidden))]
          impl<'de, $($name),+> Decode<'de> for ($($name,)+)
          where
              $($name: Decode<'de>,)+
          {
              tuple_impl_body!($len => ($($n $name)+));
          }
      )+
  };
}

macro_rules! tuple_impl_body {
  ($len:tt => ($($n:tt $name:ident)+)) => {
      #[inline]
      fn decode<D>(decoder: D) -> Result<Self, D::Error>
      where
          D: Decoder<'de>,
      {
          struct TupleVisitor<$($name,)+> {
              marker: PhantomData<($($name,)+)>,
          }

          impl<'de, $($name: Decode<'de>),+> Visitor<'de> for TupleVisitor<$($name,)+> {
              type Value = ($($name,)+);

              fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                  f.write_str(concat!("a tuple of size ", $len))
              }

              #[inline]
              #[allow(non_snake_case)]
              fn visit_arr<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
              where
                  A: ArrAccess<'de>,
              {
                  $(
                      let Some($name) = seq.next_element()? else {
                          return Err(Error::invalid_length($n, &self));
                      };
                  )+

                  Ok(($($name,)+))
              }
          }

          decoder.decode_any(TupleVisitor { marker: PhantomData })
      }

      #[inline]
      fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
      where
          D: Decoder<'de>,
      {
          struct TupleInPlaceVisitor<'a, $($name,)+>(&'a mut ($($name,)+));

          impl<'a, 'de, $($name: Decode<'de>),+> Visitor<'de> for TupleInPlaceVisitor<'a, $($name,)+> {
              type Value = ();

              fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                  f.write_str(concat!("a tuple of size ", $len))
              }

              #[inline]
              #[allow(non_snake_case)]
              fn visit_arr<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
              where
                  A: ArrAccess<'de>,
              {
                  $(
                      if seq.next_element_seed(InPlaceSeed(&mut (self.0).$n))?.is_none() {
                          return Err(Error::invalid_length($n, &self));
                      }
                  )+

                  Ok(())
              }
          }

          decoder.decode_any(TupleInPlaceVisitor(place))
      }
  };
}

#[cfg_attr(docsrs, doc(fake_variadic))]
#[cfg_attr(
    docsrs,
    doc = "This trait is implemented for tuples up to 16 items long."
)]
impl<'de, T> Decode<'de> for (T,)
where
    T: Decode<'de>,
{
    tuple_impl_body!(1 => (0 T));
}

tuple_impls! {
  2  => (0 T0 1 T1)
  3  => (0 T0 1 T1 2 T2)
  4  => (0 T0 1 T1 2 T2 3 T3)
  5  => (0 T0 1 T1 2 T2 3 T3 4 T4)
  6  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
  7  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
  8  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
  9  => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
  10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
  11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
  12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
  13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
  14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
  15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
  16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}

macro_rules! map_impl {
  (
      $(#[$attr:meta])*
      $ty:ident <K $(: $kbound1:ident $(+ $kbound2:ident)*)*, V $(, $typaram:ident : $bound1:ident $(+ $bound2:ident)*)*>,
      $access:ident,
      $with_capacity:expr,
  ) => {
      $(#[$attr])*
      impl<'de, K, V $(, $typaram)*> Decode<'de> for $ty<K, V $(, $typaram)*>
      where
          K: Decode<'de> $(+ $kbound1 $(+ $kbound2)*)*,
          V: Decode<'de>,
          $($typaram: $bound1 $(+ $bound2)*),*
      {
          fn decode<D>(decoder: D) -> Result<Self, D::Error>
          where
              D: Decoder<'de>,
          {
              struct MapVisitor<K, V $(, $typaram)*> {
                  marker: PhantomData<$ty<K, V $(, $typaram)*>>,
              }

              impl<'de, K, V $(, $typaram)*> Visitor<'de> for MapVisitor<K, V $(, $typaram)*>
              where
                  K: Decode<'de> $(+ $kbound1 $(+ $kbound2)*)*,
                  V: Decode<'de>,
                  $($typaram: $bound1 $(+ $bound2)*),*
              {
                  type Value = $ty<K, V $(, $typaram)*>;

                  fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                      f.write_str("a map")
                  }

                  #[inline]
                  fn visit_map<A>(self, mut $access: A) -> Result<Self::Value, A::Error>
                  where
                      A: MapAccess<'de>,
                  {
                      let mut values = $with_capacity;

                      while let Some((key, value)) = $access.next_entry()? {
                          values.insert(key, value);
                      }

                      Ok(values)
                  }
              }

              let visitor = MapVisitor { marker: PhantomData };
              decoder.decode_any(visitor)
          }
      }
  }
}

map_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BTreeMap<K: Ord, V>,
    map,
    BTreeMap::new(),
}

map_impl! {
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    HashMap<K: Eq + Hash, V, S: BuildHasher + Default>,
    map,
    HashMap::with_capacity_and_hasher(size_hint::cautious::<(K, V)>(map.size_hint()), S::default()),
}

forwarded_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    (T), Box<T>, Box::new
}

forwarded_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    (T), Box<[T]>, Vec::into_boxed_slice
}

forwarded_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    (), Box<str>, String::into_boxed_str
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, T> Decode<'de> for Cow<'_, T>
where
    T: ?Sized + ToOwned,
    T::Owned: Decode<'de>,
{
    #[inline]
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        T::Owned::decode(decoder).map(Cow::Owned)
    }
}

impl<'de, T> Decode<'de> for Cell<T>
where
    T: Decode<'de> + Copy,
{
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        T::decode(decoder).map(Cell::new)
    }
}

forwarded_impl! {
    (T), RefCell<T>, RefCell::new
}

forwarded_impl! {
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    (T), Mutex<T>, Mutex::new
}

forwarded_impl! {
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    (T), RwLock<T>, RwLock::new
}
