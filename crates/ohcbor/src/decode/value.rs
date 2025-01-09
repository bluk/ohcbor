//! Building blocks for deserializing basic values using the `IntoDecoder`
//! trait.

use core::{fmt, iter, marker::PhantomData};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{
    borrow::Cow,
    boxed::Box,
    collections::{BTreeMap, BTreeSet},
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    borrow::Cow,
    boxed::Box,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    hash::{BuildHasher, Hash},
    string::{String, ToString},
    vec::Vec,
};

use self::private::{First, Second};
use crate::{
    decode::{
        self, size_hint, ArrAccess, DecodeSeed, Decoder, Expected, IntoDecoder, MapAccess, Visitor,
    },
    simple::{SIMPLE_VALUE_FALSE, SIMPLE_VALUE_TRUE},
    Simple,
};

use super::IndefiniteLenItemAccess;

////////////////////////////////////////////////////////////////////////////////

// For structs that contain a PhantomData. We do not want the trait
// bound `E: Clone` inferred by derive(Clone).
macro_rules! impl_copy_clone {
    ($ty:ident $(<$lifetime:tt>)*) => {
        impl<$($lifetime,)* E> Copy for $ty<$($lifetime,)* E> {}

        impl<$($lifetime,)* E> Clone for $ty<$($lifetime,)* E> {
            fn clone(&self) -> Self {
                *self
            }
        }
    };
}

////////////////////////////////////////////////////////////////////////////////

/// A minimal representation of all possible errors that can occur using the
/// `IntoDecoder` trait.
#[derive(Clone, PartialEq)]
pub struct Error {
    err: ErrorImpl,
}

#[cfg(any(feature = "std", feature = "alloc"))]
type ErrorImpl = Box<str>;
#[cfg(not(any(feature = "std", feature = "alloc")))]
type ErrorImpl = ();

impl decode::Error for Error {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cold]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        Error {
            err: msg.to_string().into_boxed_str(),
        }
    }

    #[cfg(not(any(feature = "std", feature = "alloc")))]
    #[cold]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        let _ = msg;
        Error { err: () }
    }

    fn malformed(_byte_offset: usize) -> Self {
        Self::custom("malformed data")
    }
}

impl fmt::Display for Error {
    #[cfg(any(feature = "std", feature = "alloc"))]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.err)
    }

    #[cfg(not(any(feature = "std", feature = "alloc")))]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Decoding error")
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug = formatter.debug_tuple("Error");
        #[cfg(any(feature = "std", feature = "alloc"))]
        debug.field(&self.err);
        debug.finish()
    }
}

impl core::error::Error for Error {
    #[cfg(any(feature = "std", feature = "alloc"))]
    fn description(&self) -> &str {
        &self.err
    }

    #[cfg(not(any(feature = "std", feature = "alloc")))]
    fn description(&self) -> &str {
        ""
    }
}

macro_rules! primitive_deserializer {
    ($ty:ty, $doc:tt, $name:ident, $method:ident $($cast:tt)*) => {
        #[doc = "A deserializer holding"]
        #[doc = $doc]
        pub struct $name<E> {
            value: $ty,
            marker: PhantomData<E>
        }

        impl_copy_clone!($name);

        impl<'de, E> IntoDecoder<'de, E> for $ty
        where
    E: decode::Error,
        {
            type Decoder = $name<E>;

            fn into_decoder(self) -> $name<E> {
                $name::new(self)
            }
        }

        impl<E> $name<E> {
            #[expect(missing_docs)]
            #[must_use]
            pub fn new(value: $ty) -> Self {
                $name {
                    value,
                    marker: PhantomData,
                }
            }
        }

        impl<'de, E> Decoder<'de> for $name<E>
        where
    E: decode::Error,
        {
            type Error = E;

            fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
            where
                V: Visitor<'de>,
            {
                visitor.$method(self.value $($cast)*)
            }
        }

        impl<'de, E> IntoDecoder<'de, E> for $name<E>
        where
    E: decode::Error,
        {
            type Decoder = Self;

            fn into_decoder(self) -> Self {
                self
            }
        }

        impl<E> fmt::Debug for $name<E> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f
                    .debug_struct(stringify!($name))
                    .field("value", &self.value)
                    .finish()
            }
        }
    }
}

primitive_deserializer!(i8, "an `i8`.", I8Decoder, visit_i8);
primitive_deserializer!(i16, "an `i16`.", I16Decoder, visit_i16);
primitive_deserializer!(i32, "an `i32`.", I32Decoder, visit_i32);
primitive_deserializer!(i64, "an `i64`.", I64Decoder, visit_i64);
primitive_deserializer!(i128, "an `i128`.", I128Decoder, visit_i128);
primitive_deserializer!(u8, "a `u8`.", U8Decoder, visit_u8);
primitive_deserializer!(u16, "a `u16`.", U16Decoder, visit_u16);
primitive_deserializer!(u32, "a `u32`.", U32Decoder, visit_u32);
primitive_deserializer!(u64, "a `u64`.", U64Decoder, visit_u64);
primitive_deserializer!(u128, "a `u128`.", U128Decoder, visit_u128);
primitive_deserializer!(f32, "an `f32`.", F32Decoder, visit_f32);
primitive_deserializer!(f64, "an `f64`.", F64Decoder, visit_f64);

/// A decoder holding a `bool`.
pub struct BoolDecoder<E> {
    value: bool,
    marker: PhantomData<E>,
}

impl_copy_clone!(BoolDecoder);

impl<E> IntoDecoder<'_, E> for bool
where
    E: decode::Error,
{
    type Decoder = BoolDecoder<E>;

    fn into_decoder(self) -> BoolDecoder<E> {
        BoolDecoder::new(self)
    }
}

impl<E> BoolDecoder<E> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(value: bool) -> Self {
        Self {
            value,
            marker: PhantomData,
        }
    }
}

impl<'de, E> Decoder<'de> for BoolDecoder<E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        if self.value {
            visitor.visit_simple(Simple::new(SIMPLE_VALUE_TRUE))
        } else {
            visitor.visit_simple(Simple::new(SIMPLE_VALUE_FALSE))
        }
    }
}

impl<E> IntoDecoder<'_, E> for BoolDecoder<E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for BoolDecoder<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BoolDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `&str`.
pub struct StrDecoder<'a, E> {
    value: &'a str,
    marker: PhantomData<E>,
}

impl_copy_clone!(StrDecoder<'de>);

impl<'a, E> IntoDecoder<'_, E> for &'a str
where
    E: decode::Error,
{
    type Decoder = StrDecoder<'a, E>;

    fn into_decoder(self) -> StrDecoder<'a, E> {
        StrDecoder::new(self)
    }
}

impl<'a, E> StrDecoder<'a, E> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(value: &'a str) -> Self {
        StrDecoder {
            value,
            marker: PhantomData,
        }
    }
}

impl<'de, E> Decoder<'de> for StrDecoder<'_, E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(self.value)
    }
}

impl<E> IntoDecoder<'_, E> for StrDecoder<'_, E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for StrDecoder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StrDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `&str` with a lifetime tied to another
/// decoder.
pub struct BorrowedStrDecoder<'de, E> {
    value: &'de str,
    marker: PhantomData<E>,
}

impl_copy_clone!(BorrowedStrDecoder<'de>);

impl<'de, E> BorrowedStrDecoder<'de, E> {
    /// Create a new borrowed decoder from the given string.
    #[must_use]
    pub fn new(value: &'de str) -> BorrowedStrDecoder<'de, E> {
        BorrowedStrDecoder {
            value,
            marker: PhantomData,
        }
    }
}

impl<'de, E> Decoder<'de> for BorrowedStrDecoder<'de, E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_str(self.value)
    }
}

impl<'de, E> IntoDecoder<'de, E> for BorrowedStrDecoder<'de, E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for BorrowedStrDecoder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BorrowedStrDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `String`.
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub struct StringDecoder<E> {
    value: String,
    marker: PhantomData<E>,
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> Clone for StringDecoder<E> {
    fn clone(&self) -> Self {
        StringDecoder {
            value: self.value.clone(),
            marker: PhantomData,
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<E> IntoDecoder<'_, E> for String
where
    E: decode::Error,
{
    type Decoder = StringDecoder<E>;

    fn into_decoder(self) -> StringDecoder<E> {
        StringDecoder::new(self)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> StringDecoder<E> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(value: String) -> Self {
        StringDecoder {
            value,
            marker: PhantomData,
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<'de, E> Decoder<'de> for StringDecoder<E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_str(&self.value)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> IntoDecoder<'_, E> for StringDecoder<E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> fmt::Debug for StringDecoder<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("StringDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `Cow<str>`.
#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
pub struct CowStrDecoder<'a, E> {
    value: Cow<'a, str>,
    marker: PhantomData<E>,
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> Clone for CowStrDecoder<'_, E> {
    fn clone(&self) -> Self {
        CowStrDecoder {
            value: self.value.clone(),
            marker: PhantomData,
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'a, E> IntoDecoder<'_, E> for Cow<'a, str>
where
    E: decode::Error,
{
    type Decoder = CowStrDecoder<'a, E>;

    fn into_decoder(self) -> CowStrDecoder<'a, E> {
        CowStrDecoder::new(self)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<'a, E> CowStrDecoder<'a, E> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(value: Cow<'a, str>) -> Self {
        CowStrDecoder {
            value,
            marker: PhantomData,
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<'de, E> Decoder<'de> for CowStrDecoder<'_, E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        match self.value {
            Cow::Borrowed(string) => visitor.visit_str(string),
            Cow::Owned(string) => visitor.visit_str(&string),
        }
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> IntoDecoder<'_, E> for CowStrDecoder<'_, E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
impl<E> fmt::Debug for CowStrDecoder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CowStrDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `&[u8]`. Always calls [`Visitor::visit_bytes`].
pub struct BytesDecoder<'a, E> {
    value: &'a [u8],
    marker: PhantomData<E>,
}

impl<'a, E> BytesDecoder<'a, E> {
    /// Create a new decoder from the given bytes.
    #[must_use]
    pub fn new(value: &'a [u8]) -> Self {
        BytesDecoder {
            value,
            marker: PhantomData,
        }
    }
}

impl_copy_clone!(BytesDecoder<'a>);

impl<'a, E> IntoDecoder<'_, E> for &'a [u8]
where
    E: decode::Error,
{
    type Decoder = BytesDecoder<'a, E>;

    fn into_decoder(self) -> BytesDecoder<'a, E> {
        BytesDecoder::new(self)
    }
}

impl<'de, E> Decoder<'de> for BytesDecoder<'_, E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bytes(self.value)
    }
}

impl<E> IntoDecoder<'_, E> for BytesDecoder<'_, E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for BytesDecoder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BytesDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a `&[u8]` with a lifetime tied to another
/// decoder. Always calls [`Visitor::visit_borrowed_bytes`].
pub struct BorrowedBytesDecoder<'de, E> {
    value: &'de [u8],
    marker: PhantomData<E>,
}

impl<'de, E> BorrowedBytesDecoder<'de, E> {
    /// Create a new borrowed deserializer from the given borrowed bytes.
    #[must_use]
    pub fn new(value: &'de [u8]) -> Self {
        BorrowedBytesDecoder {
            value,
            marker: PhantomData,
        }
    }
}

impl_copy_clone!(BorrowedBytesDecoder<'de>);

impl<'de, E> Decoder<'de> for BorrowedBytesDecoder<'de, E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_borrowed_bytes(self.value)
    }
}

impl<'de, E> IntoDecoder<'de, E> for BorrowedBytesDecoder<'de, E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for BorrowedBytesDecoder<'_, E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BorrowedBytesDecoder")
            .field("value", &self.value)
            .finish()
    }
}

////////////////////////////////////////////////////////////////////////////////

/// A decoder that iterates over an array.
#[derive(Clone)]
pub struct ArrDecoder<I, E> {
    iter: iter::Fuse<I>,
    count: usize,
    marker: PhantomData<E>,
}

impl<I, E> ArrDecoder<I, E>
where
    I: Iterator,
{
    /// Construct a new `ArrDecoder<I, E>`.
    pub fn new(iter: I) -> Self {
        ArrDecoder {
            iter: iter.fuse(),
            count: 0,
            marker: PhantomData,
        }
    }
}

impl<I, E> ArrDecoder<I, E>
where
    I: Iterator,
    E: decode::Error,
{
    /// Check for remaining elements after passing a `ArrDecoder` to
    /// `Visitor::visit_arr`.
    ///
    /// # Errors
    ///
    /// If the expected number of elements does not match the actual count, an
    /// invalid length will be raised.
    pub fn end(self) -> Result<(), E> {
        let remaining = self.iter.count();
        if remaining == 0 {
            Ok(())
        } else {
            // First argument is the number of elements in the data, second
            // argument is the number of elements expected by the Decode.
            Err(E::invalid_length(
                self.count + remaining,
                &ExpectedInArr(self.count),
            ))
        }
    }
}

impl<'de, I, T, E> Decoder<'de> for ArrDecoder<I, E>
where
    I: Iterator<Item = T>,
    T: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let v = visitor.visit_arr(&mut self)?;
        self.end()?;
        Ok(v)
    }
}

impl<'de, I, T, E> IntoDecoder<'de, E> for ArrDecoder<I, E>
where
    I: Iterator<Item = T>,
    T: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<'de, I, T, E> ArrAccess<'de> for ArrDecoder<I, E>
where
    I: Iterator<Item = T>,
    T: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn next_element_seed<V>(&mut self, seed: V) -> Result<Option<V::Value>, Self::Error>
    where
        V: DecodeSeed<'de>,
    {
        match self.iter.next() {
            Some(value) => {
                self.count += 1;
                seed.decode(value.into_decoder()).map(Some)
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        size_hint::from_bounds(&self.iter)
    }
}

struct ExpectedInArr(usize);

impl Expected for ExpectedInArr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 1 {
            f.write_str("1 element in array")
        } else {
            write!(f, "{} elements in array", self.0)
        }
    }
}

impl<I, E> fmt::Debug for ArrDecoder<I, E>
where
    I: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ArrDecoder")
            .field("iter", &self.iter)
            .field("count", &self.count)
            .finish()
    }
}

////////////////////////////////////////////////////////////////////////////////

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, T, E> IntoDecoder<'de, E> for Vec<T>
where
    T: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Decoder = ArrDecoder<<Self as IntoIterator>::IntoIter, E>;

    fn into_decoder(self) -> Self::Decoder {
        ArrDecoder::new(self.into_iter())
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, T, E> IntoDecoder<'de, E> for BTreeSet<T>
where
    T: IntoDecoder<'de, E> + Eq + Ord,
    E: decode::Error,
{
    type Decoder = ArrDecoder<<Self as IntoIterator>::IntoIter, E>;

    fn into_decoder(self) -> Self::Decoder {
        ArrDecoder::new(self.into_iter())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl<'de, T, S, E> IntoDecoder<'de, E> for HashSet<T, S>
where
    T: IntoDecoder<'de, E> + Eq + Hash,
    S: BuildHasher,
    E: decode::Error,
{
    type Decoder = ArrDecoder<<Self as IntoIterator>::IntoIter, E>;

    fn into_decoder(self) -> Self::Decoder {
        ArrDecoder::new(self.into_iter())
    }
}

////////////////////////////////////////////////////////////////////////////////

/// A decoder holding a `ArrAccess`.
#[derive(Clone, Debug)]
pub struct ArrAccessDecoder<A> {
    arr: A,
}

impl<A> ArrAccessDecoder<A> {
    /// Construct a new `ArrAccessDecoder<A>`.
    pub fn new(arr: A) -> Self {
        ArrAccessDecoder { arr }
    }
}

impl<'de, A> Decoder<'de> for ArrAccessDecoder<A>
where
    A: ArrAccess<'de>,
{
    type Error = A::Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_arr(self.arr)
    }
}

impl<'de, A> IntoDecoder<'de, A::Error> for ArrAccessDecoder<A>
where
    A: ArrAccess<'de>,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

/// A decoder that iterates over a map.
pub struct MapDecoder<'de, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
{
    iter: iter::Fuse<I>,
    value: Option<Second<I::Item>>,
    count: usize,
    lifetime: PhantomData<&'de ()>,
    error: PhantomData<E>,
}

impl<I, E> MapDecoder<'_, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
{
    /// Construct a new `MapDecoder<I, E>`.
    pub fn new(iter: I) -> Self {
        MapDecoder {
            iter: iter.fuse(),
            value: None,
            count: 0,
            lifetime: PhantomData,
            error: PhantomData,
        }
    }
}

impl<I, E> MapDecoder<'_, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
    E: decode::Error,
{
    /// Check for remaining elements after passing a `MapDecoder` to
    /// `Visitor::visit_map`.
    ///
    /// # Errors
    ///
    /// If the expected number of elements does not match the actual number of
    /// elements, then an invalid length error is raised.
    pub fn end(self) -> Result<(), E> {
        let remaining = self.iter.count();
        if remaining == 0 {
            Ok(())
        } else {
            // First argument is the number of elements in the data, second
            // argument is the number of elements expected by the Decode.
            Err(E::invalid_length(
                self.count + remaining,
                &ExpectedInMap(self.count),
            ))
        }
    }
}

impl<I, E> MapDecoder<'_, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
{
    #[expect(clippy::type_complexity)]
    fn next_pair(&mut self) -> Option<(First<I::Item>, Second<I::Item>)> {
        match self.iter.next() {
            Some(kv) => {
                self.count += 1;
                Some(private::Pair::split(kv))
            }
            None => None,
        }
    }
}

impl<'de, I, E> Decoder<'de> for MapDecoder<'de, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
    First<I::Item>: IntoDecoder<'de, E>,
    Second<I::Item>: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(mut self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let value = visitor.visit_map(&mut self)?;
        self.end()?;
        Ok(value)
    }
}

impl<'de, I, E> IntoDecoder<'de, E> for MapDecoder<'de, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
    First<I::Item>: IntoDecoder<'de, E>,
    Second<I::Item>: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<'de, I, E> MapAccess<'de> for MapDecoder<'de, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
    First<I::Item>: IntoDecoder<'de, E>,
    Second<I::Item>: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn next_key_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        match self.next_pair() {
            Some((key, value)) => {
                self.value = Some(value);
                seed.decode(key.into_decoder()).map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<T>(&mut self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        let value = self.value.take();
        // Panic because this indicates a bug in the program rather than an
        // expected failure.
        let value = value.expect("MapAccess::next_value called before next_key");
        seed.decode(value.into_decoder())
    }

    fn next_entry_seed<TK, TV>(
        &mut self,
        kseed: TK,
        vseed: TV,
    ) -> Result<Option<(TK::Value, TV::Value)>, Self::Error>
    where
        TK: DecodeSeed<'de>,
        TV: DecodeSeed<'de>,
    {
        match self.next_pair() {
            Some((key, value)) => {
                let key = kseed.decode(key.into_decoder())?;
                let value = vseed.decode(value.into_decoder())?;
                Ok(Some((key, value)))
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        size_hint::from_bounds(&self.iter)
    }
}

impl<'de, I, E> ArrAccess<'de> for MapDecoder<'de, I, E>
where
    I: Iterator,
    I::Item: private::Pair,
    First<I::Item>: IntoDecoder<'de, E>,
    Second<I::Item>: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        match self.next_pair() {
            Some((k, v)) => {
                let de = PairDecoder(k, v, PhantomData);
                seed.decode(de).map(Some)
            }
            None => Ok(None),
        }
    }

    fn size_hint(&self) -> Option<usize> {
        size_hint::from_bounds(&self.iter)
    }
}

// Cannot #[derive(Clone)] because of the bound `Second<I::Item>: Clone`.
impl<I, E> Clone for MapDecoder<'_, I, E>
where
    I: Iterator + Clone,
    I::Item: private::Pair,
    Second<I::Item>: Clone,
{
    fn clone(&self) -> Self {
        MapDecoder {
            iter: self.iter.clone(),
            value: self.value.clone(),
            count: self.count,
            lifetime: self.lifetime,
            error: self.error,
        }
    }
}

impl<I, E> fmt::Debug for MapDecoder<'_, I, E>
where
    I: Iterator + fmt::Debug,
    I::Item: private::Pair,
    Second<I::Item>: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MapDecoder")
            .field("iter", &self.iter)
            .field("value", &self.value)
            .field("count", &self.count)
            .finish()
    }
}

// Used in the `impl ArrAccess for MapDecoder` to visit the map as an
// array of pairs.
struct PairDecoder<A, B, E>(A, B, PhantomData<E>);

impl<'de, A, B, E> Decoder<'de> for PairDecoder<A, B, E>
where
    A: IntoDecoder<'de, E>,
    B: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        let mut pair_visitor = PairVisitor(Some(self.0), Some(self.1), PhantomData);
        let pair = visitor.visit_arr(&mut pair_visitor)?;
        if pair_visitor.1.is_none() {
            Ok(pair)
        } else {
            let remaining = pair_visitor.size_hint().unwrap();
            // First argument is the number of elements in the data, second
            // argument is the number of elements expected by the Decode.
            Err(E::invalid_length(2, &ExpectedInArr(2 - remaining)))
        }
    }
}

struct PairVisitor<A, B, E>(Option<A>, Option<B>, PhantomData<E>);

impl<'de, A, B, E> ArrAccess<'de> for PairVisitor<A, B, E>
where
    A: IntoDecoder<'de, E>,
    B: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Error = E;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        if let Some(k) = self.0.take() {
            seed.decode(k.into_decoder()).map(Some)
        } else if let Some(v) = self.1.take() {
            seed.decode(v.into_decoder()).map(Some)
        } else {
            Ok(None)
        }
    }

    fn size_hint(&self) -> Option<usize> {
        if self.0.is_some() {
            Some(2)
        } else if self.1.is_some() {
            Some(1)
        } else {
            Some(0)
        }
    }
}

struct ExpectedInMap(usize);

impl Expected for ExpectedInMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == 1 {
            f.write_str("1 element in map")
        } else {
            write!(f, "{} elements in map", self.0)
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl<'de, K, V, E> IntoDecoder<'de, E> for BTreeMap<K, V>
where
    K: IntoDecoder<'de, E> + Eq + Ord,
    V: IntoDecoder<'de, E>,
    E: decode::Error,
{
    type Decoder = MapDecoder<'de, <Self as IntoIterator>::IntoIter, E>;

    fn into_decoder(self) -> Self::Decoder {
        MapDecoder::new(self.into_iter())
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl<'de, K, V, S, E> IntoDecoder<'de, E> for HashMap<K, V, S>
where
    K: IntoDecoder<'de, E> + Eq + Hash,
    V: IntoDecoder<'de, E>,
    S: BuildHasher,
    E: decode::Error,
{
    type Decoder = MapDecoder<'de, <Self as IntoIterator>::IntoIter, E>;

    fn into_decoder(self) -> Self::Decoder {
        MapDecoder::new(self.into_iter())
    }
}

////////////////////////////////////////////////////////////////////////////////

/// A deserializer holding a `MapAccess`.
#[derive(Clone, Debug)]
pub struct MapAccessDecoder<A> {
    map: A,
}

impl<A> MapAccessDecoder<A> {
    /// Construct a new `MapAccessDecoder<A>`.
    pub fn new(map: A) -> Self {
        MapAccessDecoder { map }
    }
}

impl<'de, A> Decoder<'de> for MapAccessDecoder<A>
where
    A: MapAccess<'de>,
{
    type Error = A::Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_map(self.map)
    }
}

impl<'de, A> IntoDecoder<'de, A::Error> for MapAccessDecoder<A>
where
    A: MapAccess<'de>,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

mod private {
    /// Avoid having to restate the generic types on `MapDecoder`. The
    /// `Iterator::Item` contains enough information to figure out K and V.
    pub trait Pair {
        type First;
        type Second;
        fn split(self) -> (Self::First, Self::Second);
    }

    impl<A, B> Pair for (A, B) {
        type First = A;
        type Second = B;
        fn split(self) -> (A, B) {
            self
        }
    }

    pub(crate) type First<T> = <T as Pair>::First;
    pub(crate) type Second<T> = <T as Pair>::Second;
}

/// A decoder holding a [`Simple`].
pub struct SimpleDecoder<E> {
    value: Simple,
    marker: PhantomData<E>,
}

impl_copy_clone!(SimpleDecoder);

impl<E> IntoDecoder<'_, E> for Simple
where
    E: decode::Error,
{
    type Decoder = SimpleDecoder<E>;

    fn into_decoder(self) -> SimpleDecoder<E> {
        SimpleDecoder::new(self)
    }
}

impl<E> SimpleDecoder<E> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(value: Simple) -> Self {
        SimpleDecoder {
            value,
            marker: PhantomData,
        }
    }
}

impl<'de, E> Decoder<'de> for SimpleDecoder<E>
where
    E: decode::Error,
{
    type Error = E;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_simple(self.value)
    }
}

impl<E> IntoDecoder<'_, E> for SimpleDecoder<E>
where
    E: decode::Error,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for SimpleDecoder<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SimpleDecoder")
            .field("value", &self.value)
            .finish()
    }
}

/// A decoder holding a tag number.
pub struct TagDecoder<D> {
    tag_num: u64,
    decoder: D,
}

impl<D> TagDecoder<D> {
    #[expect(missing_docs)]
    #[must_use]
    pub fn new(tag_num: u64, decoder: D) -> Self {
        Self { tag_num, decoder }
    }
}

impl<'de, D> Decoder<'de> for TagDecoder<D>
where
    D: Decoder<'de>,
{
    type Error = D::Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_tag(self.tag_num, self.decoder)
    }
}

impl<'de, D> IntoDecoder<'de, D::Error> for TagDecoder<D>
where
    D: Decoder<'de>,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

impl<E> fmt::Debug for TagDecoder<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TagDecoder")
            .field("tag_num", &self.tag_num)
            .finish()
    }
}

/// A decoder holding a `IndefiniteItemAccess`.
#[derive(Clone, Debug)]
pub struct IndefiniteLenBytesAccessDecoder<A> {
    arr: A,
}

impl<A> IndefiniteLenBytesAccessDecoder<A> {
    /// Construct a new `IndefiniteItemAccessDecoder<A>`.
    pub fn new(arr: A) -> Self {
        Self { arr }
    }
}

impl<'de, A> Decoder<'de> for IndefiniteLenBytesAccessDecoder<A>
where
    A: IndefiniteLenItemAccess<'de>,
{
    type Error = A::Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_indefinite_len_bytes(self.arr)
    }
}

impl<'de, A> IntoDecoder<'de, A::Error> for IndefiniteLenBytesAccessDecoder<A>
where
    A: IndefiniteLenItemAccess<'de>,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}

/// A decoder holding a `IndefiniteItemAccess`.
#[derive(Clone, Debug)]
pub struct IndefiniteLenStringAccessDecoder<A> {
    arr: A,
}

impl<A> IndefiniteLenStringAccessDecoder<A> {
    /// Construct a new `IndefiniteItemAccessDecoder<A>`.
    pub fn new(arr: A) -> Self {
        Self { arr }
    }
}

impl<'de, A> Decoder<'de> for IndefiniteLenStringAccessDecoder<A>
where
    A: IndefiniteLenItemAccess<'de>,
{
    type Error = A::Error;

    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_indefinite_len_str(self.arr)
    }
}

impl<'de, A> IntoDecoder<'de, A::Error> for IndefiniteLenStringAccessDecoder<A>
where
    A: IndefiniteLenItemAccess<'de>,
{
    type Decoder = Self;

    fn into_decoder(self) -> Self {
        self
    }
}
