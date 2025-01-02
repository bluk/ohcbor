//! Encoder for CBOR data.
//!
//! Most of the traits and implementations are forks from [Serde][serde]'s
//! generic serialization module.
//!
//! [`Encode`] is the most important trait in this module. It specifies how the
//! Rust data type should be encoded into CBOR's data model. It is similar to
//! Serde's [`Serialize`][serde_serialize] trait.
//!
//! For some types such as `u64` or `String`, there is a corresponding CBOR data
//! type so the encoding is simple. For more complex types such as a `struct`
//! with various fields and types, the `Encode` implementation may be more
//! complex.
//!
//! On the other hand, [`Encoder`] (with the `r`) is the trait which specifies
//! how data is encoded into the CBOR data model. It is similar to
//! [`Serializer`][serde_serializer]. This library provides implementations for
//! `Encoder` and most library users should not need to implement their own
//! `Encoder`.
//!
//! # Encode
//!
//! Similar to `Serde`, this library provides [`Encode`] implementations for
//! many Rust primitive and standard library types which are listed below.
//!
//!  - **Primitive types**:
//!    - `bool`
//!    - `i8`, `i16`, `i32`, `i64`, `i128`, `isize`
//!    - `u8`, `u16`, `u32`, `u64`, `u128`, `usize`
//!    - `f32`, `f64`
//!    - `str`
//!    - `&T` and `&mut T`
//!  - **Compound types**:
//!    - `[T; 0]` through `[T; 32]`
//!    - tuples up to size 16
//!  - **Common standard library types**:
//!    - `String`
//!    - `Option<T>`
//!  - **Wrapper types**:
//!    - `Box<T>`
//!    - `Cow<'a, T>`
//!    - `Cell<T>`
//!    - `RefCell<T>`
//!    - `Mutex<T>`
//!    - `RwLock<T>`
//!  - **Collection types**:
//!    - `BTreeMap<K, V>`
//!    - `BTreeSet<T>`
//!    - `BinaryHeap<T>`
//!    - `HashMap<K, V, H>`
//!    - `HashSet<T, H>`
//!    - `LinkedList<T>`
//!    - `VecDeque<T>`
//!    - `Vec<T>`
//!  - **Miscellaneous standard library types**:
//!    - `num::NonZero*`
//!    - `num::Saturating<T>`
//!    - `sync::Atomic*`
//!
//! Types which are supported by Serde but not this library currently:
//!
//!  - **Primitive types**:
//!    - `char`
//!  - **Common standard library types**:
//!    - `Result<T, E>`
//!    - `PhantomData<T>`
//!  - **Wrapper types**:
//!    - `Rc<T>`
//!    - `Arc<T>`
//!  - **FFI types**:
//!    - `CStr`
//!    - `CString`
//!    - `Box<CStr>`
//!    - `OsStr`
//!    - `OsString`
//!  - **Miscellaneous standard library types**:
//!    - `Duration`
//!    - `SystemTime`
//!    - `Path`
//!    - `PathBuf`
//!    - `Range<T>`
//!    - `RangeFrom<T>`
//!    - `RangeInclusive<T>`
//!    - `RangeTo<T>`
//!    - `Bound<T>`
//!    - `!`
//!  - **Net types**:
//!    - `IpAddr`
//!    - `Ipv4Addr`
//!    - `Ipv6Addr`
//!    - `SocketAddr`
//!    - `SocketAddrV4`
//!    - `SocketAddrV6`
//!
//! [serde]: https://serde.rs
//! [serde_serialize]: https://docs.rs/serde/latest/serde/trait.Serialize.html
//! [serde_serializer]: https://docs.rs/serde/latest/serde/ser/trait.Serializer.html

pub(crate) mod encoders;
mod fmt;
mod impls;
mod impossible;

use crate::ErrorKind;

pub use self::impossible::Impossible;

/// Trait used by [`Encode`] implementations to generically construct
/// errors belonging to the [`Encoder`] against which they are
/// currently running.
pub trait Error: Sized + core::error::Error {
    /// Used when an [`Encode`] implementation encounters any error
    /// while encoding a type.
    ///
    /// The message should not be capitalized and should not end with a
    /// period.
    fn custom<T>(msg: T) -> Self
    where
        T: core::fmt::Display;
}

impl Error for crate::Error {
    fn custom<T>(msg: T) -> Self
    where
        T: core::fmt::Display,
    {
        crate::Error::new(ErrorKind::Decode(msg.to_string()), 0)
    }
}

/// A **data structure** that can be encoded to the CBOR data format.
///
/// This library provides `Encode` implementations for many Rust primitive and
/// standard library types. The complete list is [here][crate::encode].
pub trait Encode {
    /// Encode this value into the given encoder.
    ///
    /// # Errors
    ///
    /// Invalid values and other errors returned from the [`Encoder`].
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder;
}

/// Encodes data into the CBOR data model.
#[expect(clippy::missing_errors_doc)]
pub trait Encoder: Sized {
    /// Output type produced by this `Encoder` during successful
    /// encoding. Most encoders that produce text or binary output
    /// should set `Ok = ()` and encode into an [`io::Write`] or buffer
    /// contained within the `Encoder` instance. Encoders that build
    /// in-memory data structures may be simplified by using `Ok` to propagate
    /// the data structure around.
    ///
    /// [`io::Write`]: https://doc.rust-lang.org/std/io/trait.Write.html
    type Ok;

    /// The error type when some error occurs during encoding.
    type Error: Error;

    /// Type returned from [`Encoder::encode_arr()`] for encoding the content of
    /// the array.
    type EncodeArr: EncodeArr<Ok = Self::Ok, Error = Self::Error>;

    /// Type returned from [`Encoder::encode_map()`] for encoding the content of
    /// the map.
    type EncodeMap: EncodeMap<Ok = Self::Ok, Error = Self::Error>;

    /// Encodes a `bool` value.
    fn encode_bool(self, v: bool) -> Result<Self::Ok, Self::Error>;

    /// Encode an `i8` value.
    fn encode_i8(self, v: i8) -> Result<Self::Ok, Self::Error>;

    /// Encode an `i16` value.
    fn encode_i16(self, v: i16) -> Result<Self::Ok, Self::Error>;

    /// Encode an `i32` value.
    fn encode_i32(self, v: i32) -> Result<Self::Ok, Self::Error>;

    /// Encode an `i64` value.
    fn encode_i64(self, v: i64) -> Result<Self::Ok, Self::Error>;

    /// Encode an `i128` value.
    fn encode_i128(self, v: i128) -> Result<Self::Ok, Self::Error>;

    /// Encode a `u8` value.
    fn encode_u8(self, v: u8) -> Result<Self::Ok, Self::Error>;

    /// Encode a `u16` value.
    fn encode_u16(self, v: u16) -> Result<Self::Ok, Self::Error>;

    /// Encode a `u32` value.
    fn encode_u32(self, v: u32) -> Result<Self::Ok, Self::Error>;

    /// Encode a `u64` value.
    fn encode_u64(self, v: u64) -> Result<Self::Ok, Self::Error>;

    /// Encode a `u128` value.
    fn encode_u128(self, v: u128) -> Result<Self::Ok, Self::Error>;

    /// Encode a `f32` value.
    fn encode_f32(self, v: f32) -> Result<Self::Ok, Self::Error>;

    /// Encode a `f64` value.
    fn encode_f64(self, v: f64) -> Result<Self::Ok, Self::Error>;

    /// Encode a `&str`.
    fn encode_str(self, v: &str) -> Result<Self::Ok, Self::Error>;

    /// Encode a chunk of raw byte data.
    fn encode_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error>;

    /// Encode an [`Option::None`].
    fn encode_none(self) -> Result<Self::Ok, Self::Error>;

    /// Begin to encode a variably sized array. This call must be
    /// followed by zero or more calls to `encode_element`, then a call to
    /// `end`.
    ///
    /// The argument is the number of elements in the array, which may or may
    /// not be computable before the array is iterated.
    fn encode_arr(self, len: Option<usize>) -> Result<Self::EncodeArr, Self::Error>;

    /// Begin to encode a map. This call must be followed by zero or more
    /// calls to `encode_key` and `encode_value`, then a call to `end`.
    ///
    /// The argument is the number of elements in the map, which may or may not
    /// be computable before the map is iterated.
    fn encode_map(self, len: Option<usize>) -> Result<Self::EncodeMap, Self::Error>;

    /// Collect an iterator as an array.
    ///
    /// The default implementation encodes each item yielded by the iterator
    /// using [`Encoder::encode_arr()`]. Implementors should not need to override this
    /// method.
    fn collect_arr<I>(self, iter: I) -> Result<Self::Ok, Self::Error>
    where
        I: IntoIterator,
        <I as IntoIterator>::Item: Encode,
    {
        let mut iter = iter.into_iter();
        let mut encoder = self.encode_arr(iterator_len_hint(&iter))?;
        iter.try_for_each(|item| encoder.encode_element(&item))?;
        encoder.end()
    }

    /// Collect an iterator as a map.
    ///
    /// The default implementation encodes each pair yielded by the iterator
    /// using [`Encoder::encode_map()`]. Implementors should not need to override this
    /// method.
    fn collect_map<K, V, I>(self, iter: I) -> Result<Self::Ok, Self::Error>
    where
        K: Encode,
        V: Encode,
        I: IntoIterator<Item = (K, V)>,
    {
        let mut iter = iter.into_iter();
        let mut encoder = self.encode_map(iterator_len_hint(&iter))?;
        iter.try_for_each(|(key, value)| encoder.encode_entry(&key, &value))?;
        encoder.end()
    }

    /// Encode a string produced by an implementation of `Display`.
    ///
    /// The default implementation builds a heap-allocated [`String`] and
    /// delegates to [`Encoder::encode_str()`]. Encoders are encouraged to provide a
    /// more efficient implementation if possible.
    #[cfg(any(feature = "std", feature = "alloc"))]
    fn collect_str<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + core::fmt::Display,
    {
        self.encode_str(&value.to_string())
    }

    /// Encode a string produced by an implementation of `Display`.
    ///
    /// Encoders that use `no_std` are required to provide an implementation
    /// of this method. If no more sensible behavior is possible, the
    /// implementation is expected to return an error.
    #[cfg(not(any(feature = "std", feature = "alloc")))]
    fn collect_str<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Display;
}

/// Returned from [`Encoder::encode_arr()`].
#[expect(clippy::module_name_repetitions)]
#[expect(clippy::missing_errors_doc)]
pub trait EncodeArr {
    /// Must match the `Ok` type of our [`Encoder`].
    type Ok;

    /// Must match the `Error` type of our [`Encoder`].
    type Error: Error;

    /// Encode an array element.
    fn encode_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Encode;

    /// Finish encoding an array.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

/// Returned from [`Encoder::encode_map()`].
#[expect(clippy::module_name_repetitions)]
#[expect(clippy::missing_errors_doc)]
pub trait EncodeMap {
    /// Must match the `Ok` type of our [`Encoder`].
    type Ok;

    /// Must match the `Error` type of our [`Encoder`].
    type Error: Error;

    /// Encode a map key.
    ///
    /// If possible, `Encode` implementations are encouraged to use
    /// [`EncodeMap::encode_entry()`] instead as it may be implemented more efficiently in
    /// some formats compared to a pair of calls to `encode_key` and
    /// `encode_value`.
    fn encode_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Encode;

    /// Encode a map value.
    ///
    /// # Panics
    ///
    /// Calling `encode_value` before [`EncodeMap::encode_key()`] is incorrect and is
    /// allowed to panic or produce bogus results.
    fn encode_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + Encode;

    /// Encode a map entry consisting of a key and a value.
    ///
    /// Some [`Encode`] types are not able to hold a key and value in memory at
    /// the same time so `EncodeMap` implementations are required to support
    /// [`EncodeMap::encode_key()`] and [`EncodeMap::encode_value()`]
    /// individually. The `encode_entry` method allows encoders to optimize for
    /// the case where key and value are both available.  [`Encode`]
    /// implementations are encouraged to use `encode_entry` if possible.
    ///
    /// The default implementation delegates to [`EncodeMap::encode_key()`] and
    /// [`EncodeMap::encode_value()`]. This is appropriate for encoders that do
    /// not care about performance or are not able to optimize `encode_entry`
    /// any better than this.
    fn encode_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Self::Error>
    where
        K: ?Sized + Encode,
        V: ?Sized + Encode,
    {
        self.encode_key(key)?;
        self.encode_value(value)
    }

    /// Finish encoding a map.
    fn end(self) -> Result<Self::Ok, Self::Error>;
}

fn iterator_len_hint<I>(iter: &I) -> Option<usize>
where
    I: Iterator,
{
    match iter.size_hint() {
        (lo, Some(hi)) if lo == hi => Some(lo),
        _ => None,
    }
}
