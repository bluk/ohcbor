//! Decoder for CBOR data.
//!
//! Most of the traits and implementations are forks from [Serde][serde]'s
//! generic deserialization module.
//!
//! [`Decode`] is the most important trait in this module. It specifies how data
//! from CBOR's data model should be decoded into the implementing data type. It
//! is similar to Serde's [`Deserialize`][serde_deserialize] trait. For simple
//! types such as `u64` or `String`, mapping CBOR data to the Rust type is
//! relatively straightforward. For more complex types such as a `struct` with
//! various fields and types, the `Decode` implementation may be more complex.
//!
//! On the other hand, [`Decoder`] (with the `r`) is the trait which specifies
//! how data is decoded into the CBOR data model. It is similar to
//! [`Deserializer`][serde_deserializer]. This library provides implementations
//! for `Decoder` and most library users should not need to implement their own
//! `Decoder`.
//!
//! # Decode
//!
//! Similar to `Serde`, this library provides [`Decode`] implementations for
//! many Rust primitive and standard library types which are listed below.
//!
//!  - **Primitive types**:
//!    - `bool`
//!    - `i8`, `i16`, `i32`, `i64`, `i128`, `isize`
//!    - `u8`, `u16`, `u32`, `u64`, `u128`, `usize`
//!    - `f32`, `f64`
//!  - **Compound types**:
//!    - `[T; 0]` through `[T; 32]`
//!    - tuples up to size 16
//!  - **Common standard library types**:
//!    - `String`
//!    - `Option<T>`
//!    - `PhantomData<T>`
//!  - **Wrapper types**:
//!    - `Box<T>`
//!    - `Box<[T]>`
//!    - `Box<str>`
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
//!  - **Zero-copy types**:
//!    - `&str`
//!    - `&[u8]`
//!  - **Miscellaneous standard library types**:
//!    - `num::NonZero*`
//!    - `num::Saturating<T>`
//!
//! Types which are supported by Serde but not this library currently:
//!
//!  - **Primitive types**:
//!    - `char`
//!  - **Common standard library types**:
//!    - `Result<T, E>`
//!  - **Wrapper types**:
//!    - `Rc<T>`
//!    - `Arc<T>`
//!  - **FFI types**:
//!    - `CString`
//!    - `Box<CStr>`
//!    - `OsString`
//!  - **Miscellaneous standard library types**:
//!    - `Duration`
//!    - `SystemTime`
//!    - `Path`
//!    - `PathBuf`
//!    - `Range<T>`
//!    - `RangeInclusive<T>`
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
//! [serde_deserialize]: https://docs.rs/serde/latest/serde/trait.Deserialize.html
//! [serde_deserializer]: https://docs.rs/serde/latest/serde/trait.Deserializer.html

use core::{fmt, marker::PhantomData};

use crate::error::ErrorKind;

pub(crate) mod decoders;
mod impls;
mod seed;
pub(crate) mod size_hint;
pub mod value;

/// Allows [`Decode`] implementations to create descriptive error messages
/// belonging to the [`Decoder`] against which they are currently running.
///
/// Most decoders should only need to provide the [`Error::custom()`] method and
/// inherit the default behavior for the other methods.
pub trait Error: Sized + core::error::Error {
    /// Raised when there is general error when decoding a type.
    ///
    /// The message should not be capitalized and should not end with a period.
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display;

    /// Raised when a [`Decode`] receives a type different from what it was
    /// expecting.
    ///
    /// The `unexp` argument provides information about what type was received.
    /// This is the type that was present in the input file or other source data
    /// of the [`Decoder`].
    ///
    /// The `exp` argument provides information about what type was being
    /// expected. This is the type that is written in the program.
    #[cold]
    fn invalid_type(unexp: Unexpected<'_>, exp: &dyn Expected) -> Self {
        Error::custom(format!("invalid type: {unexp}, expected {exp}"))
    }

    /// Raised when a [`Decode`] receives a value of the right type but that
    /// is wrong for some other reason.
    ///
    /// The `unexp` argument provides information about what value was received.
    /// This is the value that was present in the input file or other source
    /// data of the [`Decoder`].
    ///
    /// The `exp` argument provides information about what value was being
    /// expected. This is the type that is written in the program.
    #[cold]
    fn invalid_value(unexp: Unexpected<'_>, exp: &dyn Expected) -> Self {
        Error::custom(format!("invalid value: {unexp}, expected {exp}"))
    }

    /// Raised when decoding an array or map and the input data contains too
    /// many or too few elements.
    ///
    /// The `len` argument is the number of elements encountered. The array or
    /// map may have expected more arguments or fewer arguments.
    ///
    /// The `exp` argument provides information about what data was being
    /// expected. For example `exp` might say that a tuple of size 6 was
    /// expected.
    #[cold]
    fn invalid_length(len: usize, exp: &dyn Expected) -> Self {
        Error::custom(format!("invalid length: {len}, expected {exp}"))
    }

    /// Raised when the CBOR data is malformed such as when an invalid value is
    /// used for the data type argument.
    fn malformed(byte_offset: usize) -> Self;
}

/// Represents an unexpected invocation of any one of the [`Visitor`] trait
/// methods.
///
/// This is used as an argument to the `invalid_type`, `invalid_value`, and
/// `invalid_length` methods of the `Error` trait to build error messages.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Unexpected<'a> {
    /// The input contained a boolean value that was not expected.
    Bool(bool),

    /// The input contained an unsigned integer `u8` that was not expected.
    UnsignedU8(u8),
    /// The input contained an unsigned integer `u16` that was not expected.
    UnsignedU16(u16),
    /// The input contained an unsigned integer `u32` that was not expected.
    UnsignedU32(u32),
    /// The input contained an unsigned integer `u64` that was not expected.
    UnsignedU64(u64),
    /// The input contained an unsigned integer `u128` that was not expected.
    UnsignedU128(u128),

    /// The input contained a negative integer `i8` that was not expected.
    NegI8(i8),
    /// The input contained a negative integer `i16` that was not expected.
    NegI16(i16),
    /// The input contained a negative integer `i32` that was not expected.
    NegI32(i32),
    /// The input contained a negative integer `i64` that was not expected.
    NegI64(i64),
    /// The input contained a negative integer `i128` that was not expected.
    NegI128(i128),

    /// The input contained a string that was not expected.
    Str(&'a str),
    /// The input contained a byte string that was not expected.
    Bytes(&'a [u8]),

    /// The input contained a `null` value that was not expected.
    Null,
    /// The input contained an `undefined` value that was not expected.
    Undefined,
    /// The input contained  an `Option<T>` that was not expected.
    Option,

    /// The input contained  an array that was not expected.
    Array,
    /// The input contained  a map that was not expected.
    Map,
}

macro_rules! unexpected_from {
    ($ty:ident:$var:ident) => {
        impl<'a> From<$ty> for Unexpected<'a> {
            fn from(value: $ty) -> Self {
                Self::$var(value)
            }
        }
    };
}

unexpected_from!(u8:UnsignedU8);
unexpected_from!(u16:UnsignedU16);
unexpected_from!(u32:UnsignedU32);
unexpected_from!(u64:UnsignedU64);
unexpected_from!(u128:UnsignedU128);
unexpected_from!(i8:NegI8);
unexpected_from!(i16:NegI16);
unexpected_from!(i32:NegI32);
unexpected_from!(i64:NegI64);
unexpected_from!(i128:NegI128);

impl fmt::Display for Unexpected<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Unexpected::Bool(b) => write!(f, "boolean `{b}`"),
            Unexpected::UnsignedU8(n) => write!(f, "positive integer {n}"),
            Unexpected::UnsignedU16(n) => write!(f, "positive integer {n}"),
            Unexpected::UnsignedU32(n) => write!(f, "positive integer {n}"),
            Unexpected::UnsignedU64(n) => write!(f, "positive integer {n}"),
            Unexpected::UnsignedU128(n) => write!(f, "positive integer {n}"),
            Unexpected::NegI8(n) => write!(f, "negative integer {n}"),
            Unexpected::NegI16(n) => write!(f, "negative integer {n}"),
            Unexpected::NegI32(n) => write!(f, "negative integer {n}"),
            Unexpected::NegI64(n) => write!(f, "negative integer {n}"),
            Unexpected::NegI128(n) => write!(f, "negative integer {n}"),
            Unexpected::Str(s) => write!(f, "string {s}"),
            Unexpected::Bytes(_) => write!(f, "bytes array"),
            Unexpected::Null => write!(f, "null"),
            Unexpected::Undefined => write!(f, "undefined"),
            Unexpected::Option => write!(f, "Option value"),
            Unexpected::Array => write!(f, "array"),
            Unexpected::Map => write!(f, "map"),
        }
    }
}

/// Represents an explanation of what data a [`Visitor`] was expecting to
/// receive.
///
/// This is used as an argument to the `invalid_type`, `invalid_value`, and
/// `invalid_length` methods of the `Error` trait to build error messages. The
/// message should be a noun or noun phrase that completes the sentence "This
/// Visitor expects to receive ...", for example the message could be "an
/// integer between 0 and 64". The message should not be capitalized and should
/// not end with a period.
///
/// Within the context of a `Visitor` implementation, the `Visitor` itself
/// (`&self`) is an implementation of this trait.
///
/// Outside of a `Visitor`, `&"..."` can be used.
/// ```
pub trait Expected {
    /// Format an explanation of what data was being expected.
    ///
    /// # Errors
    ///
    /// Same as [`Display::fmt`][fmt::Display::fmt()].
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<'de, T> Expected for T
where
    T: Visitor<'de>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.expecting(f)
    }
}

impl Expected for &str {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self)
    }
}

impl fmt::Display for dyn Expected + '_ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Expected::fmt(self, f)
    }
}

impl Error for crate::Error {
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        crate::Error::new(ErrorKind::Decode(msg.to_string()), 0)
    }

    fn malformed(byte_offset: usize) -> Self {
        crate::Error::new(ErrorKind::NotWellFormed, byte_offset)
    }
}

/// A **data structure** that can be decoded from the CBOR data format.
///
/// This library provides `Decode` implementations for many Rust primitive and
/// standard library types. The complete list is [here][crate::decode].
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed by `Self` when decoded. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait Decode<'de>: Sized {
    /// Decode this value from the given decoder.
    ///
    /// # Errors
    ///
    /// Invalid values, invalid types, malformed data, and other errors returned
    /// from the [`Decoder`].
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>;

    /// Decodes a value into `self` from the given [`Decoder`].
    ///
    /// The purpose of this method is to allow the decoder to reuse
    /// resources and avoid copies. As such, if this method returns an error,
    /// `self` will be in an indeterminate state where some parts of the struct
    /// have been overwritten. Although whatever state that is will be
    /// memory-safe.
    ///
    /// This is generally useful when repeatedly decoding values that
    /// are processed one at a time, where the value of `self` doesn't matter
    /// when the next decode occurs.
    ///
    /// If you manually implement this, your recursive decodes should use
    /// `decode_in_place`.
    ///
    /// This method is stable and an official public API, but hidden from the
    /// documentation because it is almost never what newbies are looking for.
    /// Showing it in rustdoc would cause it to be featured more prominently
    /// than it deserves.
    #[doc(hidden)]
    fn decode_in_place<D>(decoder: D, place: &mut Self) -> Result<(), D::Error>
    where
        D: Decoder<'de>,
    {
        *place = Decode::decode(decoder)?;
        Ok(())
    }
}

/// `DecodeSeed` is the stateful form of the `Decode` trait. If you
/// ever find yourself looking for a way to pass data into a `Decode` impl,
/// this trait is the way to do it.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed by `Self` when decoded. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
#[expect(clippy::module_name_repetitions)]
pub trait DecodeSeed<'de>: Sized {
    /// The type produced by using this seed.
    type Value;

    /// Equivalent to the more common [`Decode::decode`] method, except
    /// with some initial piece of data (the seed) passed in.
    ///
    /// # Errors
    ///
    /// Similar to [`Decode::decode`].
    fn decode<D>(self, decoder: D) -> Result<Self::Value, D::Error>
    where
        D: Decoder<'de>;
}

impl<'de, T> DecodeSeed<'de> for PhantomData<T>
where
    T: Decode<'de>,
{
    type Value = T;

    #[inline]
    fn decode<D>(self, decoder: D) -> Result<T, D::Error>
    where
        D: Decoder<'de>,
    {
        T::decode(decoder)
    }
}

/// Decodes data into the CBOR data model.
///
/// In most cases, the [`Decoder::decode_any()`] method is called in a
/// [`Decode::decode()`] implementation. The `decode_any()` reads the binary
/// data into a CBOR value and then calls the corresponding [`Visitor`]'s
/// method. The `Visitor` will then take the decoded value and return an
/// instance of the decoding type.
///
/// For instance, imagine there is a type named `Foo` which implements `Decode`.
/// Also, suppose a `Visitor` type named `FooVisitor` exists. In `Foo`'s
/// `decode()` method, the `Decoder`'s `decode_any()` method is called with an
/// instance of `FooVisitor` as the argument.
///
/// If the decoder decodes a positive integer that can be represented by a `u8`
/// from the binary data, the decoder will call [`Visitor::visit_u8()`].
/// Assuming `FooVisitor` has implemented `visit_u8()`, then it will attempt to
/// map that `u8` value into a `Foo` instance.
///
/// # `decode_any()`
///
/// `decode_any()` is the main method which most [`Decode::decode()`]
/// implementations will call. It decodes the next CBOR value and calls the
/// corresponding [`Visitor`] method.
///
/// # Other decode_*() methods
///
/// While `decode_any()` works for most types, there are some types which
/// require unqiue handling. For instance, if a field is an `Option<T>`, then
/// `decode_option()` should be called.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed from the input when decoding. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait Decoder<'de>: Sized {
    /// The error type that can be returned if some error occurs during
    /// decoding.
    type Error: Error;

    /// Decodes the next CBOR value.
    ///
    /// # Errors
    ///
    /// Any error with decoding will be returned as an error including invalid
    /// types, invalid values, end of file, and so forth.
    fn decode_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>;

    /// Decodes the next value as an `Option<T>`.
    ///
    /// # Errors
    ///
    /// Any error with decoding will be returned as an error including invalid
    /// types, invalid values, end of file, and so forth.
    fn decode_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }
}

/// This trait represents a visitor that walks through a decoder.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed by `Self::Value` when decoded. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait Visitor<'de>: Sized {
    /// Value produced by this visitor.
    type Value;

    /// Format a message stating what data this Visitor expects to receive.
    ///
    /// This is used in error messages. The message should complete the sentence
    /// "This Visitor expects to receive ...", for example the message could be
    /// "an integer between 0 and 64". The message should not be capitalized and
    /// should not end with a period.
    ///
    /// # Errors
    ///
    /// Errors when formatting a message can be returned.
    fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;

    /// The input contains a boolean.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_bool<E>(self, v: bool) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::Bool(v), &self))
    }

    /// The input contains an `i8`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_i8<E>(self, v: i8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::NegI8(v), &self))
    }

    /// The input contains an `i16`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_i16<E>(self, v: i16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::NegI16(v), &self))
    }

    /// The input contains an `i32`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_i32<E>(self, v: i32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::NegI32(v), &self))
    }

    /// The input contains an `i64`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::NegI64(v), &self))
    }

    /// The input contains an `i128`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_i128<E>(self, v: i128) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::NegI128(v), &self))
    }

    /// The input contains an `u8`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_u8<E>(self, v: u8) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::UnsignedU8(v), &self))
    }

    /// The input contains an `u16`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_u16<E>(self, v: u16) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::UnsignedU16(v), &self))
    }

    /// The input contains an `u32`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::UnsignedU32(v), &self))
    }

    /// The input contains an `u64`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::UnsignedU64(v), &self))
    }

    /// The input contains an `u128`.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_u128<E>(self, v: u128) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::UnsignedU128(v), &self))
    }

    /// The input contains a string. The lifetime of the string is ephemeral and
    /// it may be destroyed after this method returns.
    ///
    /// This method allows the `Decoder` to avoid a copy by retaining ownership
    /// of any buffered data.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::Str(v), &self))
    }

    /// The input contains a string that lives at least as long as the
    /// `Decoder`.
    ///
    /// This enables zero-copy deserialization of strings in some formats.
    ///
    /// The default implementation forwards to `visit_str`.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    #[inline]
    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_str(v)
    }

    /// The input contains a byte array. The lifetime of the byte array is
    /// ephemeral and it may be destroyed after this method returns.
    ///
    /// This method allows the `Decoder` to avoid a copy by retaining
    /// ownership of any buffered data.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::Bytes(v), &self))
    }

    /// The input contains a byte array that lives at least as long as the
    /// `Decoder`.
    ///
    /// This enables zero-copy deserialization of bytes in some formats.
    ///
    /// The default implementation forwards to `visit_bytes`.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    #[inline]
    fn visit_borrowed_bytes<E>(self, v: &'de [u8]) -> Result<Self::Value, E>
    where
        E: Error,
    {
        self.visit_bytes(v)
    }

    /// The input contains a `null` value.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_null<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::Null, &self))
    }

    /// The input contains an `undefined` value.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_undefined<E>(self) -> Result<Self::Value, E>
    where
        E: Error,
    {
        Err(Error::invalid_type(Unexpected::Undefined, &self))
    }

    /// The input contains an optional that is present.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_some<D>(self, _decoder: D) -> Result<Self::Value, D::Error>
    where
        D: Decoder<'de>,
    {
        Err(Error::invalid_type(Unexpected::Option, &self))
    }

    /// The input contains an array of elements.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_arr<A>(self, _arr: A) -> Result<Self::Value, A::Error>
    where
        A: ArrAccess<'de>,
    {
        Err(Error::invalid_type(Unexpected::Array, &self))
    }

    /// The input contains a key-value map.
    ///
    /// The default implementation fails with a type error.
    ///
    /// # Errors
    ///
    /// Any error encountered during decoding or when creating the `Self::Value`
    /// type can be returned.
    fn visit_map<A>(self, _map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        Err(Error::invalid_type(Unexpected::Map, &self))
    }
}

/// Provides a [`Visitor`] access to each element of an array in the input.
///
/// This is a trait that a [`Decoder`] passes to a [`Visitor`] implementation,
/// which decodes each item in an array.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed by decoded array elements. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait ArrAccess<'de> {
    /// Error type that can be returned if some error occurs during
    /// decoding.
    type Error: Error;

    /// Returns `Ok(Some(value))` for the next value in the array, or `Ok(None)`
    /// if there are no more remaining items.
    ///
    /// `Decode` implementations should typically use `ArrAccess::next_element`
    /// instead.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>;

    /// Returns `Ok(Some(value))` for the next value in the array, or `Ok(None)`
    /// if there are no more remaining items.
    ///
    /// This method exists as a convenience for `Decode` implementations.
    /// `ArrAccess` implementations should not override the default behavior.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    #[inline]
    fn next_element<T>(&mut self) -> Result<Option<T>, Self::Error>
    where
        T: Decode<'de>,
    {
        self.next_element_seed(PhantomData)
    }

    /// Returns the number of elements remaining in the sequence, if known.
    #[inline]
    fn size_hint(&self) -> Option<usize> {
        None
    }
}

impl<'de, A> ArrAccess<'de> for &mut A
where
    A: ?Sized + ArrAccess<'de>,
{
    type Error = A::Error;

    #[inline]
    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: DecodeSeed<'de>,
    {
        (**self).next_element_seed(seed)
    }

    #[inline]
    fn next_element<T>(&mut self) -> Result<Option<T>, Self::Error>
    where
        T: Decode<'de>,
    {
        (**self).next_element()
    }

    #[inline]
    fn size_hint(&self) -> Option<usize> {
        (**self).size_hint()
    }
}

type EntrySeedResult<K, V, E> = Result<Option<(K, V)>, E>;

/// Provides a [`Visitor`] access to each entry of a map in the input.
///
/// This is a trait that a [`Decoder`] passes to a [`Visitor`] implementation.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed by decoded map entries. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait MapAccess<'de> {
    /// Error type that can be returned if some error occurs during
    /// deserialization.
    type Error: Error;

    /// Returns `Ok(Some(key))` for the next key in the map, or `Ok(None)`
    /// if there are no more remaining entries.
    ///
    /// `Decode` implementations should typically use `MapAccess::next_key` or
    /// `MapAccess::next_entry` instead.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DecodeSeed<'de>;

    /// Returns a `Ok(value)` for the next value in the map.
    ///
    /// `Decode` implementations should typically use `MapAccess::next_value`
    /// instead.
    ///
    /// # Panics
    ///
    /// Calling `next_value_seed` before `next_key_seed` is incorrect and is
    /// allowed to panic or return bogus results.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DecodeSeed<'de>;

    /// Returns `Ok(Some((key, value)))` for the next (key-value) pair in
    /// the map, or `Ok(None)` if there are no more remaining items.
    ///
    /// `MapAccess` implementations should override the default behavior if a
    /// more efficient implementation is possible.
    ///
    /// [`Decode`] implementations should typically use
    /// `MapAccess::next_entry` instead.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> EntrySeedResult<K::Value, V::Value, Self::Error>
    where
        K: DecodeSeed<'de>,
        V: DecodeSeed<'de>,
    {
        match self.next_key_seed(kseed)? {
            Some(key) => {
                let value = self.next_value_seed(vseed)?;
                Ok(Some((key, value)))
            }
            None => Ok(None),
        }
    }

    /// Returns `Ok(Some(key))` for the next key in the map, or `Ok(None)`
    /// if there are no more remaining entries.
    ///
    /// This method exists as a convenience for [`Decode`] implementations.
    /// `MapAccess` implementations should not override the default behavior.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    #[inline]
    fn next_key<K>(&mut self) -> Result<Option<K>, Self::Error>
    where
        K: Decode<'de>,
    {
        self.next_key_seed(PhantomData)
    }

    /// Returns a `Ok(value)` for the next value in the map.
    ///
    /// This method exists as a convenience for [`Decode`] implementations.
    /// `MapAccess` implementations should not override the default behavior.
    ///
    /// # Panics
    ///
    /// Calling `next_value` before `next_key` is incorrect and is allowed to
    /// panic or return bogus results.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    #[inline]
    fn next_value<V>(&mut self) -> Result<V, Self::Error>
    where
        V: Decode<'de>,
    {
        self.next_value_seed(PhantomData)
    }

    /// Returns `Ok(Some((key, value)))` for the next (key-value) pair in
    /// the map, or `Ok(None)` if there are no more remaining items.
    ///
    /// This method exists as a convenience for `Decode` implementations.
    /// `MapAccess` implementations should not override the default behavior.
    ///
    /// # Errors
    ///
    /// Similar to [`Decoder::decode_any`].
    #[inline]
    fn next_entry<K, V>(&mut self) -> Result<Option<(K, V)>, Self::Error>
    where
        K: Decode<'de>,
        V: Decode<'de>,
    {
        self.next_entry_seed(PhantomData, PhantomData)
    }

    /// Returns the number of entries remaining in the map, if known.
    #[inline]
    fn size_hint(&self) -> Option<usize> {
        None
    }
}

impl<'de, A> MapAccess<'de> for &mut A
where
    A: ?Sized + MapAccess<'de>,
{
    type Error = A::Error;

    #[inline]
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: DecodeSeed<'de>,
    {
        (**self).next_key_seed(seed)
    }

    #[inline]
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: DecodeSeed<'de>,
    {
        (**self).next_value_seed(seed)
    }

    #[inline]
    fn next_entry_seed<K, V>(
        &mut self,
        kseed: K,
        vseed: V,
    ) -> Result<Option<(K::Value, V::Value)>, Self::Error>
    where
        K: DecodeSeed<'de>,
        V: DecodeSeed<'de>,
    {
        (**self).next_entry_seed(kseed, vseed)
    }

    #[inline]
    fn next_entry<K, V>(&mut self) -> Result<Option<(K, V)>, Self::Error>
    where
        K: Decode<'de>,
        V: Decode<'de>,
    {
        (**self).next_entry()
    }

    #[inline]
    fn next_key<K>(&mut self) -> Result<Option<K>, Self::Error>
    where
        K: Decode<'de>,
    {
        (**self).next_key()
    }

    #[inline]
    fn next_value<V>(&mut self) -> Result<V, Self::Error>
    where
        V: Decode<'de>,
    {
        (**self).next_value()
    }

    #[inline]
    fn size_hint(&self) -> Option<usize> {
        (**self).size_hint()
    }
}

/// Converts an existing value into a [`Decoder`] from which other values can
/// be decoded.
///
/// # Lifetime
///
/// The `'de` lifetime of this trait is the lifetime of data that may be
/// borrowed from the resulting `Decoder`. See Serde's page [Understanding
/// deserializer lifetimes] for a more detailed explanation of these lifetimes.
///
/// [Understanding deserializer lifetimes]: https://serde.rs/lifetimes.html
pub trait IntoDecoder<'de, E: Error = value::Error> {
    /// The type of the decoder being converted into.
    type Decoder: Decoder<'de, Error = E>;

    /// Convert this value into a decoder.
    fn into_decoder(self) -> Self::Decoder;
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "std")]
    use crate::from_reader;
    use crate::{from_slice, ByteString, Result};

    use hex_literal::hex;

    #[cfg(all(feature = "alloc", not(feature = "std")))]
    use alloc::{
        collections::BTreeMap,
        string::{String, ToString},
        vec,
    };
    #[cfg(feature = "std")]
    use std::{collections::BTreeMap, string::String, vec};

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
    fn test_decode_neg_18_446_744_073_709_551_616() -> Result<()> {
        let input = hex!("3b ff ff ff ff ff ff ff ff");
        // assert_decode_i128_val!(-18_446_744_073_709_551_616, input);
        assert_eq!(
            -18_446_744_073_709_551_616,
            from_slice::<i128>(&input)?,
            "decoding i128 {input:X?}"
        );
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
    fn test_decode_empty_byte_str() -> Result<()> {
        let input = hex!("40");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[]);
        assert_eq!(*from_slice::<ByteString>(&input)?, vec![]);
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_empty_byte_str() -> Result<()> {
        use crate::ByteString;

        let input = hex!("40");
        assert_eq!(*from_reader::<_, ByteString>(&input[..])?, vec![]);
        Ok(())
    }

    #[test]
    fn test_decode_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(from_slice::<&[u8]>(&input)?, &[0x01, 0x02, 0x03, 0x04]);
        assert_eq!(
            *from_slice::<ByteString>(&input)?,
            vec![0x01, 0x02, 0x03, 0x04]
        );
        Ok(())
    }

    #[cfg(feature = "std")]
    #[test]
    fn test_decode_reader_byte_str() -> Result<()> {
        let input = hex!("44 01 02 03 04");
        assert_eq!(
            *from_reader::<_, ByteString>(&input[..])?,
            vec![0x01, 0x02, 0x03, 0x04]
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
    fn test_decode_bool_false() -> Result<()> {
        let input = hex!("f4");
        assert!(!from_slice::<bool>(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_bool_true() -> Result<()> {
        let input = hex!("f5");
        assert!(from_slice::<bool>(&input)?);
        Ok(())
    }

    #[test]
    fn test_decode_null() -> Result<()> {
        let input = hex!("f6");
        assert_eq!(from_slice::<Option<i8>>(&input)?, None);
        Ok(())
    }

    #[test]
    fn test_decode_option() -> Result<()> {
        let input = hex!("f5");
        assert_eq!(from_slice::<Option<bool>>(&input)?, Some(true));
        Ok(())
    }

    #[test]
    fn test_decode_empty_arr() -> Result<()> {
        let input = hex!("80");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, vec![]);
        Ok(())
    }

    #[test]
    fn test_decode_arr() -> Result<()> {
        let input = hex!("83 01 02 03");
        assert_eq!(from_slice::<Vec<u8>>(&input)?, vec![1, 2, 3]);
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
            vec![
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25
            ],
            from_slice::<Vec<u8>>(&input)?
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
}
