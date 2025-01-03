//! # OhCbor
//!
//! OhCbor is a library which decodes and encodes in the [Concise Binary
//! Object Representation (CBOR)][cbor] data format. CBOR is specified in [RFC
//! 8949][rfc_8949]. Imagine starting with the JSON data model, making it more
//! efficient by using a binary (instead of plain text) format, and adding some
//! extensibility via tagging data.
//!
//! # Forking Serde
//!
//! While this library started as a Serde implementation for CBOR, this library
//! is now a fork of Serde's traits and implementation specialized for CBOR.
//!
//! [`Decode`][decode::Decode] is a fork of the
//! [`Deserialize`][serde_deserialize] trait, [`Decoder`][decode::Decoder]
//! is a fork of the [`Deserializer`][serde_deserializer] trait, and so
//! forth.
//!
//! They were forked due to several unique properties of `CBOR` which are not
//! directly supported by Serde's data model. In this context, a `data model` is
//! defined as support for data types such as strings, floating point numbers,
//! booleans, integers, arrays/sequences, maps, enums, and so forth. For
//! instance, JSON supports strings, numbers, objects, arrays, booleans, and
//! null. JSON does not directly support types like binary data (byte strings),
//! so JSON users have to use workarounds like base64 encoding the binary data
//! into a string to encode binary data,.
//!
//! Serde provides a framework to deserialize any data format into its own data
//! model. Then, Serde provides a way to map data from its own data model into a
//! Rust type. Serde's data model does not explicitly support concepts such as
//! CBOR's data tags or distinct values for `undefined` and `null`.
//!
//! ## Advantages of Forking
//!
//! * Support CBOR tags directly
//! * Support simple types like `null` and `undefined` with distinct values
//! * Removal of unit type, enums, and other Serde data model types which do not
//!   exist in CBOR
//!
//! ## Disadvantages of Forking
//!
//! * Types (which may already implement/derive Serde's `Serialize` or
//!   `Deserialize`) will need to implement/derive this crate's traits.
//! * Lack of ecosystem support. Serde features have been adopted by many
//!   libraries which this library will not have support for.
//! * Greater maintenance required for this library (e.g. new features or bug
//!   fixes to serde would need to be ported)
//!
//! ## License
//!
//! Licensed under either of [Apache License, Version 2.0][LICENSE_APACHE] or [MIT
//! License][LICENSE_MIT] at your option.
//!
//! ### Contributions
//!
//! Unless you explicitly state otherwise, any contribution intentionally submitted
//! for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
//! dual licensed as above, without any additional terms or conditions.
//!
//! [LICENSE_APACHE]: LICENSE-APACHE
//! [LICENSE_MIT]: LICENSE-MIT
//! [cbor]: https://cbor.io/
//! [rfc_8949]: https://www.rfc-editor.org/rfc/rfc8949.html
//! [serde]: https://serde.rs
//! [serde_deserialize]: https://docs.rs/serde/latest/serde/trait.Deserialize.html
//! [serde_deserializer]: https://docs.rs/serde/latest/serde/trait.Deserializer.html

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(all(feature = "alloc", not(feature = "std")))]
extern crate alloc;

pub mod decode;
pub mod encode;
mod error;

#[cfg(any(feature = "alloc", feature = "std"))]
mod bstring;
pub mod buf;
pub mod read;
pub mod simple;
pub mod tag;
pub mod write;

#[doc(inline)]
#[cfg(any(feature = "alloc", feature = "std"))]
pub use bstring::ByteString;
#[doc(inline)]
pub use error::{Error, ErrorKind, Result};
#[doc(inline)]
pub use simple::Simple;
#[doc(inline)]
pub use tag::Tag;

const IB_UINT_MIN: u8 = 0b0000_0000;
const IB_SINT_MIN: u8 = 0b0010_0000;
const IB_BYTE_STR_MIN: u8 = 0b0100_0000;
const IB_TEXT_STR_MIN: u8 = 0b0110_0000;
const IB_ARRAY_MIN: u8 = 0b1000_0000;
const IB_MAP_MIN: u8 = 0b1010_0000;
const IB_TAG_MIN: u8 = 0b1100_0000;
const IB_FP_SIMPLE_MIN: u8 = 0b1110_0000;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::{io, vec::Vec};

/// Decode an instance of `T` from the bytes of an [`io::Read`] type.
///
/// The entire [`io::Read`] source is consumed, and it is an error if there is
/// trailing data.
///
/// # Errors
///
/// Decoding can fail if the data is not valid, if the data cannot cannot be
/// decoded into an instance of `T`, if there is trailing data, and other IO
/// errors.
#[cfg(feature = "std")]
pub fn from_reader<R, T>(r: R) -> Result<T>
where
    R: io::Read,
    T: for<'de> decode::Decode<'de>,
{
    let mut de = decode::decoders::DecoderImpl::new(read::IoRead::new(r), Vec::new());
    let value = T::decode(&mut de)?;
    de.end()?;
    Ok(value)
}

/// Decode an instance of `T` from a slice of bytes.
///
/// The entire slice of bytes is consumed, and it is an error if there is
/// trailing data.
///
/// # Errors
///
/// Decoding can fail if the data is not valid, if the data cannot be decoded
/// into an instance of `T`, if there is trailing data, and other IO errors.
#[cfg(any(feature = "alloc", feature = "std"))]
pub fn from_slice<'a, T>(s: &'a [u8]) -> Result<T>
where
    T: decode::Decode<'a>,
{
    let mut de = decode::decoders::DecoderImpl::new(read::SliceRead::new(s), Vec::new());
    let value = T::decode(&mut de)?;
    de.end()?;
    Ok(value)
}

/// Encodes an instance of `T` into the writer `W` as CBOR data.
///
/// # Errors
///
/// Encoding can fail if `T`'s implementation of
/// [`Encode`][crate::encode::Encode] decides to fail, if or `T` contains
/// unsupported types for encoding.
#[cfg(feature = "std")]
#[inline]
pub fn to_writer<W, T>(writer: W, value: &T) -> Result<()>
where
    W: io::Write,
    T: ?Sized + encode::Encode,
{
    let mut enc = encode::encoders::Encoder::new(write::IoWrite::new(writer));
    value.encode(&mut enc)?;
    Ok(())
}

/// Encodes an instance of `T` into a new [Vec] as CBOR data.
///
/// # Errors
///
/// Encoding can fail if `T`'s implementation of
/// [`Encode`][crate::encode::Encode] decides to fail, if or `T` contains
/// unsupported types for encoding.
#[cfg(any(feature = "alloc", feature = "std"))]
#[inline]
pub fn to_vec<T>(value: &T) -> Result<Vec<u8>>
where
    T: ?Sized + encode::Encode,
{
    let mut writer = Vec::new();
    let mut enc = encode::encoders::Encoder::new(&mut writer);
    value.encode(&mut enc)?;
    Ok(writer)
}
