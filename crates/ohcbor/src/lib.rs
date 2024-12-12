//! # OhCbor
//!
//! OhCbor is a library which decodes and encodes in the [Concise Binary
//! Object Representation (CBOR)][cbor] data format. CBOR is specified in [RFC
//! 8949][rfc_8949]. Imagine starting with the JSON data model, making it more
//! efficient by using a binary (instead of plain text) format, and adding some
//! extensibility to allow more types.
//!
//! It uses the [Serde][serde] library to serialize and deserialize CBOR data.
//! It is similar to [Serde JSON][serde_json] in terms of functionality and
//! implementation.
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
//! [serde_json]: https://github.com/serde-rs/json

#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(docsrs, feature(doc_cfg))]

#[cfg(all(feature = "alloc", not(feature = "std")))]
extern crate alloc;

mod de;
mod error;

pub mod read;

#[doc(inline)]
pub use de::{from_slice, Deserializer};
#[doc(inline)]
pub use error::{Error, ErrorKind, Result};

#[doc(inline)]
#[cfg(feature = "std")]
pub use de::from_reader;

const IB_UINT_MIN: u8 = 0b0000_0000;
const IB_SINT_MIN: u8 = 0b0010_0000;
const IB_BYTE_STR_MIN: u8 = 0b0100_0000;
const IB_TEXT_STR_MIN: u8 = 0b0110_0000;
const IB_ARRAY_MIN: u8 = 0b1000_0000;
const IB_MAP_MIN: u8 = 0b1010_0000;
const IB_TAG_MIN: u8 = 0b1100_0000;
const IB_BOOL_FALSE: u8 = 0b1111_0100;
const IB_BOOL_TRUE: u8 = 0b1111_0101;
const IB_NULL: u8 = 0b1111_0110;
