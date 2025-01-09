//! Byte string which helps with the decoding of binary data.

use core::{
    borrow::{Borrow, BorrowMut},
    cmp, fmt,
    ops::{Deref, DerefMut},
};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{string::String, vec::Vec};
#[cfg(feature = "std")]
use std::{string::String, vec::Vec};

use crate::{
    decode::{ArrAccess, Decode, DecodeSeed, Decoder, IndefiniteLenItemAccess, Visitor},
    encode::{Encode, Encoder},
};

/// A sequence of bytes like a `Vec<u8>`.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ByteString(Vec<u8>);

impl AsRef<[u8]> for ByteString {
    fn as_ref(&self) -> &[u8] {
        &self.0
    }
}

impl AsMut<[u8]> for ByteString {
    fn as_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl Borrow<[u8]> for ByteString {
    fn borrow(&self) -> &[u8] {
        &self.0
    }
}

impl BorrowMut<[u8]> for ByteString {
    fn borrow_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}

impl fmt::Debug for ByteString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl Deref for ByteString {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ByteString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> From<&'a [u8]> for ByteString {
    fn from(value: &'a [u8]) -> Self {
        Self(Vec::from(value))
    }
}

impl<'a> From<&'a str> for ByteString {
    fn from(value: &'a str) -> Self {
        Self(Vec::from(value))
    }
}

impl From<String> for ByteString {
    fn from(value: String) -> Self {
        Self(Vec::from(value))
    }
}

impl From<Vec<u8>> for ByteString {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl Encode for ByteString {
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.encode_bytes(&self.0)
    }
}

impl<'de> Decode<'de> for ByteString {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: Decoder<'de>,
    {
        struct ByteStringVisitor;

        impl<'de> Visitor<'de> for ByteStringVisitor {
            type Value = ByteString;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("a byte string")
            }

            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(ByteString::from(v))
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

                Ok(ByteString::from(bytes.0))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: crate::decode::Error,
            {
                Ok(ByteString::from(v))
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

                Ok(ByteString::from(s.0))
            }

            fn visit_arr<V>(self, mut visitor: V) -> Result<Self::Value, V::Error>
            where
                V: ArrAccess<'de>,
            {
                let capacity = cmp::min(visitor.size_hint().unwrap_or_default(), 4096);
                let mut bytes = Vec::with_capacity(capacity);

                while let Some(b) = visitor.next_element()? {
                    bytes.push(b);
                }

                Ok(ByteString::from(bytes))
            }
        }

        decoder.decode_any(ByteStringVisitor)
    }
}

impl ByteString {
    /// Returns the inner vector.
    #[inline]
    #[must_use]
    pub fn into_vec(self) -> Vec<u8> {
        self.0
    }
}
