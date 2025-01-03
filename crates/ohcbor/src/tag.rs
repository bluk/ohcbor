//! Tag helpers.

use core::marker::PhantomData;

use crate::{
    decode::{Decode, Visitor},
    encode::{self, Encode},
};

/// Tag number
pub type Num = u64;

/// Tag number and content.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tag<T> {
    num: Num,
    content: T,
}

impl<T> Tag<T> {
    /// Construct a new tag.
    #[inline]
    #[must_use]
    pub const fn new(num: Num, content: T) -> Self {
        Self { num, content }
    }

    /// Return the tag content.
    #[inline]
    #[must_use]
    pub fn content(&self) -> &T {
        &self.content
    }

    /// Return the tag number.
    #[inline]
    #[must_use]
    pub fn num(&self) -> Num {
        self.num
    }
}

impl<T> Encode for Tag<T>
where
    T: Encode,
{
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: encode::Encoder,
    {
        encoder.encode_tag(self.num, &self.content)
    }
}

impl<'de, T> Decode<'de> for Tag<T>
where
    T: Decode<'de>,
{
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: crate::decode::Decoder<'de>,
    {
        struct TagVisitor<T> {
            ty: PhantomData<T>,
        }

        impl<'de, T> Visitor<'de> for TagVisitor<T>
        where
            T: Decode<'de>,
        {
            type Value = Tag<T>;

            fn expecting(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.write_str("tag")
            }

            fn visit_tag<D>(self, tag_num: Num, decoder: D) -> Result<Self::Value, D::Error>
            where
                D: crate::decode::Decoder<'de>,
            {
                let content = T::decode(decoder)?;

                Ok(Tag {
                    num: tag_num,
                    content,
                })
            }
        }

        decoder.decode_any(TagVisitor { ty: PhantomData })
    }
}
