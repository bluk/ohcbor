//! Simple value

use core::{fmt, ops::Deref};

use crate::{
    decode::{self, Decode, Visitor},
    encode::Encode,
};

/// Boolean `false`
pub const SIMPLE_VALUE_FALSE: u8 = 20;
/// Boolean `true`
pub const SIMPLE_VALUE_TRUE: u8 = 21;
/// `null` value
pub const SIMPLE_VALUE_NULL: u8 = 22;
/// `undefined` value
pub const SIMPLE_VALUE_UNDEFINED: u8 = 23;

/// Simple value
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Simple(pub u8);

impl Simple {
    /// Null
    pub const NULL: Simple = Simple::new(SIMPLE_VALUE_NULL);
    /// True
    pub const TRUE: Simple = Simple::new(SIMPLE_VALUE_TRUE);
    /// False
    pub const FALSE: Simple = Simple::new(SIMPLE_VALUE_FALSE);
    /// Undefined
    pub const UNDEFINED: Simple = Simple::new(SIMPLE_VALUE_UNDEFINED);

    /// Construct a simple value from the numerical value
    #[inline]
    #[must_use]
    pub const fn new(value: u8) -> Self {
        Self(value)
    }

    /// Returns the simple value as a boolean. Return `None` if the simple value
    /// is not a boolean.
    #[must_use]
    pub const fn as_bool(self) -> Option<bool> {
        match self.0 {
            SIMPLE_VALUE_FALSE => Some(false),
            SIMPLE_VALUE_TRUE => Some(true),
            _ => None,
        }
    }

    /// Returns true if the simple value is the `null` value.
    #[must_use]
    pub const fn is_null(self) -> bool {
        self.0 == SIMPLE_VALUE_NULL
    }

    /// Returns true if the simple value is the `undefined` value.
    #[must_use]
    pub const fn is_undefined(self) -> bool {
        self.0 == SIMPLE_VALUE_UNDEFINED
    }
}

impl fmt::Display for Simple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

impl From<u8> for Simple {
    fn from(value: u8) -> Self {
        Simple::new(value)
    }
}

impl From<Simple> for u8 {
    fn from(value: Simple) -> Self {
        value.0
    }
}

impl AsRef<u8> for Simple {
    fn as_ref(&self) -> &u8 {
        &self.0
    }
}

impl Deref for Simple {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Encode for Simple {
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: crate::encode::Encoder,
    {
        encoder.encode_simple(*self)
    }
}

impl<'de> Decode<'de> for Simple {
    fn decode<D>(decoder: D) -> Result<Self, D::Error>
    where
        D: decode::Decoder<'de>,
    {
        struct SimpleVisitor;

        impl Visitor<'_> for SimpleVisitor {
            type Value = Simple;

            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("simple value")
            }

            fn visit_simple<E>(self, v: Simple) -> Result<Self::Value, E>
            where
                E: decode::Error,
            {
                Ok(v)
            }
        }

        decoder.decode_any(SimpleVisitor)
    }
}
