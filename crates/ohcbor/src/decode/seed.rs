use crate::decode::{Decode, DecodeSeed, Decoder};

/// A [`DecodeSeed`] helper for implementing
/// [`decode_in_place`][decode_in_place] Visitors.
///
/// Wraps a mutable reference and calls [`decode_in_place`][decode_in_place] on
/// it.
///
/// [decode_in_place]: crate::decode::Decode::decode_in_place()
pub(crate) struct InPlaceSeed<'a, T>(pub &'a mut T);

impl<'de, T> DecodeSeed<'de> for InPlaceSeed<'_, T>
where
    T: Decode<'de>,
{
    type Value = ();

    fn decode<D>(self, decoder: D) -> Result<Self::Value, D::Error>
    where
        D: Decoder<'de>,
    {
        T::decode_in_place(decoder, self.0)
    }
}
