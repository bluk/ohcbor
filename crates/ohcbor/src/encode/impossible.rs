//! This module contains [`Impossible`] encoder and its implementations.

use core::marker::PhantomData;

use crate::encode::{self, Encode, EncodeArr, EncodeMap};

/// Helper type for implementing an [`Encoder`][crate::encode::Encoder] that
/// does not support encoding one of the compound types.
///
/// This type cannot be instantiated, but implements every one of the traits
/// corresponding to the `Encoder` compound types: [`EncodeArr`] and
/// [`EncodeMap`].
#[allow(missing_debug_implementations)]
pub struct Impossible<Ok, Error> {
    void: Void,
    ok: PhantomData<Ok>,
    error: PhantomData<Error>,
}

enum Void {}

impl<Ok, Error> EncodeArr for Impossible<Ok, Error>
where
    Error: encode::Error,
{
    type Ok = Ok;
    type Error = Error;

    fn encode_element<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Encode,
    {
        let _ = value;
        match self.void {}
    }

    fn end(self) -> Result<Ok, Error> {
        match self.void {}
    }
}

impl<Ok, Error> EncodeMap for Impossible<Ok, Error>
where
    Error: encode::Error,
{
    type Ok = Ok;
    type Error = Error;

    fn encode_key<T>(&mut self, key: &T) -> Result<(), Error>
    where
        T: ?Sized + Encode,
    {
        let _ = key;
        match self.void {}
    }

    fn encode_value<T>(&mut self, value: &T) -> Result<(), Error>
    where
        T: ?Sized + Encode,
    {
        let _ = value;
        match self.void {}
    }

    fn end(self) -> Result<Ok, Error> {
        match self.void {}
    }
}
