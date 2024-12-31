use core::{
    cell::{Cell, RefCell},
    cmp::Reverse,
    fmt,
    num::{Saturating, Wrapping},
};

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    collections::{BTreeMap, BTreeSet, BinaryHeap, LinkedList, VecDeque},
    string::String,
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque},
    string::String,
    sync::{
        atomic::{
            AtomicBool, AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicIsize, AtomicU16,
            AtomicU32, AtomicU64, AtomicU8, AtomicUsize, Ordering,
        },
        Mutex, RwLock,
    },
    vec::Vec,
};

use crate::encode::{Encode, EncodeArr, Encoder, Error};

macro_rules! primitive_impl {
    ($ty:ident, $method:ident $($cast:tt)*) => {
        impl Encode for $ty {
            #[inline]
            fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
            where
                E: Encoder,
            {
                encoder.$method(*self $($cast)*)
            }
        }
    }
}

primitive_impl!(bool, encode_bool);
primitive_impl!(isize, encode_i64 as i64);
primitive_impl!(i8, encode_i8);
primitive_impl!(i16, encode_i16);
primitive_impl!(i32, encode_i32);
primitive_impl!(i64, encode_i64);
primitive_impl!(i128, encode_i128);
primitive_impl!(usize, encode_u64 as u64);
primitive_impl!(u8, encode_u8);
primitive_impl!(u16, encode_u16);
primitive_impl!(u32, encode_u32);
primitive_impl!(u64, encode_u64);
primitive_impl!(u128, encode_u128);
primitive_impl!(f32, encode_f32);
primitive_impl!(f64, encode_f64);

impl Encode for str {
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.encode_str(self)
    }
}

#[cfg(any(feature = "std", feature = "alloc"))]
#[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
impl Encode for String {
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.encode_str(self)
    }
}

impl Encode for fmt::Arguments<'_> {
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.collect_str(self)
    }
}

impl<T> Encode for Option<T>
where
    T: Encode,
{
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self {
            Some(v) => v.encode(encoder),
            None => encoder.encode_none(),
        }
    }
}

// Does not require T: Encode.
impl<T> Encode for [T; 0] {
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.encode_arr(Some(0))?.end()
    }
}

macro_rules! array_impls {
    ($($len:tt)+) => {
        $(
            impl<T> Encode for [T; $len]
            where
                T: Encode,
            {
                #[inline]
                fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
                where
                    E: Encoder,
                {
                    let mut arr = encoder.encode_arr(Some($len))?;
                    for e in self {
                        arr.encode_element(e)?;
                    }
                    arr.end()
                }
            }
        )+
    }
}

array_impls! {
    1 2 3 4 5 6 7 8 9 10
    11 12 13 14 15 16 17 18 19 20
    21 22 23 24 25 26 27 28 29 30
    31 32
}

impl<T> Encode for [T]
where
    T: Encode,
{
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        encoder.collect_arr(self)
    }
}

macro_rules! arr_impl {
    (
        $(#[$attr:meta])*
        $ty:ident <T $(: $tbound1:ident $(+ $tbound2:ident)*)* $(, $typaram:ident : $bound:ident)*>
    ) => {
        $(#[$attr])*
        impl<T $(, $typaram)*> Encode for $ty<T $(, $typaram)*>
        where
            T: Encode,
        {
            #[inline]
            fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
            where
                E: Encoder,
            {
                encoder.collect_arr(self)
            }
        }
    }
}

arr_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BinaryHeap<T: Ord>
}

arr_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BTreeSet<T: Ord>
}

arr_impl! {
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    HashSet<T: Eq + Hash, H: BuildHasher>
}

arr_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    LinkedList<T>
}

arr_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    Vec<T>
}

arr_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    VecDeque<T>
}

macro_rules! tuple_impls {
    ($($len:expr => ($($n:tt $name:ident)+))+) => {
        $(
            #[cfg_attr(docsrs, doc(hidden))]
            impl<$($name),+> Encode for ($($name,)+)
            where
                $($name: Encode,)+
            {
                tuple_impl_body!($len => ($($n)+));
            }
        )+
    };
}

macro_rules! tuple_impl_body {
    ($len:expr => ($($n:tt)+)) => {
        #[inline]
        fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
        where
            E: Encoder,
        {
            let mut arr = encoder.encode_arr(Some($len))?;
            $(
                arr.encode_element(&self.$n)?;
            )+
            arr.end()
        }
    };
}

#[cfg_attr(docsrs, doc(fake_variadic))]
#[cfg_attr(
    docsrs,
    doc = "This trait is implemented for tuples up to 16 items long."
)]
impl<T> Encode for (T,)
where
    T: Encode,
{
    tuple_impl_body!(1 => (0));
}

tuple_impls! {
    2 => (0 T0 1 T1)
    3 => (0 T0 1 T1 2 T2)
    4 => (0 T0 1 T1 2 T2 3 T3)
    5 => (0 T0 1 T1 2 T2 3 T3 4 T4)
    6 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
    7 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
    8 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
    9 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
    10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
    11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
    12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
    13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
    14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
    15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
    16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}

macro_rules! map_impl {
    (
        $(#[$attr:meta])*
        $ty:ident <K $(: $kbound1:ident $(+ $kbound2:ident)*)*, V $(, $typaram:ident : $bound:ident)*>
    ) => {
        $(#[$attr])*
        impl<K, V $(, $typaram)*> Encode for $ty<K, V $(, $typaram)*>
        where
            K: Encode,
            V: Encode,
        {
            #[inline]
            fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
            where
                E: Encoder,
            {
                encoder.collect_map(self)
            }
        }
    }
}

map_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    BTreeMap<K: Ord, V>
}

map_impl! {
    #[cfg(feature = "std")]
    #[cfg_attr(docsrs, doc(cfg(feature = "std")))]
    HashMap<K: Eq + Hash, V, H: BuildHasher>
}

macro_rules! deref_impl {
    (
        $(#[$attr:meta])*
        <$($desc:tt)+
    ) => {
        $(#[$attr])*
        impl <$($desc)+ {
            #[inline]
            fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
            where
                E: Encoder,
            {
                (**self).encode(encoder)
            }
        }
    };
}

deref_impl! {
    <'a, T> Encode for &'a T where T: ?Sized + Encode
}

deref_impl! {
    <'a, T> Encode for &'a mut T where T: ?Sized + Encode
}

deref_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    <T> Encode for Box<T> where T: ?Sized + Encode
}

deref_impl! {
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[cfg_attr(docsrs, doc(cfg(any(feature = "std", feature = "alloc"))))]
    <'a, T> Encode for Cow<'a, T> where T: ?Sized + Encode + ToOwned
}

macro_rules! nonzero_integers {
    ($($T:ident,)+) => {
        $(
            impl Encode for core::num::$T {
                fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
                where
                    E: Encoder,
                {
                    self.get().encode(encoder)
                }
            }
        )+
    }
}

nonzero_integers! {
    NonZeroU8,
    NonZeroU16,
    NonZeroU32,
    NonZeroU64,
    NonZeroU128,
    NonZeroUsize,
}

nonzero_integers! {
    NonZeroI8,
    NonZeroI16,
    NonZeroI32,
    NonZeroI64,
    NonZeroI128,
    NonZeroIsize,
}

impl<T> Encode for Cell<T>
where
    T: Encode + Copy,
{
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        self.get().encode(encoder)
    }
}

impl<T> Encode for RefCell<T>
where
    T: ?Sized + Encode,
{
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self.try_borrow() {
            Ok(value) => value.encode(encoder),
            Err(_) => Err(E::Error::custom("already mutably borrowed")),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl<T> Encode for Mutex<T>
where
    T: ?Sized + Encode,
{
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self.lock() {
            Ok(locked) => locked.encode(encoder),
            Err(_) => Err(E::Error::custom("lock poison error while encoding")),
        }
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl<T> Encode for RwLock<T>
where
    T: ?Sized + Encode,
{
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        match self.read() {
            Ok(locked) => locked.encode(encoder),
            Err(_) => Err(E::Error::custom("lock poison error while encoding")),
        }
    }
}

impl<T> Encode for Wrapping<T>
where
    T: Encode,
{
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        self.0.encode(encoder)
    }
}

impl<T> Encode for Saturating<T>
where
    T: Encode,
{
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        self.0.encode(encoder)
    }
}

impl<T> Encode for Reverse<T>
where
    T: Encode,
{
    #[inline]
    fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
    where
        E: Encoder,
    {
        self.0.encode(encoder)
    }
}

#[cfg(feature = "std")]
macro_rules! atomic_impl {
    ($($ty:ident $size:expr)*) => {
        $(
            #[cfg(any(target_has_atomic = $size))]
            #[cfg_attr(docsrs, doc(cfg(all(feature = "std", target_has_atomic = $size))))]
            impl Encode for $ty {
                fn encode<E>(&self, encoder: E) -> Result<E::Ok, E::Error>
                where
                    E: Encoder,
                {
                    // Matches the atomic ordering used in libcore for the Debug impl
                    self.load(Ordering::Relaxed).encode(encoder)
                }
            }
        )*
    }
}

#[cfg(feature = "std")]
atomic_impl! {
    AtomicBool "8"
    AtomicI8 "8"
    AtomicI16 "16"
    AtomicI32 "32"
    AtomicIsize "ptr"
    AtomicU8 "8"
    AtomicU16 "16"
    AtomicU32 "32"
    AtomicUsize "ptr"
}

#[cfg(feature = "std")]
atomic_impl! {
    AtomicI64 "64"
    AtomicU64 "64"
}
