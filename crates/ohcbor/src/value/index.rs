//! Indexes into the [Value] type.

use super::Value;
use core::ops;

#[cfg(all(feature = "alloc", not(feature = "std")))]
use alloc::string::{String, ToString};
#[cfg(feature = "std")]
use std::string::{String, ToString};

/// Indexes into the [Value] type.
pub trait Index {
    /// If possible, returns a reference to the value using `&self` as an index for the [Value] parameter.
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value>;

    /// If possible, returns a mutable reference to the value using `&self` as an index for the [Value] parameter.
    fn index_mut<'a>(&self, v: &'a mut Value) -> Option<&'a mut Value>;
}

impl Index for usize {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Array(ref a) => a.get(*self),
            _ => None,
        }
    }

    fn index_mut<'a>(&self, v: &'a mut Value) -> Option<&'a mut Value> {
        match v {
            Value::Array(ref mut a) => a.get_mut(*self),
            _ => None,
        }
    }
}

impl Index for Value {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Map(ref m) => m.get(self),
            _ => None,
        }
    }

    fn index_mut<'a>(&self, v: &'a mut Value) -> Option<&'a mut Value> {
        match v {
            Value::Map(ref mut m) => m.get_mut(self),
            _ => None,
        }
    }
}

impl Index for str {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        match v {
            Value::Map(ref m) => m.get(&Value::TextStr(self.to_string())),
            _ => None,
        }
    }

    fn index_mut<'a>(&self, v: &'a mut Value) -> Option<&'a mut Value> {
        match v {
            Value::Map(ref mut m) => m.get_mut(&Value::TextStr(self.to_string())),
            _ => None,
        }
    }
}

impl Index for String {
    fn index<'a>(&self, v: &'a Value) -> Option<&'a Value> {
        self[..].index(v)
    }

    fn index_mut<'a>(&self, v: &'a mut Value) -> Option<&'a mut Value> {
        self[..].index_mut(v)
    }
}

impl<T> Index for &T
where
    T: Index + ?Sized,
{
    fn index<'a>(&self, val: &'a Value) -> Option<&'a Value> {
        (*self).index(val)
    }

    fn index_mut<'a>(&self, val: &'a mut Value) -> Option<&'a mut Value> {
        (*self).index_mut(val)
    }
}

impl<I> ops::Index<I> for Value
where
    I: Index,
{
    type Output = Value;

    fn index(&self, index: I) -> &Value {
        self.get(index).expect("invalid index")
    }
}

impl<I> ops::IndexMut<I> for Value
where
    I: Index,
{
    fn index_mut(&mut self, index: I) -> &mut Value {
        self.get_mut(index).expect("invalid index")
    }
}
