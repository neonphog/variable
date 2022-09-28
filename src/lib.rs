#![cfg_attr(not(feature = "std"), no_std)]
#![deny(warnings)]
#![deny(missing_docs)]
#![deny(unsafe_code)]
//! Variable - sometimes you just want to be able to use anything.

#[cfg(not(feature = "std"))]
#[macro_use]
extern crate alloc;

// lib facade
mod lib {
    pub mod core {
        #[cfg(not(feature = "std"))]
        pub use core::*;
        #[cfg(feature = "std")]
        pub use std::*;
    }

    pub use self::core::cmp;
    pub use self::core::fmt;
    pub use self::core::future::Future;
    pub use self::core::hash;
    pub use self::core::iter;
    pub use self::core::mem;
    pub use self::core::ops;
    pub use self::core::pin;
    pub use self::core::result;
    pub use self::core::slice;

    #[cfg(not(feature = "std"))]
    pub use alloc::vec::Vec;
    #[cfg(feature = "std")]
    pub use std::vec::Vec;

    #[cfg(not(feature = "std"))]
    pub use alloc::boxed::Box;
    #[cfg(feature = "std")]
    pub use std::boxed::Box;

    #[cfg(not(feature = "std"))]
    pub use alloc::borrow::Cow;
    #[cfg(feature = "std")]
    pub use std::borrow::Cow;

    #[cfg(not(feature = "std"))]
    pub use alloc::string::{String, ToString};
    #[cfg(feature = "std")]
    pub use std::string::{String, ToString};
}
pub(crate) use lib::*;

mod std_err {
    use crate::*;

    /// Stand-in Error Trait, given we are not including "std"
    /// (or the stand-in error trait from serde)
    pub trait Error: fmt::Debug + fmt::Display {
        /// The underlying cause of this error, if any.
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            None
        }
    }
}

/// Helper utility types.
pub mod util {
    #[cfg(all(not(feature = "std"), feature = "serde"))]
    pub use ::serde::de::StdError;
    #[cfg(feature = "std")]
    pub use ::std::error::Error as StdError;
    #[cfg(all(not(feature = "std"), not(feature = "serde")))]
    pub use std_err::Error as StdError;
}

mod prim;
pub use prim::*;

mod var;
pub use var::*;
