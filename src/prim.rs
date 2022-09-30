use crate::*;

/// Generic variable storing basic primitive types.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(Arc<PrimInner>);

impl util::StdError for Prim {}

impl From<&Prim> for Prim {
    #[inline]
    fn from(p: &Prim) -> Self {
        p.clone()
    }
}

impl From<&()> for Prim {
    #[inline]
    fn from(_: &()) -> Self {
        Self(Arc::new(PrimInner::N(N)))
    }
}

impl From<()> for Prim {
    #[inline]
    fn from(_: ()) -> Self {
        Self(Arc::new(PrimInner::N(N)))
    }
}

impl From<&bool> for Prim {
    #[inline]
    fn from(b: &bool) -> Self {
        Self(Arc::new(PrimInner::B(B(*b))))
    }
}

impl From<bool> for Prim {
    #[inline]
    fn from(b: bool) -> Self {
        Self(Arc::new(PrimInner::B(B(b))))
    }
}

impl From<String> for Prim {
    #[inline]
    fn from(s: String) -> Self {
        Self(Arc::new(PrimInner::S(S(s))))
    }
}

impl From<&'static str> for Prim {
    #[inline]
    fn from(s: &'static str) -> Self {
        Self(Arc::new(PrimInner::SS(SS(s))))
    }
}

impl From<Vec<u8>> for Prim {
    #[inline]
    fn from(d: Vec<u8>) -> Self {
        Self(Arc::new(PrimInner::D(D(d))))
    }
}

macro_rules! from_int {
    ($($t:ty)*) => {$(
        impl From<&$t> for Prim {
            #[inline]
            fn from(i: &$t) -> Self {
                Self(Arc::new(PrimInner::I(I(*i as i64))))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(i: $t) -> Self {
                Self(Arc::new(PrimInner::I(I(i as i64))))
            }
        }
    )*};
}
from_int!(i8 i16 i32 i64 isize);

macro_rules! from_uint {
    ($($t:ty)*) => {$(
        impl From<&$t> for Prim {
            #[inline]
            fn from(u: &$t) -> Self {
                Self(Arc::new(PrimInner::U(U(*u as u64))))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(u: $t) -> Self {
                Self(Arc::new(PrimInner::U(U(u as u64))))
            }
        }
    )*};
}
from_uint!(u8 u16 u32 u64 usize);

macro_rules! from_float {
    ($($t:ty)*) => {$(
        impl From<&$t> for Prim {
            #[inline]
            fn from(f: &$t) -> Self {
                Self(Arc::new(PrimInner::F(F::new(*f as f64))))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(f: $t) -> Self {
                Self(Arc::new(PrimInner::F(F::new(f as f64))))
            }
        }
    )*};
}
from_float!(f32 f64);

impl From<&Prim> for () {
    fn from(_: &Prim) -> Self {}
}

impl From<Prim> for () {
    fn from(_: Prim) -> Self {}
}

impl From<&Prim> for bool {
    fn from(p: &Prim) -> Self {
        match &*p.0 {
            PrimInner::B(B(b)) => *b,
            _ => false,
        }
    }
}

impl From<Prim> for bool {
    #[inline]
    fn from(p: Prim) -> Self {
        (&p).into()
    }
}

macro_rules! to_xint {
    ($($t:ty)*) => {$(
        impl From<&Prim> for $t {
            fn from(p: &Prim) -> Self {
                let i: i128 = match &*p.0 {
                    PrimInner::F(F(f)) => {
                        f.clamp(i128::MIN as f64, i128::MAX as f64) as i128
                    }
                    PrimInner::I(I(i)) => *i as i128,
                    PrimInner::U(U(u)) => *u as i128,
                    _ => 0,
                };
                i.clamp(<$t>::MIN as i128, <$t>::MAX as i128) as $t
            }
        }

        impl From<Prim> for $t {
            #[inline]
            fn from(p: Prim) -> Self {
                (&p).into()
            }
        }
    )*};
}
to_xint!(i8 u8 i16 u16 i32 u32 i64 u64 isize usize);

macro_rules! to_float {
    ($($t:ty)*) => {$(
        impl From<&Prim> for $t {
            fn from(p: &Prim) -> Self {
                match &*p.0 {
                    PrimInner::F(F(f)) => *f as $t,
                    PrimInner::I(I(i)) => *i as $t,
                    PrimInner::U(U(u)) => *u as $t,
                    _ => 0.0,
                }
            }
        }

        impl From<Prim> for $t {
            #[inline]
            fn from(p: Prim) -> Self {
                (&p).into()
            }
        }
    )*};
}
to_float!(f32 f64);

macro_rules! other_cmp {
    ($($t:ty)*) => {$(
        impl PartialEq<Prim> for $t {
            fn eq(&self, other: &Prim) -> bool {
                Prim::from(self).eq(other)
            }
        }

        impl PartialOrd<Prim> for $t {
            fn partial_cmp(&self, other: &Prim) -> Option<cmp::Ordering> {
                Prim::from(self).partial_cmp(other)
            }
        }

        impl PartialEq<$t> for Prim {
            fn eq(&self, other: &$t) -> bool {
                self.eq(&Prim::from(other))
            }
        }

        impl PartialOrd<$t> for Prim {
            fn partial_cmp(&self, other: &$t) -> Option<cmp::Ordering> {
                self.partial_cmp(&Prim::from(other))
            }
        }
    )*};
}
other_cmp!(() bool i8 u8 i16 u16 i32 u32 i64 u64 isize usize f32 f64);

impl PartialEq<Prim> for str {
    fn eq(&self, other: &Prim) -> bool {
        match &*other.0 {
            PrimInner::S(S(s)) => self.eq(s),
            PrimInner::SS(SS(s)) => (&self).eq(s),
            _ => false,
        }
    }
}

impl PartialEq<Prim> for &str {
    fn eq(&self, other: &Prim) -> bool {
        match &*other.0 {
            PrimInner::S(S(s)) => self.eq(s),
            PrimInner::SS(SS(s)) => self.eq(s),
            _ => false,
        }
    }
}

impl PartialEq<str> for Prim {
    fn eq(&self, other: &str) -> bool {
        match &*self.0 {
            PrimInner::S(S(s)) => s.eq(other),
            PrimInner::SS(SS(s)) => s.eq(&other),
            _ => false,
        }
    }
}

impl PartialEq<&str> for Prim {
    fn eq(&self, other: &&str) -> bool {
        match &*self.0 {
            PrimInner::S(S(s)) => s.eq(other),
            PrimInner::SS(SS(s)) => s.eq(other),
            _ => false,
        }
    }
}

impl fmt::Debug for Prim {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            PrimInner::N(_) => f.write_str("Prim::Unit"),
            PrimInner::B(B(b)) => write!(f, "Prim::Bool({:?})", b),
            PrimInner::S(S(s)) => write!(f, "Prim::String({:?})", s),
            PrimInner::SS(SS(s)) => write!(f, "Prim::String({:?})", s),
            PrimInner::D(D(d)) => write!(f, "Prim::Bytes({:?})", d),
            PrimInner::I(I(i)) => write!(f, "Prim::Int({:?})", i),
            PrimInner::U(U(u)) => write!(f, "Prim::Int({:?})", u),
            PrimInner::F(F(fl)) => write!(f, "Prim::Float({:?})", fl),
        }
    }
}

impl fmt::Display for Prim {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.as_str())
    }
}

impl Default for Prim {
    #[inline]
    fn default() -> Self {
        Self(Arc::new(PrimInner::N(N)))
    }
}

impl Prim {
    /// Construct a new default (unit) Prim instance.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the value tracked by this Prim instance.
    #[inline]
    pub fn set<P: Into<Prim>>(&mut self, p: P) {
        self.0 = (p.into()).0
    }

    /// Get this primitive as a str.
    pub fn as_str(&self) -> borrow::Cow<'_, str> {
        match &*self.0 {
            PrimInner::N(_) => "()".into(),
            PrimInner::B(B(b)) => format!("{}", b).into(),
            PrimInner::S(S(s)) => borrow::Cow::Borrowed(s),
            PrimInner::SS(SS(s)) => borrow::Cow::Borrowed(s),
            PrimInner::D(D(d)) => format!("[{} bytes]", d.len()).into(),
            PrimInner::I(I(i)) => format!("{}", i).into(),
            PrimInner::U(U(u)) => format!("{}", u).into(),
            PrimInner::F(F(f)) => format!("{}", f).into(),
        }
    }

    /// Get this primitive as a byte slice.
    pub fn as_slice(&self) -> borrow::Cow<'_, [u8]> {
        match &*self.0 {
            PrimInner::N(_) => (&b"()"[..]).into(),
            PrimInner::B(B(b)) => format!("{}", b).into_bytes().into(),
            PrimInner::S(S(s)) => borrow::Cow::Borrowed(s.as_bytes()),
            PrimInner::SS(SS(s)) => borrow::Cow::Borrowed(s.as_bytes()),
            PrimInner::D(D(d)) => borrow::Cow::Borrowed(d),
            PrimInner::I(I(i)) => format!("{}", i).into_bytes().into(),
            PrimInner::U(U(u)) => format!("{}", u).into_bytes().into(),
            PrimInner::F(F(f)) => format!("{}", f).into_bytes().into(),
        }
    }

    /// As any type that implements From<&Prim>.
    #[inline]
    pub fn as_<'a, T>(&'a self) -> T
    where
        T: From<&'a Prim>,
    {
        self.into()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct N;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct B(bool);

#[derive(Clone, PartialEq, Eq, Hash)]
struct S(String);

#[derive(Clone, PartialEq, Eq, Hash)]
struct SS(&'static str);

#[derive(Clone, PartialEq, Eq, Hash)]
struct D(Vec<u8>);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct I(i64);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct U(u64);

#[derive(Clone, Copy)]
struct F(f64);

// because of our careful new/ord/hash impls, we can apply this
impl Eq for F {}

fn bin_sort<const N: usize>(mut b: [u8; N], is_pos: bool) -> [u8; N] {
    if is_pos {
        b[0] ^= 0x80;
    } else {
        for i in b.iter_mut() {
            *i ^= 0xff;
        }
    }
    b
}

impl PartialOrd for F {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for F {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let a = bin_sort(self.0.to_be_bytes(), self.0 >= 0.0);
        let b = bin_sort(other.0.to_be_bytes(), other.0 >= 0.0);
        (&a[..]).cmp(&b[..])
    }
}

impl PartialEq for F {
    fn eq(&self, other: &Self) -> bool {
        matches!(self.cmp(other), cmp::Ordering::Equal)
    }
}

impl hash::Hash for F {
    fn hash<H>(&self, state: &mut H)
    where
        H: hash::Hasher,
    {
        self.0.to_be_bytes().hash(state);
    }
}

impl F {
    pub fn new(f: f64) -> Self {
        Self(if f.is_nan() {
            0.0
        } else if f.is_infinite() && f.is_sign_positive() {
            f64::MAX
        } else if f.is_infinite() {
            f64::MIN
        } else {
            f
        })
    }
}

#[allow(clippy::derive_hash_xor_eq)]
#[derive(Clone, Hash)]
enum PrimInner {
    N(N),
    B(B),
    S(S),
    SS(SS),
    D(D),
    I(I),
    U(U),
    F(F),
}

impl cmp::PartialEq for PrimInner {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        matches!(self.cmp(other), cmp::Ordering::Equal)
    }
}

impl cmp::Eq for PrimInner {}

impl cmp::PartialOrd for PrimInner {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl cmp::Ord for PrimInner {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (PrimInner::N(_), PrimInner::N(_)) => cmp::Ordering::Equal,
            (PrimInner::B(B(a)), PrimInner::B(B(b))) => a.cmp(b),
            (PrimInner::S(S(a)), PrimInner::S(S(b))) => a.cmp(b),
            (PrimInner::SS(SS(a)), PrimInner::SS(SS(b))) => a.cmp(b),
            (PrimInner::S(S(a)), PrimInner::SS(SS(b))) => a.as_str().cmp(b),
            (PrimInner::SS(SS(a)), PrimInner::S(S(b))) => (&**a).cmp(b.as_str()),
            (PrimInner::D(D(a)), PrimInner::D(D(b))) => a.cmp(b),
            (PrimInner::I(I(a)), PrimInner::I(I(b))) => a.cmp(b),
            (PrimInner::U(U(a)), PrimInner::U(U(b))) => a.cmp(b),
            (PrimInner::I(I(a)), PrimInner::U(U(b))) => (*a as i128).cmp(&(*b as i128)),
            (PrimInner::U(U(a)), PrimInner::I(I(b))) => (*a as i128).cmp(&(*b as i128)),
            // TODO - might be nice if ints and floats could sort together
            //        but there's a lot of complexity there...
            (PrimInner::F(a), PrimInner::F(b)) => a.cmp(b),
            (PrimInner::N(_), _) => cmp::Ordering::Less,
            (_, PrimInner::N(_)) => cmp::Ordering::Greater,
            (PrimInner::B(_), _) => cmp::Ordering::Less,
            (_, PrimInner::B(_)) => cmp::Ordering::Greater,
            (PrimInner::S(_), _) => cmp::Ordering::Less,
            (_, PrimInner::S(_)) => cmp::Ordering::Greater,
            (PrimInner::SS(_), _) => cmp::Ordering::Less,
            (_, PrimInner::SS(_)) => cmp::Ordering::Greater,
            (PrimInner::D(_), _) => cmp::Ordering::Less,
            (_, PrimInner::D(_)) => cmp::Ordering::Greater,
            (PrimInner::I(_), _) => cmp::Ordering::Less,
            (_, PrimInner::I(_)) => cmp::Ordering::Greater,
            (PrimInner::U(_), _) => cmp::Ordering::Less,
            (_, PrimInner::U(_)) => cmp::Ordering::Greater,
        }
    }
}

#[cfg(test)]
mod prim_test {
    use super::*;

    #[test]
    fn mem_size() {
        assert_eq!(8, mem::size_of::<Prim>());
    }

    #[test]
    fn sort() {
        let expect = vec![
            Prim::from(()),
            Prim::from(false),
            Prim::from(true),
            Prim::from("hello".to_string()),
            Prim::from("hello"),
            Prim::from("world".to_string()),
            Prim::from("world"),
            Prim::from(vec![0, 1, 2, 3]),
            Prim::from(-42_i32),
            Prim::from(42_u32),
            Prim::from(3.14159),
        ];
        let mut v = expect.iter().cloned().collect::<Vec<_>>();
        for _ in 0..10000 {
            v.sort_unstable();
        }
        assert_eq!(expect, v);
    }

    #[test]
    fn as_() {
        assert_eq!((), Prim::from(()).as_::<()>());
        assert_eq!(true, Prim::from(true).as_::<bool>());
        assert_eq!(42, Prim::from(42).as_::<i8>());
        assert_eq!(3.14159, Prim::from(3.14159).as_::<f64>());
    }
}
