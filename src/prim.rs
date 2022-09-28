use crate::*;

/// Generic variable storing basic primitive types.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Prim(PrimInner);

impl util::StdError for Prim {}

impl fmt::Debug for Prim {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for Prim {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<&()> for Prim {
    #[inline]
    fn from(_: &()) -> Self {
        Self(PrimInner::N(N))
    }
}

impl From<()> for Prim {
    #[inline]
    fn from(_: ()) -> Self {
        Self(PrimInner::N(N))
    }
}

impl From<&bool> for Prim {
    #[inline]
    fn from(b: &bool) -> Self {
        Self(PrimInner::B(B(*b)))
    }
}

impl From<bool> for Prim {
    #[inline]
    fn from(b: bool) -> Self {
        Self(PrimInner::B(B(b)))
    }
}

impl From<String> for Prim {
    #[inline]
    fn from(s: String) -> Self {
        Self(PrimInner::S(S(Box::new(s))))
    }
}

impl From<Vec<u8>> for Prim {
    #[inline]
    fn from(d: Vec<u8>) -> Self {
        Self(PrimInner::D(D(Box::new(d))))
    }
}

macro_rules! from_int {
    ($($t:ty)*) => {$(
        impl From<&$t> for Prim {
            #[inline]
            fn from(i: &$t) -> Self {
                Self(PrimInner::I(I(*i as i64)))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(i: $t) -> Self {
                Self(PrimInner::I(I(i as i64)))
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
                Self(PrimInner::U(U(*u as u64)))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(u: $t) -> Self {
                Self(PrimInner::U(U(u as u64)))
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
                Self(PrimInner::F(F::new(*f as f64)))
            }
        }

        impl From<$t> for Prim {
            #[inline]
            fn from(f: $t) -> Self {
                Self(PrimInner::F(F::new(f as f64)))
            }
        }
    )*};
}
from_float!(f32 f64);

macro_rules! to_xint {
    ($($t:ty)*) => {$(
        impl From<&Prim> for $t {
            fn from(p: &Prim) -> Self {
                let i: i128 = match &p.0 {
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

/*
impl PartialEq for Prim {
    fn eq(&self, other: &Self) -> bool {
        let mut iter1 = self.0.ident_iter();
        let mut iter2 = other.0.ident_iter();
        match (iter1.next(), iter2.next()) {
            (None, Some(_)) => return false,
            (Some(_), None) => return false,
            (Some(a), Some(b)) => {
                if !a.eq(b) {
                    return false;
                }
            }
            _ => (),
        }
        true
    }
}

// Since we're using the bytes, not the floats themselves for
// comparison, hashing and ordering, this is okay.
impl Eq for Prim {}

impl PartialOrd for Prim {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Prim {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        let mut iter1 = self.0.ident_iter();
        let mut iter2 = other.0.ident_iter();
        match (iter1.next(), iter2.next()) {
            (None, None) => return cmp::Ordering::Equal,
            (None, Some(_)) => return cmp::Ordering::Greater,
            (Some(_), None) => return cmp::Ordering::Less,
            (Some(a), Some(b)) => match a.cmp(b) {
                cmp::Ordering::Equal => (),
                oth => return oth,
            },
        }
        cmp::Ordering::Equal
    }
}

impl hash::Hash for Prim {
    fn hash<H>(&self, state: &mut H)
    where
        H: hash::Hasher,
    {
        for slice in self.0.ident_iter() {
            slice.hash(state);
        }
    }
}

macro_rules! arith_int {
    ($($t:ty)*) => {$(
        impl ops::Add<$t> for &Prim {
            type Output = $t;

            #[inline]
            fn add(self, rhs: $t) -> Self::Output {
                <$t>::from(self).add(rhs)
            }
        }

        impl ops::Add<$t> for Prim {
            type Output = $t;

            #[inline]
            fn add(self, rhs: $t) -> Self::Output {
                <$t>::from(self).add(rhs)
            }
        }

        impl ops::Add<&Prim> for $t {
            type Output = $t;

            #[inline]
            fn add(self, rhs: &Prim) -> Self::Output {
                self.add(<$t>::from(rhs))
            }
        }

        impl ops::Add<Prim> for $t {
            type Output = $t;

            #[inline]
            fn add(self, rhs: Prim) -> Self::Output {
                self.add(<$t>::from(rhs))
            }
        }

        impl ops::Sub<$t> for &Prim {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: $t) -> Self::Output {
                <$t>::from(self).sub(rhs)
            }
        }

        impl ops::Sub<$t> for Prim {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: $t) -> Self::Output {
                <$t>::from(self).sub(rhs)
            }
        }

        impl ops::Sub<&Prim> for $t {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: &Prim) -> Self::Output {
                self.sub(<$t>::from(rhs))
            }
        }

        impl ops::Sub<Prim> for $t {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: Prim) -> Self::Output {
                self.sub(<$t>::from(rhs))
            }
        }
    )*};
}
arith_int!(i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize);

impl ops::AddAssign<Prim> for i16 {
    fn add_assign(&mut self, rhs: Prim) {
        let rhs = match rhs.0 {
            PrimInner::Int(_, _, i) => i as i16,
            _ => 0,
        };
        *self += rhs;
    }
}

impl ops::AddAssign<i16> for Prim {
    fn add_assign(&mut self, rhs: i16) {
        let this = match self.0 {
            PrimInner::Int(_, _, i) => i as i16,
            _ => 0,
        };
        *self = (this + rhs).into()
    }
}

static STATIC_UNIT: Prim = Prim(PrimInner::Unit);

impl ops::Index<&Prim> for Prim {
    type Output = Prim;

    fn index(&self, index: &Prim) -> &Self::Output {
        match &self.0 {
            PrimInner::Arr(v) => v.index(usize::from(index)),
            _ => &STATIC_UNIT,
        }
    }
}

impl ops::Index<Prim> for Prim {
    type Output = Prim;

    #[inline]
    fn index(&self, index: Prim) -> &Self::Output {
        self.index(&index)
    }
}

macro_rules! index {
    ($($t:ty)*) => {$(
        impl ops::Index<$t> for Prim {
            type Output = Prim;

            #[inline]
            fn index(&self, index: $t) -> &Self::Output {
                self.index(Prim::from(index))
            }
        }
    )*};
}
index!( i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize String &'static str );

impl From<()> for Prim {
    fn from(_: ()) -> Self {
        Self(PrimInner::Unit)
    }
}

impl From<&'static str> for Prim {
    fn from(s: &'static str) -> Self {
        // TODO - static PrimInner variant?
        Self(PrimInner::String(s.to_string().into_boxed_str()))
    }
}

impl From<Vec<Prim>> for Prim {
    fn from(v: Vec<Prim>) -> Self {
        Self(PrimInner::Arr(v))
    }
}

impl From<String> for Prim {
    fn from(s: String) -> Self {
        Self(PrimInner::String(s.into_boxed_str()))
    }
}

impl From<Vec<u8>> for Prim {
    fn from(b: Vec<u8>) -> Self {
        b.into_boxed_slice().into()
    }
}

impl From<Box<[u8]>> for Prim {
    fn from(b: Box<[u8]>) -> Self {
        match String::from_utf8(b.into_vec()) {
            Ok(s) => Self(PrimInner::String(s.into_boxed_str())),
            Err(e) => Self(PrimInner::Bytes(e.into_bytes().into_boxed_slice())),
        }
    }
}

impl From<i128> for Prim {
    fn from(i: i128) -> Self {
        let b = bin_sort(i.to_be_bytes(), i >= 0);
        let s = i.to_string().into_boxed_str();
        Self(PrimInner::Int(b, s, i))
    }
}

macro_rules! from_int {
    ($($t:ty)*) => {$(
        impl From<$t> for Prim {
            #[inline]
            fn from(i: $t) -> Self {
                (i as i128).into()
            }
        }
    )*};
}
from_int!(i8 u8 i16 u16 i32 u32 i64 u64 isize usize);

macro_rules! to_int {
    ($($t:ty)*) => {$(
        impl From<&Prim> for $t {
            fn from(v: &Prim) -> Self {
                match v.0 {
                    PrimInner::Int(_, _, i) => i.clamp(
                        <$t>::MIN as i128,
                        <$t>::MAX as i128,
                    ) as $t,
                    PrimInner::Float(_, _, f) => f as $t,
                    _ => 0,
                }
            }
        }

        impl From<Prim> for $t {
            #[inline]
            fn from(v: Prim) -> Self {
                (&v).into()
            }
        }
    )*};
}
to_int!(i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize);

impl From<f64> for Prim {
    fn from(f: f64) -> Self {
        let b = bin_sort(f.to_be_bytes(), f >= 0.0);
        let s = f.to_string().into_boxed_str();
        Self(PrimInner::Float(b, s, f))
    }
}

fn bin_sort<const N: usize>(mut b: [u8; N], is_pos: bool) -> [u8; N] {
    if is_pos {
        b[0] = b[0] ^ 0x80;
    } else {
        for i in 0..N {
            b[i] ^= 0xff;
        }
    }
    b
}

impl From<f32> for Prim {
    #[inline]
    fn from(f: f32) -> Self {
        (f as f64).into()
    }
}

macro_rules! to_float {
    ($($t:ty)*) => {$(
        impl From<&Prim> for $t {
            fn from(v: &Prim) -> Self {
                match v.0 {
                    PrimInner::Int(_, _, i) => i as $t,
                    PrimInner::Float(_, _, f) => f as $t,
                    _ => 0.0,
                }
            }
        }

        impl From<Prim> for $t {
            #[inline]
            fn from(v: Prim) -> Self {
                (&v).into()
            }
        }
    )*};
}
to_float!(f32 f64);
*/

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct N;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct B(bool);

#[derive(Clone, PartialEq, Eq, Hash)]
struct S(Box<String>);

#[derive(Clone, PartialEq, Eq, Hash)]
struct D(Box<Vec<u8>>);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct I(i64);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct U(u64);

#[derive(Clone, Copy, PartialEq)]
struct F(f64);

// because of our careful new/ord/hash impls, we can apply this
impl Eq for F {}

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
        Self(if f == f64::NAN {
            0.0
        } else if f == f64::INFINITY {
            f64::MAX
        } else if f == f64::NEG_INFINITY {
            f64::MIN
        } else {
            f
        })
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum PrimInner {
    N(N),
    B(B),
    S(S),
    D(D),
    I(I),
    U(U),
    F(F),
}

impl fmt::Debug for PrimInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimInner::N(_) => f.write_str("Prim::Unit("),
            PrimInner::B(_) => f.write_str("Prim::Bool("),
            PrimInner::S(_) => f.write_str("Prim::Str("),
            PrimInner::D(_) => f.write_str("Prim::Bytes("),
            PrimInner::I(_) => f.write_str("Prim::Int("),
            PrimInner::U(_) => f.write_str("Prim::UInt("),
            PrimInner::F(_) => f.write_str("Prim::Float("),
        }?;
        write!(f, "{}", self)?;
        f.write_str(")")
    }
}

impl fmt::Display for PrimInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimInner::N(_) => f.write_str("()"),
            PrimInner::B(B(b)) => b.fmt(f),
            PrimInner::S(S(s)) => s.fmt(f),
            PrimInner::D(D(d)) => {
                f.write_str("0x")?;
                for d in d.iter() {
                    write!(f, "{:02x}", d)?;
                }
                Ok(())
            }
            PrimInner::I(I(i)) => i.fmt(f),
            PrimInner::U(U(u)) => u.fmt(f),
            PrimInner::F(F(fl)) => fl.fmt(f),
        }
    }
}

impl cmp::PartialOrd for PrimInner {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl cmp::Ord for PrimInner {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (PrimInner::N(_), PrimInner::N(_)) => cmp::Ordering::Equal,
            (PrimInner::N(_), _) => cmp::Ordering::Less,
            (_, PrimInner::N(_)) => cmp::Ordering::Greater,
            (PrimInner::B(B(a)), PrimInner::B(B(b))) => a.cmp(b),
            (PrimInner::B(_), _) => cmp::Ordering::Less,
            (_, PrimInner::B(_)) => cmp::Ordering::Greater,
            (PrimInner::S(S(a)), PrimInner::S(S(b))) => a.cmp(b),
            _ => cmp::Ordering::Less,
        }
    }
}

/*
fn sub_conv(v: &Prim) -> IdentIter<'_> {
    v.0.ident_iter()
}

enum IdentIter<'lt> {
    Slice(Option<&'lt [u8]>),
    Sub(
        Option<Box<IdentIter<'lt>>>,
        iter::Map<slice::Iter<'lt, Prim>, for<'r> fn(&'r Prim) -> IdentIter<'r>>,
    ),
}

impl<'lt> Iterator for IdentIter<'lt> {
    type Item = &'lt [u8];

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            IdentIter::Slice(s) => s.take(),
            IdentIter::Sub(cur, it) => {
                if cur.is_none() {
                    if let Some(c) = it.next() {
                        *cur = Some(Box::new(c));
                    }
                }
                if let Some(mut c) = cur.take() {
                    if let Some(item) = c.next() {
                        *cur = Some(c);
                        return Some(item);
                    }
                }
                None
            }
        }
    }
}

impl PrimInner {
    pub fn ident_iter(&self) -> IdentIter<'_> {
        match self {
            PrimInner::Unit => IdentIter::Slice(None),
            PrimInner::String(s) => IdentIter::Slice(Some(s.as_bytes())),
            PrimInner::Bytes(b) => IdentIter::Slice(Some(b)),
            PrimInner::Int(b, _, _) => IdentIter::Slice(Some(&b[..])),
            PrimInner::Float(b, _, _) => IdentIter::Slice(Some(&b[..])),
            PrimInner::Arr(v) => IdentIter::Sub(None, v.iter().map(sub_conv)),
        }
    }
}
*/

#[cfg(test)]
mod prim_test {
    use super::*;

    #[test]
    fn mem_size() {
        assert_eq!(16, mem::size_of::<Prim>());
    }
}
