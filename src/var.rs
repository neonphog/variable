use crate::*;

/// Generic variable storing basic types.
#[derive(Debug, Clone)]
pub struct Var(VarInner);

impl util::StdError for Var {}

impl fmt::Display for Var {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl PartialEq for Var {
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
impl Eq for Var {}

impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Var {
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

impl hash::Hash for Var {
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
        impl ops::Add<$t> for &Var {
            type Output = $t;

            #[inline]
            fn add(self, rhs: $t) -> Self::Output {
                <$t>::from(self).add(rhs)
            }
        }

        impl ops::Add<$t> for Var {
            type Output = $t;

            #[inline]
            fn add(self, rhs: $t) -> Self::Output {
                <$t>::from(self).add(rhs)
            }
        }

        impl ops::Add<&Var> for $t {
            type Output = $t;

            #[inline]
            fn add(self, rhs: &Var) -> Self::Output {
                self.add(<$t>::from(rhs))
            }
        }

        impl ops::Add<Var> for $t {
            type Output = $t;

            #[inline]
            fn add(self, rhs: Var) -> Self::Output {
                self.add(<$t>::from(rhs))
            }
        }

        impl ops::Sub<$t> for &Var {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: $t) -> Self::Output {
                <$t>::from(self).sub(rhs)
            }
        }

        impl ops::Sub<$t> for Var {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: $t) -> Self::Output {
                <$t>::from(self).sub(rhs)
            }
        }

        impl ops::Sub<&Var> for $t {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: &Var) -> Self::Output {
                self.sub(<$t>::from(rhs))
            }
        }

        impl ops::Sub<Var> for $t {
            type Output = $t;

            #[inline]
            fn sub(self, rhs: Var) -> Self::Output {
                self.sub(<$t>::from(rhs))
            }
        }
    )*};
}
arith_int!(i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize);

impl ops::AddAssign<Var> for i16 {
    fn add_assign(&mut self, rhs: Var) {
        let rhs = match rhs.0 {
            VarInner::Int(_, _, i) => i as i16,
            _ => 0,
        };
        *self += rhs;
    }
}

impl ops::AddAssign<i16> for Var {
    fn add_assign(&mut self, rhs: i16) {
        let this = match self.0 {
            VarInner::Int(_, _, i) => i as i16,
            _ => 0,
        };
        *self = (this + rhs).into()
    }
}

static STATIC_UNIT: Var = Var(VarInner::Unit);

impl ops::Index<&Var> for Var {
    type Output = Var;

    fn index(&self, index: &Var) -> &Self::Output {
        match &self.0 {
            VarInner::Arr(v) => v.index(usize::from(index)),
            _ => &STATIC_UNIT,
        }
    }
}

impl ops::Index<Var> for Var {
    type Output = Var;

    #[inline]
    fn index(&self, index: Var) -> &Self::Output {
        self.index(&index)
    }
}

macro_rules! index {
    ($($t:ty)*) => {$(
        impl ops::Index<$t> for Var {
            type Output = Var;

            #[inline]
            fn index(&self, index: $t) -> &Self::Output {
                self.index(Var::from(index))
            }
        }
    )*};
}
index!( i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize String &'static str );

impl From<()> for Var {
    fn from(_: ()) -> Self {
        Self(VarInner::Unit)
    }
}

impl From<&'static str> for Var {
    fn from(s: &'static str) -> Self {
        // TODO - static VarInner variant?
        Self(VarInner::String(s.to_string().into_boxed_str()))
    }
}

impl From<Vec<Var>> for Var {
    fn from(v: Vec<Var>) -> Self {
        Self(VarInner::Arr(v))
    }
}

impl From<String> for Var {
    fn from(s: String) -> Self {
        Self(VarInner::String(s.into_boxed_str()))
    }
}

impl From<Vec<u8>> for Var {
    fn from(b: Vec<u8>) -> Self {
        b.into_boxed_slice().into()
    }
}

impl From<Box<[u8]>> for Var {
    fn from(b: Box<[u8]>) -> Self {
        match String::from_utf8(b.into_vec()) {
            Ok(s) => Self(VarInner::String(s.into_boxed_str())),
            Err(e) => Self(VarInner::Bytes(e.into_bytes().into_boxed_slice())),
        }
    }
}

impl From<i128> for Var {
    fn from(i: i128) -> Self {
        let b = bin_sort(i.to_be_bytes(), i >= 0);
        let s = i.to_string().into_boxed_str();
        Self(VarInner::Int(b, s, i))
    }
}

macro_rules! from_int {
    ($($t:ty)*) => {$(
        impl From<$t> for Var {
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
        impl From<&Var> for $t {
            fn from(v: &Var) -> Self {
                match v.0 {
                    VarInner::Int(_, _, i) => i.clamp(
                        <$t>::MIN as i128,
                        <$t>::MAX as i128,
                    ) as $t,
                    VarInner::Float(_, _, f) => f as $t,
                    _ => 0,
                }
            }
        }

        impl From<Var> for $t {
            #[inline]
            fn from(v: Var) -> Self {
                (&v).into()
            }
        }
    )*};
}
to_int!(i8 u8 i16 u16 i32 u32 i64 u64 i128 isize usize);

impl From<f64> for Var {
    fn from(f: f64) -> Self {
        let b = bin_sort(f.to_be_bytes(), f >= 0.0);
        let s = f.to_string().into_boxed_str();
        Self(VarInner::Float(b, s, f))
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

impl From<f32> for Var {
    #[inline]
    fn from(f: f32) -> Self {
        (f as f64).into()
    }
}

macro_rules! to_float {
    ($($t:ty)*) => {$(
        impl From<&Var> for $t {
            fn from(v: &Var) -> Self {
                match v.0 {
                    VarInner::Int(_, _, i) => i as $t,
                    VarInner::Float(_, _, f) => f as $t,
                    _ => 0.0,
                }
            }
        }

        impl From<Var> for $t {
            #[inline]
            fn from(v: Var) -> Self {
                (&v).into()
            }
        }
    )*};
}
to_float!(f32 f64);

#[derive(Clone)]
enum VarInner {
    Unit,
    String(Box<str>),
    Bytes(Box<[u8]>),
    Int([u8; 16], Box<str>, i128),
    Float([u8; 8], Box<str>, f64),
    Arr(Vec<Var>),
}

impl fmt::Debug for VarInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for VarInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarInner::Unit => f.write_str("()"),
            VarInner::String(s) => f.write_str(s),
            VarInner::Bytes(b) => {
                f.write_str("0x")?;
                for b in b.iter() {
                    write!(f, "{:02x}", b)?;
                }
                Ok(())
            }
            VarInner::Int(_, s, _) => f.write_str(s),
            VarInner::Float(_, s, _) => f.write_str(s),
            VarInner::Arr(v) => {
                f.write_str("[")?;
                let mut first = true;
                for i in v.iter() {
                    if first {
                        first = false;
                    } else {
                        f.write_str(",")?;
                    }
                    i.fmt(f)?;
                }
                f.write_str("]")
            }
        }
    }
}

fn sub_conv(v: &Var) -> IdentIter<'_> {
    v.0.ident_iter()
}

enum IdentIter<'lt> {
    Slice(Option<&'lt [u8]>),
    Sub(
        Option<Box<IdentIter<'lt>>>,
        iter::Map<slice::Iter<'lt, Var>, for<'r> fn(&'r Var) -> IdentIter<'r>>,
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

impl VarInner {
    pub fn ident_iter(&self) -> IdentIter<'_> {
        match self {
            VarInner::Unit => IdentIter::Slice(None),
            VarInner::String(s) => IdentIter::Slice(Some(s.as_bytes())),
            VarInner::Bytes(b) => IdentIter::Slice(Some(b)),
            VarInner::Int(b, _, _) => IdentIter::Slice(Some(&b[..])),
            VarInner::Float(b, _, _) => IdentIter::Slice(Some(&b[..])),
            VarInner::Arr(v) => IdentIter::Sub(None, v.iter().map(sub_conv)),
        }
    }
}
