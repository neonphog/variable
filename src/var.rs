use crate::*;
use collections::{hash_map, HashMap};

/// Reference to a var as an array, returned from assert_array.
pub struct VarArrRef<'lt>(&'lt mut Vec<Var>);

impl VarArrRef<'_> {
    /// Access the raw inner vec.
    pub fn as_raw_mut(&mut self) -> &mut Vec<Var> {
        &mut self.0
    }
}

/// Reference to a var as a map, returned from assert_map.
pub struct VarMapRef<'lt>(&'lt mut HashMap<Prim, Var>);

impl VarMapRef<'_> {
    /// Access the raw inner map.
    pub fn as_raw_mut(&mut self) -> &mut HashMap<Prim, Var> {
        &mut self.0
    }

    /// Assert a var is in the map, will create a new unit var if needed.
    pub fn assert<K: Into<Prim>>(&mut self, key: K) -> &mut Var {
        match self.0.entry(key.into()) {
            hash_map::Entry::Occupied(e) => e.into_mut(),
            hash_map::Entry::Vacant(e) => e.insert(Var::new()),
        }
    }
}

/// Generic variable storing basic primitive types, as well as arrays and maps.
#[derive(Clone, PartialEq, Eq)]
pub struct Var(VarInner);

impl util::StdError for Var {}

impl<P: Into<Prim>> From<P> for Var {
    #[inline]
    fn from(p: P) -> Self {
        Self(VarInner::Prim(Box::new(p.into())))
    }
}

impl From<Vec<Var>> for Var {
    #[inline]
    fn from(a: Vec<Var>) -> Self {
        Self(VarInner::Arr(Box::new(a)))
    }
}

impl From<HashMap<Prim, Var>> for Var {
    #[inline]
    fn from(m: HashMap<Prim, Var>) -> Self {
        Self(VarInner::Map(Box::new(m)))
    }
}

impl From<&Var> for () {
    fn from(_: &Var) -> Self {
        ()
    }
}

impl From<Var> for () {
    fn from(_: Var) -> Self {
        ()
    }
}

impl From<&Var> for bool {
    fn from(p: &Var) -> Self {
        match &p.0 {
            VarInner::Prim(p) => (&**p).into(),
            _ => false,
        }
    }
}

impl From<Var> for bool {
    #[inline]
    fn from(p: Var) -> Self {
        (&p).into()
    }
}

macro_rules! to_xint {
    ($($t:ty)*) => {$(
        impl From<&Var> for $t {
            fn from(p: &Var) -> Self {
                match &p.0 {
                    VarInner::Prim(p) => (&**p).into(),
                    _ => 0,
                }
            }
        }

        impl From<Var> for $t {
            #[inline]
            fn from(p: Var) -> Self {
                (&p).into()
            }
        }
    )*};
}
to_xint!(i8 u8 i16 u16 i32 u32 i64 u64 isize usize);

macro_rules! to_float {
    ($($t:ty)*) => {$(
        impl From<&Var> for $t {
            fn from(p: &Var) -> Self {
                match &p.0 {
                    VarInner::Prim(p) => (&**p).into(),
                    _ => 0.0,
                }
            }
        }

        impl From<Var> for $t {
            #[inline]
            fn from(p: Var) -> Self {
                (&p).into()
            }
        }
    )*};
}
to_float!(f32 f64);

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            VarInner::Prim(p) => write!(f, "Var({:?})", p),
            VarInner::Arr(a) => write!(f, "Var({:?})", a),
            VarInner::Map(m) => write!(f, "Var({:?})", m),
        }
    }
}

impl fmt::Display for Var {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.as_str())
    }
}

impl Default for Var {
    #[inline]
    fn default() -> Self {
        Self(VarInner::Prim(Box::new(Prim::default())))
    }
}

impl Var {
    /// Construct a new default (unit) Var instance.
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the value tracked by this Var instance.
    #[inline]
    pub fn set<V: Into<Var>>(&mut self, v: V) {
        self.0 = (v.into()).0
    }

    /// Get this variable as a str.
    pub fn as_str(&self) -> borrow::Cow<'_, str> {
        match &self.0 {
            VarInner::Prim(p) => p.as_str(),
            VarInner::Arr(a) => format!("[{} elem array]", a.len()).into(),
            VarInner::Map(m) => format!("[{} entry map]", m.len()).into(),
        }
    }

    /// Get this variable as a bytes slice.
    pub fn as_slice(&self) -> borrow::Cow<'_, [u8]> {
        match &self.0 {
            VarInner::Prim(p) => p.as_slice(),
            _ => (&[][..]).into(),
        }
    }

    /// As any type that implements From<&Var>.
    #[inline]
    pub fn as_<'a, T>(&'a self) -> T
    where
        T: From<&'a Var>,
    {
        self.into()
    }

    /// If this var is not already an array, it will be converted into
    /// an empty array.
    /// Returns a mutable reference to this var as an array of Vars.
    pub fn assert_array(&mut self) -> VarArrRef<'_> {
        // why do we need this extra matches!, rust?
        if matches!(self.0, VarInner::Arr(_)) {
            if let VarInner::Arr(a) = &mut self.0 {
                return VarArrRef(a);
            }
            unreachable!()
        }
        self.0 = (Var::from(<Vec<Var>>::new())).0;
        if let VarInner::Arr(a) = &mut self.0 {
            return VarArrRef(a);
        }
        unreachable!()
    }

    /// If this var is not already a map, it will be converted into
    /// an empty map.
    /// Returns a mutable reference to this var as a map of Prim->Vars.
    pub fn assert_map(&mut self) -> VarMapRef<'_> {
        // why do we need this extra matches!, rust?
        if matches!(self.0, VarInner::Map(_)) {
            if let VarInner::Map(m) = &mut self.0 {
                return VarMapRef(m);
            }
            unreachable!()
        }
        self.0 = (Var::from(<HashMap<Prim, Var>>::new())).0;
        if let VarInner::Map(m) = &mut self.0 {
            return VarMapRef(m);
        }
        unreachable!()
    }
}

#[derive(Clone, PartialEq, Eq)]
enum VarInner {
    Prim(Box<Prim>),
    Arr(Box<Vec<Var>>),
    Map(Box<HashMap<Prim, Var>>),
}

#[cfg(test)]
mod var_test {
    use super::*;

    #[test]
    fn mem_size() {
        assert_eq!(16, mem::size_of::<Var>());
    }

    #[test]
    fn usage() {
        let mut v = Var::new();

        v.assert_map().assert("hello").set("world");
        v.assert_map().assert(21).set(3.14159);

        println!("{:#?}", v);
    }
}
