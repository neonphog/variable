//! Variable hook system.

use crate::*;
use std::sync::{Weak, Mutex};

/// A hook that can react to triggers and modify system state.
pub trait Hook: 'static + Send {
    /// A name for the hook (largely for tracing).
    fn hook_name(&self) -> &'static str;

    /// React to a trigger.
    fn on_trigger(
        &mut self,
        sys: &Arc<HookSys>,
        state: &mut Var,
        trigger: &mut Prim,
        arg: &mut Var,
    );
}

/// Hook system handle.
pub struct HookSys(Mutex<SysInner>);

impl HookSys {
    /// Create a new hook system.
    pub fn new() -> Arc<Self> {
        Arc::new_cyclic(|this| {
            Self(Mutex::new(SysInner::new(this.clone())))
        })
    }

    /// Manage the hooks in the system.
    pub fn manage_hooks<Cb>(&self, cb: Cb)
    where
        Cb: FnOnce(&mut Vec<Box<dyn Hook>>),
    {
        let mut lock = self.0.lock().unwrap();
        cb(lock.hooks());
    }

    /// Trigger a system notification.
    pub fn trigger<T, A>(
        &self,
        trigger: T,
        arg: A,
    )
    where
        T: Into<Prim>,
        A: Into<Var>,
    {
        self.0.lock().unwrap().trigger(trigger.into(), arg.into());
    }
}

struct SysInner {
    this: Weak<HookSys>,
    hooks: Vec<Box<dyn Hook>>,
    state: Var,
}

impl SysInner {
    pub fn new(this: Weak<HookSys>) -> Self {
        Self {
            this,
            hooks: Vec::new(),
            state: Var::new(),
        }
    }

    pub fn hooks(&mut self) -> &mut Vec<Box<dyn Hook>> {
        &mut self.hooks
    }

    pub fn trigger(&mut self, _trigger: Prim, _arg: Var) {
    }
}
