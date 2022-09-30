//! Variable hook system.

use crate::*;

/// A hook that can react to triggers and modify system state.
pub trait Hook: 'static + Send {
    /// A name for the hook (largely for tracing).
    fn hook_name(&self) -> &'static str;

    /// React to a trigger.
    fn on_trigger(
        &mut self,
        //sys: &Arc<HookSys>,
        state: &mut Var,
        trigger: &mut Prim,
        arg: &mut Var,
    );
}

/// Hook system handle.
pub struct HookSys(tokio::sync::mpsc::UnboundedSender<Cmd>);

/// Driver future for processing hook triggers.
pub type HookSysDriver = std::pin::Pin<Box<dyn std::future::Future<Output = ()> + 'static + Send>>;

impl HookSys {
    /// Create a new hook system.
    pub fn new() -> (Arc<Self>, HookSysDriver) {
        let (cmd_send, cmd_recv) = tokio::sync::mpsc::unbounded_channel();

        let driver: HookSysDriver = Box::pin(hook_task(cmd_recv));

        (Arc::new(Self(cmd_send)), driver)
    }

    /// Push a new hook into the system.
    pub fn push_hook<H: Hook>(&self, hook: H) {
        let _ = self.0.send(Cmd::PushHook(Box::new(hook)));
    }

    /// Broadcast a new trigger to the system.
    pub fn trigger<T, A>(
        &self,
        trigger: T,
        arg: A,
    )
    where
        T: Into<Prim>,
        A: Into<Var>,
    {
        let _ = self.0.send(Cmd::Trigger(trigger.into(), arg.into()));
    }
}

enum Cmd {
    PushHook(Box<dyn Hook>),
    Trigger(Prim, Var),
}

async fn hook_task(mut recv: tokio::sync::mpsc::UnboundedReceiver<Cmd>) {
    let mut inner = SysInner::new();

    while let Some(cmd) = recv.recv().await {
        inner.dispatch(cmd);
    }
}

struct SysInner {
    hooks: Vec<Box<dyn Hook>>,
    state: Var,
}

impl SysInner {
    pub fn new() -> Self {
        Self {
            hooks: Vec::new(),
            state: Var::new(),
        }
    }

    pub fn dispatch(&mut self, cmd: Cmd) {
        match cmd {
            Cmd::PushHook(hook) => self.push_hook(hook),
            Cmd::Trigger(trigger, arg) => self.trigger(trigger, arg),
        }
    }

    pub fn push_hook(&mut self, hook: Box<dyn Hook>) {
        self.hooks.push(hook);
    }

    pub fn trigger(&mut self, mut trigger: Prim, mut arg: Var) {
        let Self { hooks, state } = self;

        for hook in hooks.iter_mut() {
            hook.on_trigger(state, &mut trigger, &mut arg);
        }
    }

    /*
    pub fn hooks(&mut self) -> &mut Vec<Box<dyn Hook>> {
        &mut self.hooks
    }

    pub fn trigger(&mut self, _trigger: Prim, _arg: Var) {
    }
    */
}
