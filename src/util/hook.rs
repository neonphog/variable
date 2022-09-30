//! Variable hook system.

use crate::*;
use collections::HashMap;
use sync::Weak;

fn uniq() -> Prim {
    static UNIQ: sync::atomic::AtomicU64 = sync::atomic::AtomicU64::new(0);
    UNIQ.fetch_add(1, sync::atomic::Ordering::Relaxed).into()
}

/// A hook that can react to triggers and modify system state.
pub trait Hook: 'static + Send {
    /// Initialize a hook.
    fn on_init(&mut self, sys: &Arc<HookSys>, state: &mut Var, hook_id: Prim) {
        let _ = (sys, state, hook_id);
    }

    /// React to a trigger.
    fn on_trigger(
        &mut self,
        sys: &Arc<HookSys>,
        state: &mut Var,
        trigger: &mut Prim,
        arg: &mut Var,
    ) {
        let _ = (sys, state, trigger, arg);
    }

    /// React to a request.
    fn on_request(
        &mut self,
        sys: &Arc<HookSys>,
        state: &mut Var,
        request: Prim,
        arg: Var,
        request_id: Prim,
    ) {
        let _ = (sys, state, request, arg, request_id);
    }
}

/// Hook system handle.
pub struct HookSys(tokio::sync::mpsc::UnboundedSender<Cmd>);

/// Driver future for processing hook triggers.
pub type HookSysDriver = std::pin::Pin<Box<dyn std::future::Future<Output = ()> + 'static + Send>>;

impl HookSys {
    /// Create a new hook system.
    pub fn new() -> (Arc<Self>, HookSysDriver) {
        let (cmd_send, cmd_recv) = tokio::sync::mpsc::unbounded_channel();

        let this = Arc::new(Self(cmd_send));

        let driver = Box::pin(hook_task(Arc::downgrade(&this), cmd_recv));

        (this, driver)
    }

    /// Push a new hook into the system.
    pub fn push_hook<H: Hook>(&self, hook: H) {
        let _ = self.0.send(Cmd::PushHook(Box::new(hook)));
    }

    /// Broadcast a new trigger to the system.
    pub fn trigger<T, A>(&self, trigger: T, arg: A)
    where
        T: Into<Prim>,
        A: Into<Var>,
    {
        let _ = self.0.send(Cmd::Trigger(trigger.into(), arg.into()));
    }

    /// Make a request of a specific sibling hook via hook_id.
    pub async fn request<R, A, I>(&self, request: R, arg: A, hook_id: I) -> Result<Var, Prim>
    where
        R: Into<Prim>,
        A: Into<Var>,
        I: Into<Prim>,
    {
        let request_id = uniq();
        let _drop = {
            struct D(tokio::sync::mpsc::UnboundedSender<Cmd>, Prim);
            impl Drop for D {
                fn drop(&mut self) {
                    let _ = self.0.send(Cmd::DropRequest(self.1.clone()));
                }
            }
            D(self.0.clone(), request_id.clone())
        };
        let (s, r) = tokio::sync::oneshot::channel();
        if self
            .0
            .send(Cmd::Request {
                request: request.into(),
                arg: arg.into(),
                hook_id: hook_id.into(),
                request_id,
                respond: s,
            })
            .is_err()
        {
            return Err("DriverClosed".into());
        }
        r.await.map_err(|_| Prim::from("DriverClosed"))?
    }

    /// Respond with success to a request via request_id.
    pub fn respond_ok<R, I>(&self, response: R, request_id: I)
    where
        R: Into<Var>,
        I: Into<Prim>,
    {
        let _ = self.0.send(Cmd::Respond {
            response: Ok(response.into()),
            request_id: request_id.into(),
        });
    }

    /// Respond with an error to a request via request_id.
    pub fn respond_err<E, I>(&self, response: E, request_id: I)
    where
        E: Into<Prim>,
        I: Into<Prim>,
    {
        let _ = self.0.send(Cmd::Respond {
            response: Err(response.into()),
            request_id: request_id.into(),
        });
    }
}

enum Cmd {
    PushHook(Box<dyn Hook>),
    Trigger(Prim, Var),
    Request {
        request: Prim,
        arg: Var,
        hook_id: Prim,
        request_id: Prim,
        respond: tokio::sync::oneshot::Sender<Result<Var, Prim>>,
    },
    DropRequest(Prim),
    Respond {
        response: Result<Var, Prim>,
        request_id: Prim,
    },
}

async fn hook_task(hnd: Weak<HookSys>, mut recv: tokio::sync::mpsc::UnboundedReceiver<Cmd>) {
    let mut inner = SysInner::new(hnd);

    while let Some(cmd) = recv.recv().await {
        inner.dispatch(cmd);
    }
}

struct SysInner {
    hnd: Weak<HookSys>,
    hook_order: Vec<Prim>,
    hooks: HashMap<Prim, Box<dyn Hook>>,
    responses: HashMap<Prim, tokio::sync::oneshot::Sender<Result<Var, Prim>>>,
    state: Var,
}

impl SysInner {
    pub fn new(hnd: Weak<HookSys>) -> Self {
        Self {
            hnd,
            hook_order: Vec::new(),
            hooks: HashMap::new(),
            responses: HashMap::new(),
            state: Var::new(),
        }
    }

    pub fn dispatch(&mut self, cmd: Cmd) {
        match cmd {
            Cmd::PushHook(hook) => self.push_hook(hook),
            Cmd::Trigger(trigger, arg) => self.trigger(trigger, arg),
            Cmd::Request {
                request,
                arg,
                hook_id,
                request_id,
                respond,
            } => self.request(request, arg, hook_id, request_id, respond),
            Cmd::DropRequest(request_id) => self.drop_request(request_id),
            Cmd::Respond {
                response,
                request_id,
            } => self.respond(response, request_id),
        }
    }

    pub fn push_hook(&mut self, hook: Box<dyn Hook>) {
        let Self {
            hnd,
            hook_order,
            hooks,
            state,
            ..
        } = self;

        let hnd = match hnd.upgrade() {
            None => return,
            Some(hnd) => hnd,
        };

        let hook_id = uniq();
        hook_order.push(hook_id.clone());
        hooks.insert(hook_id.clone(), hook);
        hooks
            .get_mut(&hook_id)
            .unwrap()
            .on_init(&hnd, state, hook_id);
    }

    pub fn trigger(&mut self, mut trigger: Prim, mut arg: Var) {
        let Self {
            hnd,
            hook_order,
            hooks,
            state,
            ..
        } = self;

        let hnd = match hnd.upgrade() {
            None => return,
            Some(hnd) => hnd,
        };

        for hook_id in hook_order.iter() {
            if let Some(hook) = hooks.get_mut(hook_id) {
                hook.on_trigger(&hnd, state, &mut trigger, &mut arg);
            }
        }
    }

    pub fn request(
        &mut self,
        request: Prim,
        arg: Var,
        hook_id: Prim,
        request_id: Prim,
        respond: tokio::sync::oneshot::Sender<Result<Var, Prim>>,
    ) {
        let Self {
            hnd,
            hooks,
            responses,
            state,
            ..
        } = self;

        let hook = match hooks.get_mut(&hook_id) {
            None => {
                let _ = respond.send(Err("InvalidHookId".into()));
                return;
            }
            Some(hook) => hook,
        };

        let hnd = match hnd.upgrade() {
            None => return,
            Some(hnd) => hnd,
        };

        responses.insert(request_id.clone(), respond);

        hook.on_request(&hnd, state, request, arg, request_id);
    }

    pub fn drop_request(&mut self, request_id: Prim) {
        self.responses.remove(&request_id);
    }

    pub fn respond(&mut self, response: Result<Var, Prim>, request_id: Prim) {
        if let Some(respond) = self.responses.remove(&request_id) {
            let _ = respond.send(response);
        }
    }
}

#[cfg(test)]
mod hook_test {
    use super::*;

    #[tokio::test(flavor = "multi_thread")]
    async fn hook_usage() {
        let (sys, driver) = HookSys::new();
        let task = tokio::task::spawn(driver);

        struct HookA(Option<tokio::sync::oneshot::Sender<Prim>>);

        impl Hook for HookA {
            fn on_init(&mut self, _sys: &Arc<HookSys>, state: &mut Var, hook_id: Prim) {
                state.assert_map().assert("hook_a").set(&hook_id);

                if let Some(s) = self.0.take() {
                    let _ = s.send(hook_id);
                }
            }

            fn on_trigger(
                &mut self,
                _sys: &Arc<HookSys>,
                state: &mut Var,
                trigger: &mut Prim,
                _arg: &mut Var,
            ) {
                if trigger == "hook_b_init" {
                    println!("hook_a got hook_b_init, state: {:#?}", state);
                }
            }

            fn on_request(
                &mut self,
                sys: &Arc<HookSys>,
                state: &mut Var,
                _request: Prim,
                _arg: Var,
                request_id: Prim,
            ) {
                let sys = sys.clone();
                let hook_b_id = Prim::from(state.assert_map().assert("hook_b"));
                tokio::task::spawn(async move {
                    match sys.request((), (), hook_b_id).await {
                        Ok(r) => {
                            sys.respond_ok(format!("hello {}", r), request_id);
                        }
                        Err(e) => {
                            sys.respond_err(e, request_id);
                        }
                    }
                });
            }
        }

        let (a_s, a_r) = tokio::sync::oneshot::channel();
        sys.push_hook(HookA(Some(a_s)));

        let hook_a_id = a_r.await.unwrap();
        println!("HookA::on_init: HookId: {}", &hook_a_id);

        struct HookB(Option<tokio::sync::oneshot::Sender<Prim>>);

        impl Hook for HookB {
            fn on_init(&mut self, sys: &Arc<HookSys>, state: &mut Var, hook_id: Prim) {
                state.assert_map().assert("hook_b").set(&hook_id);

                sys.trigger("hook_b_init", &hook_id);

                if let Some(s) = self.0.take() {
                    let _ = s.send(hook_id);
                }
            }

            fn on_request(
                &mut self,
                sys: &Arc<HookSys>,
                _state: &mut Var,
                _request: Prim,
                _arg: Var,
                request_id: Prim,
            ) {
                sys.respond_ok("world!", request_id);
            }
        }

        let (b_s, b_r) = tokio::sync::oneshot::channel();
        sys.push_hook(HookB(Some(b_s)));

        let hook_b_id = b_r.await.unwrap();
        println!("HookB::on_init: HookId: {}", &hook_b_id);

        assert_eq!(
            "hello world!",
            sys.request((), (), hook_a_id).await.unwrap(),
        );

        drop(sys);
        task.await.unwrap();
    }
}
