module context

import time

pub const (
	background         = Context(EmptyContext{})
	todo               = Context(EmptyContext{})

	cancel_context_key = [0]

	// canceled is the error returned by Context.err when the context is canceled.
	canceled           = 'context canceled'

	// deadline_exceeded is the error returned by Context.err when the context's
	// deadline passes.
	deadline_exceeded  = 'context deadline exceeded'
)

// background returns an empty Context. It is never canceled, has no
// values, and has no deadline. It is typically used by the main function,
// initialization, and tests, and as the top-level Context for incoming
// requests.
pub fn background() Context {
	return context.background
}

// todo returns an empty Context. Code should use context.todo when
// it's unclear which Context to use or it is not yet available (because the
// surrounding function has not yet been extended to accept a Context
// parameter).
pub fn todo() Context {
	return context.todo
}

pub type Context = CancelContext | CancelerContext | EmptyContext | TimerContext | ValueContext

pub type CancelerContext = CancelContext | TimerContext

fn context_name(c Context) string {
	return typeof(c)
}

// deadline returns the time when work done on behalf of this context
// should be canceled. deadline returns none when no deadline is
// set. Successive calls to deadline return the same results.
pub fn (ctx Context) deadline() ?time.Time {
	match ctx {
		EmptyContext {
			return ctx.deadline()
		}
		CancelContext {
			return ctx.deadline()
		}
		TimerContext {
			return ctx.deadline()
		}
		CancelerContext {
			return ctx.deadline()
		}
		ValueContext {
			return ctx.deadline()
		}
	}
}

// done returns a channel that's closed when work done on behalf of this
// context should be canceled. done may return nil if this context can
// never be canceled. Successive calls to done return the same value.
// The close of the done channel may happen asynchronously,
// after the cancel function returns.
//
// with_cancel arranges for done to be closed when cancel is called;
// with_deadline arranges for done to be closed when the deadline
// expires; with_timeout arranges for done to be closed when the timeout
// elapses.
pub fn (ctx Context) done() chan int {
	match mut ctx {
		EmptyContext {
			return ctx.done()
		}
		CancelContext {
			return ctx.done()
		}
		TimerContext {
			return ctx.done()
		}
		CancelerContext {
			return ctx.done()
		}
		ValueContext {
			return ctx.done()
		}
	}
}

// If done is not yet closed, err returns nil.
// If done is closed, err returns a non-nil error explaining why:
// canceled if the context was canceled
// or deadline_exceeded if the context's deadline passed.
// After err returns a non-nil error, successive calls to err return the same error.
pub fn (ctx Context) err() string {
	match ctx {
		EmptyContext {
			return ctx.err()
		}
		CancelContext {
			return ctx.err()
		}
		TimerContext {
			return ctx.err()
		}
		CancelerContext {
			return ctx.err()
		}
		ValueContext {
			return ctx.err()
		}
	}
}

// Value returns the value associated with this context for key, or nil
// if no value is associated with key. Successive calls to Value with
// the same key returns the same result.
//
// Use context values only for request-scoped data that transits
// processes and API boundaries, not for passing optional parameters to
// functions.
//
// A key identifies a specific value in a Context. Functions that wish
// to store values in Context typically allocate a key in a global
// variable then use that key as the argument to context.with_value and
// Context.Value. A key can be any type that supports equality;
// packages should define keys as an unexported type to avoid
// collisions.
pub fn (ctx Context) value(key voidptr) voidptr {
	match ctx {
		EmptyContext {
			return ctx.value(key)
		}
		CancelContext {
			return ctx.value(key)
		}
		TimerContext {
			return ctx.value(key)
		}
		CancelerContext {
			return ctx.value(key)
		}
		ValueContext {
			return ctx.value(key)
		}
	}
}

pub fn (ctx Context) str() string {
	match ctx {
		EmptyContext {
			return ctx.str()
		}
		CancelContext {
			return ctx.str()
		}
		TimerContext {
			return ctx.str()
		}
		CancelerContext {
			return ctx.str()
		}
		ValueContext {
			return ctx.str()
		}
	}
}

pub fn (c CancelerContext) cancel(remove_from_parent bool, err string) {
	match shared c {
		CancelContext {
			c.cancel(remove_from_parent, err)
		}
		TimerContext {
			c.cancel(remove_from_parent, err)
		}
	}
}

// deadline returns the time when work done on behalf of this context
// should be canceled. deadline returns none when no deadline is
// set. Successive calls to deadline return the same results.
pub fn (ctx CancelerContext) deadline() ?time.Time {
	match ctx {
		CancelContext {
			return ctx.deadline()
		}
		TimerContext {
			return ctx.deadline()
		}
	}
}

// done returns a channel that's closed when work done on behalf of this
// context should be canceled. done may return nil if this context can
// never be canceled. Successive calls to done return the same value.
// The close of the done channel may happen asynchronously,
// after the cancel function returns.
//
// with_cancel arranges for done to be closed when cancel is called;
// with_deadline arranges for done to be closed when the deadline
// expires; with_timeout arranges for done to be closed when the timeout
// elapses.
pub fn (ctx CancelerContext) done() chan int {
	match mut ctx {
		CancelContext {
			return ctx.done()
		}
		TimerContext {
			return ctx.done()
		}
	}
}

// If done is not yet closed, err returns nil.
// If done is closed, err returns a non-nil error explaining why:
// canceled if the context was canceled
// or deadline_exceeded if the context's deadline passed.
// After err returns a non-nil error, successive calls to err return the same error.
pub fn (ctx CancelerContext) err() string {
	match ctx {
		CancelContext {
			return ctx.err()
		}
		TimerContext {
			return ctx.err()
		}
	}
}

pub fn (ctx CancelerContext) str() string {
	match ctx {
		CancelContext {
			return ctx.str()
		}
		TimerContext {
			return ctx.str()
		}
	}
}

// Value returns the value associated with this context for key, or nil
// if no value is associated with key. Successive calls to Value with
// the same key returns the same result.
//
// Use context values only for request-scoped data that transits
// processes and API boundaries, not for passing optional parameters to
// functions.
//
// A key identifies a specific value in a Context. Functions that wish
// to store values in Context typically allocate a key in a global
// variable then use that key as the argument to context.with_value and
// Context.Value. A key can be any type that supports equality;
// packages should define keys as an unexported type to avoid
// collisions.
pub fn (ctx CancelerContext) value(key voidptr) voidptr {
	match ctx {
		CancelContext {
			return ctx.value(key)
		}
		TimerContext {
			return ctx.value(key)
		}
	}
}

// An EmptyContext is never canceled, has no values, and has no deadline. It is not
// struct{}, since vars of this type must have distinct addresses.
pub struct EmptyContext {}

pub fn (ctx EmptyContext) deadline() ?time.Time {
	return none
}

pub fn (ctx EmptyContext) done() chan int {
	ch := chan int{}
	ch.close()
	return ch
}

pub fn (ctx EmptyContext) err() string {
	return ''
}

pub fn (ctx EmptyContext) value(key voidptr) voidptr {
	return voidptr(0)
}

pub fn (ctx EmptyContext) str() string {
	if Context(ctx) == context.background {
		return 'context.Background'
	}
	if Context(ctx) == context.todo {
		return 'context.TODO'
	}
	return 'unknown empty Context'
}

// A CancelContext can be canceled. When canceled, it also cancels any children
// that implement CancelerContext.
pub struct CancelContext {
	context Context
mut:
	done     chan int
	children map[voidptr]bool
	err      string
}

// A CancelFunc tells an operation to abandon its work.
// A CancelFunc does not wait for the work to stop.
// A CancelFunc may be called by multiple goroutines simultaneously.
// After the first call, subsequent calls to a CancelFunc do nothing.
pub type CancelFunc = fn (c Context)

// with_cancel returns a copy of parent with a new done channel. The returned
// context's done channel is closed when the returned cancel function is called
// or when the parent context's done channel is closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_cancel(parent Context) (&Context, CancelFunc) {
	mut c := new_cancel_context(parent)
	propagate_cancel(parent, mut c)
	return &c, fn (c Context) {
		if c is CancelerContext {
			c.cancel(true, context.canceled)
		}
	}
}

// new_cancel_context returns an initialized CancelContext.
fn new_cancel_context(parent Context) CancelContext {
	return CancelContext{
		context: parent
	}
}

fn propagate_cancel(parent Context, mut child CancelerContext) {
	done := parent.done()
	select {
		_ := <-done {
			// parent is already canceled
			child.cancel(false, parent.err())
			return
		}
		else {}
	}
	mut p := parent_cancel_context(parent) or {
		go fn (parent Context, mut child CancelerContext) {
			pdone := parent.done()
			cdone := child.done()
			select {
				_ := <-pdone {
					child.cancel(false, parent.err())
				}
				_ := <-cdone {}
				else {}
			}
		}(parent, mut child)
		return
	}

	if p.err != '' {
		// parent has already been canceled
		child.cancel(false, p.err)
	} else {
		p.children[child] = true
	}
}

// parent_cancel_context returns the underlying CancelContext for parent.
// It does this by looking up parent.value(&cancel_context_key) to find
// the innermost enclosing CancelContext and then checking whether
// parent.done() matches that CancelContext. (If not, the CancelContext
// has been wrapped in a custom implementation providing a
// different done channel, in which case we should not bypass it.)
fn parent_cancel_context(parent Context) ?CancelContext {
	done := parent.done()
	if done.closed {
		return none
	}
	p_ptr := parent.value(context.cancel_context_key)
	if !isnil(p_ptr) {
		mut p := &CancelContext(p_ptr)
		pdone := p.done()
		if done == pdone {
			return *p
		}
	}
	return none
}

// remove_child removes a context from its parent.
fn remove_child(parent Context, child CancelerContext) {
	shared p := parent_cancel_context(parent) or { return }
	lock p {
		child_ptr := voidptr(&child)
		p.children[child_ptr] = false
	}
}

pub fn (ctx CancelContext) deadline() ?time.Time {
	return none
}

pub fn (ctx CancelContext) done() chan int {
	return ctx.done
}

pub fn (ctx CancelContext) err() string {
	return ctx.err
}

pub fn (ctx CancelContext) value(key voidptr) voidptr {
	if (*&byte(context.cancel_context_key.data)) == (*&byte(key)) {
		return voidptr(&ctx)
	}
	return ctx.context.value(key)
}

pub fn (shared ctx CancelContext) cancel(remove_from_parent bool, err string) {
	if err == '' {
		panic('context: internal error: missing cancel error')
	}

	lock ctx {
		ctx.done <- 0
		if !ctx.done.closed {
			ctx.done.close()
		}
	}

	for child_ptr, _ in ctx.children {
		shared child := &CancelContext(child_ptr)
		lock  {
			child.cancel(false, err)
		}
	}

	lock ctx {
		ctx.children = map[voidptr]bool{}

		if remove_from_parent {
			remove_child(ctx.context, CancelerContext(ctx))
		}
	}
}

pub fn (ctx CancelContext) str() string {
	return context_name(ctx.context) + '.with_cancel'
}

// A TimerContext carries a timer and a deadline. It embeds a CancelContext to
// implement done and err. It implements cancel by stopping its timer then
// delegating to CancelContext.cancel
pub struct TimerContext {
mut:
	cancel   CancelContext
	finished bool
	deadline time.Time
}

// with_deadline returns a copy of the parent context with the deadline adjusted
// to be no later than d. If the parent's deadline is already earlier than d,
// with_deadline(parent, d) is semantically equivalent to parent. The returned
// context's Done channel is closed when the deadline expires, when the returned
// cancel function is called, or when the parent context's Done channel is
// closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_deadline(parent Context, d time.Time) (&Context, CancelFunc) {
	if cur := parent.deadline() {
		if cur < d {
			// The current deadline is already sooner than the new one.
			return with_cancel(parent)
		}
	}
	cancel_ctx := new_cancel_context(parent)
	mut ctx := &TimerContext{
		cancel: cancel_ctx
		deadline: d
	}
	propagate_cancel(parent, mut ctx)
	dur := d - time.now()
	if dur.nanoseconds() <= 0 {
		ctx.cancel(true, context.deadline_exceeded) // deadline has already passed
		return &Context(ctx), fn (c Context) {
			if c is CancelerContext {
				c.cancel(true, context.canceled)
			}
		}
	}

	if ctx.cancel.err == '' {
		go fn (mut ctx TimerContext, dur time.Duration) {
			time.sleep(dur)
			ctx.cancel(true, context.deadline_exceeded)
		}(mut ctx, dur)
	}
	return &Context(ctx), fn (c Context) {
		if c is CancelerContext {
			c.cancel(true, context.canceled)
		}
	}
}

// with_timeout returns with_deadline(parent, time.now().add(timeout)).
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete
pub fn with_timeout(parent Context, timeout time.Duration) (&Context, CancelFunc) {
	return with_deadline(parent, time.now().add(timeout))
}

pub fn (ctx TimerContext) deadline() ?time.Time {
	return ctx.deadline
}

pub fn (ctx TimerContext) done() chan int {
	return ctx.cancel.done()
}

pub fn (ctx TimerContext) err() string {
	return ctx.cancel.err()
}

pub fn (ctx TimerContext) value(key voidptr) voidptr {
	return ctx.cancel.value(key)
}

pub fn (mut ctx TimerContext) cancel(remove_from_parent bool, err string) {
	ctx.cancel.cancel(false, err)
	if remove_from_parent {
		// Remove this TimerContext from its parent CancelContext's children.
		remove_child(ctx.cancel.context, CancelerContext(ctx))
	}
	ctx.finished = true
}

pub fn (ctx TimerContext) str() string {
	return context_name(ctx.cancel.context) + '.with_deadline(' + ctx.deadline.str() + ' [' +
		(time.now() - ctx.deadline).str() + '])'
}

// A ValueContext carries a key-value pair. It implements Value for that key and
// delegates all other calls to the embedded Context.
pub struct ValueContext {
	context Context
	key     voidptr
	value   voidptr
}

// with_value returns a copy of parent in which the value associated with key is
// val.
//
// Use context Values only for request-scoped data that transits processes and
// APIs, not for passing optional parameters to functions.
//
// The provided key must be comparable and should not be of type
// string or any other built-in type to avoid collisions between
// packages using context. Users of with_value should define their own
// types for keys
pub fn with_value(parent Context, key voidptr, value voidptr) &Context {
	if isnil(key) {
		panic('nil key')
	}
	return &ValueContext{parent, key, value}
}

pub fn (ctx ValueContext) deadline() ?time.Time {
	return ctx.context.deadline()
}

pub fn (ctx ValueContext) done() chan int {
	return ctx.context.done()
}

pub fn (ctx ValueContext) err() string {
	return ctx.context.err()
}

pub fn (ctx ValueContext) value(key voidptr) voidptr {
	if (*&byte(ctx.key)) == (*&byte(key)) {
		return ctx.value
	}
	return ctx.context.value(key)
}

pub fn (ctx ValueContext) str() string {
	return context_name(ctx.context) + '.with_value'
}
