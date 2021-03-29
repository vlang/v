// Package context defines the Context type, which carries deadlines,
// cancellation signals, and other request-scoped values across API boundaries
// and between processes.
//
// Incoming requests to a server should create a Context, and outgoing
// calls to servers should accept a Context. The chain of function
// calls between them must propagate the Context, optionally replacing
// it with a derived Context created using with_cancel, Withdeadline,
// with_timeout, or with_value. When a Context is canceled, all
// Contexts derived from it are also canceled.
//
// The with_cancel, Withdeadline, and with_timeout functions take a
// Context (the parent) and return a derived Context (the child) and a
// CancelFunc. Calling the CancelFunc cancels the child and its
// children, removes the parent's reference to the child, and stops
// any associated timers. Failing to call the CancelFunc leaks the
// child and its children until the parent is canceled or the timer
// fires. The go vet tool checks that CancelFuncs are used on all
// control-flow paths.
//
// Programs that use Contexts should follow these rules to keep interfaces
// consistent across packages and enable static analysis tools to check context
// propagation:
//
// Do not store Contexts inside a struct type; instead, pass a Context
// explicitly to each function that needs it. The Context should be the first
// parameter, typically named ctx:
//
// 	pub fn do_something(ctx context.context, arg Arg) ?int {
// 		// ... use ctx ...
// 	}
//
// Do not pass a nil Context, even if a function permits it. Pass context.todo
// if you are unsure about which Context to use.
//
// Use context values only for request-scoped data that transits processes and
// APIs, not for passing optional parameters to functions.
//
// The same Context may be passed to functions running in different routines;
// Contexts are safe for simultaneous use by multiple routines.
//

module context

import sync
import sync.atomic2
import time

pub const (
	// canceled is the error returned by Context.err when the context is canceled.
	canceled          = 'context canceled'

	// deadlineExceeded is the error returned by Context.err when the context's
	// deadline passes.
	deadline_exceeded = 'context deadline exceeded'

	background        = EmptyCtx(0)
	todo              = EmptyCtx(0)
)

pub interface ContextKey {}

pub interface ContextValue {
	str() string
}

// A Context carries a deadline, a cancellation signal, and other values across
// API boundaries.
//
// Context's methods may be called by multiple routines simultaneously.
pub interface Context {
	// deadline returns the time when work done on behalf of this context
	// should be canceled. deadline returns ok==false when no deadline is
	// set. Successive calls to deadline return the same results.
	deadline() ?time.Time
	// done returns a channel that's closed when work done on behalf of this
	// context should be canceled. done may return an option if this context can
	// never be canceled. Successive calls to done return the same value.
	// The close of the done channel may happen asynchronously,
	// after the cancel function returns.
	done() chan int
	// If done is not yet closed, err returns none.
	// If done is closed, err returns a non-none error explaining why:
	// canceled if the context was canceled
	// or deadlineExceeded if the context's deadline passed.
	// After err returns a non-none error, successive calls to err return the same error.
	err() ?int
	// value returns the value associated with this context for key, or nil
	// if no value is associated with key. Successive calls to value with
	// the same key returns the same result.
	//
	// Use context values only for request-scoped data that transits
	// processes and API boundaries, not for passing optional parameters to
	// functions.
	//
	// A key identifies a specific value in a Context. Functions that wish
	// to store values in Context typically allocate a key in a global
	// variable then use that key as the argument to context.with_value and
	// Context.value. A key can be any type that supports equality;
	// packages should define keys as an unexported type to avoid
	// collisions.
	value(key ContextKey) ?ContextValue
}

// An EmptyCtx is never canceled, has no values, and has no deadline. It is not
// int, since vars of this type must have distinct addresses.
pub type EmptyCtx = int

pub fn (_ EmptyCtx) deadline() ?time.Time {
	return time.now()
}

pub fn (_ EmptyCtx) done() chan int {
	return chan int{}
}

pub fn (_ EmptyCtx) err() ?int {
	return none
}

pub fn (_ EmptyCtx) value(key ContextKey) ?ContextValue {
	return none
}

pub fn (e EmptyCtx) str() string {
	return match e {
		context.background {
			'context.background'
		}
		context.todo {
			'context.todo'
		}
		else {
			'unknown empty Context'
		}
	}
}

// background returns a non-nil, empty Context. It is never canceled, has no
// values, and has no deadline. It is typically used by the main function,
// initialization, and tests, and as the top-level Context for incoming
// requests.
pub fn background() Context {
	return context.background
}

// todo returns a non-nil, empty Context. Code should use context.todo when
// it's unclear which Context to use or it is not yet available (because the
// surrounding function has not yet been extended to accept a Context
// parameter).
pub fn todo() Context {
	return context.todo
}

// A CancelFunc tells an operation to abandon its work.
// A CancelFunc does not wait for the work to stop.
// A CancelFunc may be called by multiple routines simultaneously.
// After the first call, subsequent calls to a CancelFunc do nothing.
pub type CancelFunc = fn (mut context CancelCtx)

pub type CancelerErr = string

// with_cancel returns a copy of parent with a new done channel. The returned
// context's done channel is closed when the returned cancel function is called
// or when the parent context's done channel is closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_cancel(parent &Context) (&Context, CancelFunc) {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	mut c := new_cancel_ctx(parent)
	propagate_cancel(parent, c)
	return &c, fn (mut c CancelCtx) {
		c.cancel(true, context.canceled)
	}
}

// new_cancel_ctx returns an initialized CancelCtx.
pub fn new_cancel_ctx(parent &Context) CancelCtx {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	return CancelCtx{
		context: parent
	}
}

// routines counts the number of routines ever created; for testing.
__global ( routines i64 )

// propagate_cancel arranges for child to be canceled when parent is.
fn propagate_cancel(parent &Context, child Canceler) {
	done := parent.done()
	select {
		_ := <-done {
			// parent is already canceled
			child.cancel(false, '')
			return
		}
		else {}
	}

	mut p := parent_cancel_ctx(parent) or {
		atomic2.add_i64(&routines, 1)
		go fn (parent &Context, child Canceler) {
			parent_ch := parent.done()
			child_ch := child.done()
			select {
				_ := <-parent_ch {
					child.cancel(false, '')
				}
				_ := <-child_ch {}
			}
		}(parent, child)
		return
	}

	p.mu.@lock()
	p.err() or {
		// parent has already been canceled
		child.cancel(false, p.err)
	}
	p.children << child
	p.mu.unlock()
}

// cancel_ctx_key is the key that a cancel_ctx returns itself for.
__global ( cancel_ctx_key ContextKey )

// parent_cancel_ctx returns the underlying CancelCtx for parent.
// It does this by looking up parent.value(cancel_ctx_key) to find
// the innermost enclosing CancelCtx and then checking whether
// parent.done() matches that CancelCtx. (If not, the CancelCtx
// has been wrapped in a custom implementation providing a
// different done channel, in which case we should not bypass it.)
fn parent_cancel_ctx(parent &Context) ?&CancelCtx {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	done := parent.done()
	if done == closed_chan {
		return none
	}
	mut p := new_cancel_ctx(parent)
	p.mu.@lock()
	ok := p.done == done
	p.mu.unlock()
	if !ok {
		return none
	}
	return &p
}

// remove_child removes a context from its parent.
fn remove_child(parent &Context, child Canceler) {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	mut p := parent_cancel_ctx(parent) or { return }
	p.mu.@lock()
	// TODO: FIX
	idx := 0
	if idx >= 0 {
		p.children.delete(idx)
	}
	p.mu.unlock()
}

// A Canceler is a context type that can be canceled directly. The
// implementations are CancelCtx and *timerCtx.
pub interface Canceler {
	cancel(remove_from_parent bool, err CancelerErr)
	done() chan int
}

// closed_chan is a reusable closed channel.
__global ( closed_chan chan int )

fn init() {
	closed_chan.close()
}

// A CancelCtx can be canceled. When canceled, it also cancels any children
// that implement Canceler.
pub struct CancelCtx {
mut:
	context &Context

	mu       sync.Mutex // protects following fields
	done     chan int   // created lazily, closed by first cancel call
	children []Canceler
	err      CancelerErr
}

pub fn (c CancelCtx) value(key ContextKey) ?ContextValue {
	if key == cancel_ctx_key {
		return c
	}
	return c.context.value(key)
}

pub fn (mut c CancelCtx) done() chan int {
	c.mu.@lock()
	c.done = chan int{}
	d := c.done
	c.mu.unlock()
	return d
}

pub fn (_ CancelCtx) deadline() ?time.Time {
	return none
}

pub fn (mut c CancelCtx) err() ?int {
	c.mu.@lock()
	err := c.err
	c.mu.unlock()
	return error(err)
}

fn context_name(c Context) string {
	return typeof(c)
}

pub fn (c CancelCtx) str() string {
	return context_name(c.context) + '.with_cancel'
}

// cancel closes c.done, cancels each of c's children, and, if
// remove_from_parent is true, removes c from its parent's children.
pub fn (mut c CancelCtx) cancel(remove_from_parent bool, err CancelerErr) {
	c.mu.@lock()
	c.err() or {
		c.mu.unlock()
		// already canceled
		return
	}
	c.err = err
	c.done.close()
	c.done = closed_chan
	for child in c.children {
		// NOTE: acquiring the child's lock while holding parent's lock.
		child.cancel(false, err)
	}
	c.children = []
	c.mu.unlock()

	if remove_from_parent {
		remove_child(c.context, c)
	}
}

// with_deadline returns a copy of the parent context with the deadline adjusted
// to be no later than d. If the parent's deadline is already earlier than d,
// with_deadline(parent, d) is semantically equivalent to parent. The returned
// context's done channel is closed when the deadline expires, when the returned
// cancel function is called, or when the parent context's done channel is
// closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_deadline(parent &Context, d time.Time) (&Context, CancelFunc) {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	if cur := parent.deadline() {
		if cur < d {
			// The current deadline is already sooner than the new one.
			return with_cancel(parent)
		}
	}
	cancel_ctx := new_cancel_ctx(parent)
	mut c := &TimerCtx{
		cancel_ctx: &cancel_ctx
		deadline: d
	}
	propagate_cancel(parent, c)
	dur := time.now() - d
	if dur.microseconds() <= 0 {
		c.cancel(true, context.deadline_exceeded) // deadline has already passed
		return c, fn (mut c CancelCtx) {
			c.cancel(false, context.canceled)
		}
	}
	c.cancel_ctx.mu.@lock()
	defer {
		c.cancel_ctx.mu.unlock()
	}
	if _ := c.cancel_ctx.err() {
		go fn (dur time.Duration, mut c TimerCtx) {
			time.sleep(dur)
			if !c.reached {
				c.cancel(true, context.deadline_exceeded)
			}
		}(dur, mut c)
	}
	return c, fn (mut c CancelCtx) {
		c.cancel(true, context.canceled)
	}
}

// A TimerCtx carries a timer and a deadline. It embeds a cancel_ctx to
// implement done and Err. It implements cancel by stopping its timer then
// delegating to cancel_ctx.cancel.
pub struct TimerCtx {
mut:
	cancel_ctx &CancelCtx
	reached    bool // Under cancel_ctx.mu
	deadline   time.Time
}

pub fn (c TimerCtx) deadline() ?time.Time {
	return c.deadline
}

pub fn (mut c TimerCtx) done() chan int {
	return c.cancel_ctx.done()
}

pub fn (mut c TimerCtx) err() ?int {
	return c.cancel_ctx.err()
}

pub fn (mut c TimerCtx) value(key ContextKey) ?ContextValue {
	return c.cancel_ctx.value(key)
}

pub fn (c TimerCtx) str() string {
	return context_name(c.cancel_ctx.context) + '.with_deadline(' + c.deadline.str() + ' [' +
		(time.now() - c.deadline).str() + '])'
}

pub fn (mut c TimerCtx) cancel(remove_from_parent bool, err CancelerErr) {
	c.cancel_ctx.cancel(false, err)
	if remove_from_parent {
		// Remove this TimerCtx from its parent cancel_ctx's children.
		remove_child(c.cancel_ctx.context, c)
	}
	c.cancel_ctx.mu.@lock()
	c.reached = true
	c.cancel_ctx.mu.unlock()
}

// with_timeout returns with_deadline(parent, time.Now().Add(timeout)).
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete:
//
// 	func slowOperationwith_timeout(ctx context.context) (Result, error) {
// 		ctx, cancel := context.with_timeout(ctx, 100*time.Millisecond)
// 		defer cancel()  // releases resources if slowOperation completes before timeout elapses
// 		return slowOperation(ctx)
// 	}
pub fn with_timeout(parent &Context, timeout time.Duration) (&Context, CancelFunc) {
	return with_deadline(parent, time.now().add(timeout))
}

// with_value returns a copy of parent in which the value associated with key is
// val.
//
// Use context values only for request-scoped data that transits processes and
// APIs, not for passing optional parameters to functions.
//
// The provided key must be comparable and should not be of type
// string or any other built-in type to avoid collisions between
// packages using context. Users of with_value should define their own
// types for keys. To avoid allocating when assigning to an
// any, context keys often have concrete type
// int. Alternatively, exported context key variables' static
// type should be a pointer or interface.
pub fn with_value(parent &Context, key ContextKey, val ContextValue) Context {
	if isnil(parent) {
		panic('cannot create context from nil parent')
	}
	if isnil(key) {
		panic('nil key')
	}
	return &ValueCtx{parent, key, val}
}

// A ValueCtx carries a key-value pair. It implements value for that key and
// delegates all other calls to the embedded Context.
pub struct ValueCtx {
	context &Context
	key     ContextKey
	val     ContextValue
}

pub fn (c ValueCtx) deadline() ?time.Time {
	return c.context.deadline()
}

pub fn (c ValueCtx) done() chan int {
	return c.context.done()
}

pub fn (c ValueCtx) err() ?int {
	return c.context.err()
}

pub fn (c ValueCtx) value(key ContextKey) ?ContextValue {
	if c.key == key {
		return c.val
	}
	return c.context.value(key)
}

pub fn (c ValueCtx) str() string {
	return context_name(c.context) + '.with_value(type ' + typeof(c.key).str() + ', val ' +
		c.val.str() + ')'
}
