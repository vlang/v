// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import rand
import sync
import time

pub type CancelFn = fn ()

pub interface Canceler {
	id string
mut:
	cancel(remove_from_parent bool, err IError)
	done() chan int
}

// A CancelContext can be canceled. When canceled, it also cancels any children
// that implement Canceler.
pub struct CancelContext {
	id string
mut:
	context  Context
	mutex    &sync.Mutex = sync.new_mutex()
	done     chan int
	children map[string]Canceler
	err      IError = none
	cause    IError = none
}

// with_cancel returns a copy of parent with a new done channel. The returned
// context's done channel is closed when the returned cancel function is called
// or when the parent context's done channel is closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_cancel(mut parent Context) (Context, CancelFn) {
	mut c := new_cancel_context(parent)
	propagate_cancel(mut parent, mut c)
	cancel_fn := fn [mut c] () {
		c.cancel(true, canceled)
	}
	return Context(c), CancelFn(cancel_fn)
}

// new_cancel_context returns an initialized CancelContext.
fn new_cancel_context(parent Context) &CancelContext {
	return &CancelContext{
		id:      rand.uuid_v4()
		context: parent
		mutex:   sync.new_mutex()
		done:    chan int{cap: 2}
		err:     none
	}
}

// deadline always returns none for a CancelContext, since it has no deadline.
pub fn (ctx &CancelContext) deadline() ?time.Time {
	return none
}

// done returns the done channel, which is closed when this context is canceled.
pub fn (mut ctx CancelContext) done() chan int {
	ctx.mutex.lock()
	done := ctx.done
	ctx.mutex.unlock()
	return done
}

// err returns `canceled` after the context has been canceled, or `none` if not yet canceled.
pub fn (mut ctx CancelContext) err() IError {
	ctx.mutex.lock()
	err := ctx.err
	ctx.mutex.unlock()
	return err
}

// value returns the CancelContext itself if the key matches the cancel context key,
// otherwise delegates to the parent context.
pub fn (ctx &CancelContext) value(key Key) ?Any {
	if key == cancel_context_key {
		return ctx
	}
	return ctx.context.value(key)
}

// str returns a string representation of the CancelContext,
// showing the parent context name suffixed with '.with_cancel'.
pub fn (ctx &CancelContext) str() string {
	return context_name(ctx.context) + '.with_cancel'
}

fn (mut ctx CancelContext) cancel(remove_from_parent bool, err IError) {
	ctx.cancel_with_cause(remove_from_parent, err, err)
}

fn (mut ctx CancelContext) cancel_with_cause(remove_from_parent bool, err IError, cancellation_cause IError) {
	if err is none {
		panic('context: internal error: missing cancel error')
	}

	ctx.mutex.lock()
	if ctx.err !is none {
		ctx.mutex.unlock()
		// already canceled
		return
	}

	ctx.err = err
	ctx.cause = if cancellation_cause is none { err } else { cancellation_cause }

	if !ctx.done.closed {
		ctx.done <- 0
		ctx.done.close()
	}

	for _, mut child in ctx.children {
		// NOTE: acquiring the child's lock while holding parent's lock.
		cancel_canceler(mut child, false, err, ctx.cause)
	}

	// The private map has no aliases, and cancellation prevents future inserts.
	// Release its values too, so a live canceled parent does not retain children.
	unsafe { ctx.children.free() }
	ctx.mutex.unlock()

	if remove_from_parent {
		mut cc := &ctx.context
		remove_child(mut cc, ctx)
	}
}

// Only wrappers owned by this module are safe to unwrap. An external context
// can override done() even when it embeds one of these parent implementations.
fn parent_never_cancels(parent Context) bool {
	mut current := parent
	for {
		match current {
			BackgroundContext, TodoContext, EmptyContext { return true }
			ValueContext { current = current.context }
			else { return false }
		}
	}
	return false
}

// Keep the public Canceler interface unchanged while passing the winning cause
// through the concrete implementations without relocking a canceled parent.
fn cancel_canceler(mut child Canceler, remove_from_parent bool, err IError, cancellation_cause IError) {
	match mut child {
		CancelContext { child.cancel_with_cause(remove_from_parent, err, cancellation_cause) }
		TimerContext { child.cancel_with_cause(remove_from_parent, err, cancellation_cause) }
		CauseContext { child.cancel_with_cause(remove_from_parent, err, cancellation_cause) }
		else { child.cancel(remove_from_parent, err) }
	}
}

fn propagate_cancel(mut parent Context, mut child Canceler) {
	if parent_never_cancels(parent) { return }
	done := parent.done()
	// A non-blocking V select skips already-closed receive channels. Context
	// cancellation is a close notification, so check that state directly.
	if done.closed {
		cancel_canceler(mut child, false, parent.err(), cause(parent))
		return
	}
	mut p := parent_cancel_context(mut parent) or {
		child_done := child.done()
		// Copy interface containers into the worker. Their headers may belong to
		// a caller's stack, and the caller can also rebind its parent variable.
		spawn wait_for_parent(parent, child, done, child_done)
		return
	}

	p.mutex.lock()
	if p.err is none {
		p.children[child.id] = child
	} else {
		cancel_canceler(mut child, false, p.err, p.cause)
	}
	p.mutex.unlock()
}

struct CancellationWorker {
mut:
	parent Context
	child  Canceler
}

// These waits run first in fresh context workers, before any user method can
// allocate values that escape. Their channel-select storage is wholly local.
@[inline]
fn release_wait_storage() {
	$if prealloc {
		unsafe { prealloc_thread_cleanup() }
	}
}

fn wait_for_parent_signal(parent_done chan int, child_done chan int) bool {
	// Release select storage before parent methods can create escaping errors.
	defer { release_wait_storage() }
	select {
		_ := <-parent_done {
			return true
		}
		_ := <-child_done {
			return false
		}
	}
	return false
}

fn wait_for_parent(parent_value Context, child_value Canceler, parent_done chan int, child_done chan int) {
	// A mutable local interface can be implicitly heap-boxed by the compiler.
	// Own these headers explicitly, independent of a worker's prealloc arena.
	mut worker := unsafe { &CancellationWorker(vcalloc_heap(int(sizeof(CancellationWorker)))) }
	defer { unsafe { vfree_heap(worker) } }
	worker.parent = parent_value
	worker.child = child_value
	if wait_for_parent_signal(parent_done, child_done) {
		cancel_canceler(mut worker.child, false, worker.parent.err(), cause(worker.parent))
	}
}

// parent_cancel_context returns the underlying CancelContext for parent.
// It does this by looking up parent.value(&cancel_context_key) to find
// the innermost enclosing CancelContext and then checking whether
// parent.done() matches that CancelContext. (If not, the CancelContext
// has been wrapped in a custom implementation providing a
// different done channel, in which case we should not bypass it.)
fn parent_cancel_context(mut parent Context) ?&CancelContext {
	done := parent.done()
	mut p := parent.value(cancel_context_key)?
	match mut p {
		CancelContext {
			pdone := p.done()
			if done == pdone {
				return p
			}
		}
		else {}
	}

	return none
}

// remove_child removes a context from its parent.
fn remove_child(mut parent Context, child Canceler) {
	mut p := parent_cancel_context(mut parent) or { return }
	p.mutex.lock()
	p.children.delete(child.id)
	p.mutex.unlock()
}
