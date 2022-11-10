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

[deprecated]
pub fn cancel(mut ctx Context) {
	match mut ctx {
		CancelContext {
			ctx.cancel(true, canceled)
		}
		TimerContext {
			ctx.cancel(true, canceled)
		}
		else {}
	}
}

// A CancelContext can be canceled. When canceled, it also cancels any children
// that implement Canceler.
pub struct CancelContext {
	id string
mut:
	context  Context
	mutex    &sync.Mutex
	done     chan int
	children map[string]Canceler
	err      IError
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
		id: rand.uuid_v4()
		context: parent
		mutex: sync.new_mutex()
		done: chan int{cap: 2}
		err: none
	}
}

pub fn (ctx &CancelContext) deadline() ?time.Time {
	return none
}

pub fn (mut ctx CancelContext) done() chan int {
	ctx.mutex.@lock()
	done := ctx.done
	ctx.mutex.unlock()
	return done
}

pub fn (mut ctx CancelContext) err() IError {
	ctx.mutex.@lock()
	err := ctx.err
	ctx.mutex.unlock()
	return err
}

pub fn (ctx &CancelContext) value(key Key) ?Any {
	if key == cancel_context_key {
		return ctx
	}
	return ctx.context.value(key)
}

pub fn (ctx &CancelContext) str() string {
	return context_name(ctx.context) + '.with_cancel'
}

fn (mut ctx CancelContext) cancel(remove_from_parent bool, err IError) {
	if err is none {
		panic('context: internal error: missing cancel error')
	}

	ctx.mutex.@lock()
	if ctx.err !is none {
		ctx.mutex.unlock()
		// already canceled
		return
	}

	ctx.err = err

	if !ctx.done.closed {
		ctx.done <- 0
		ctx.done.close()
	}

	for _, child in ctx.children {
		// NOTE: acquiring the child's lock while holding parent's lock.
		mut c := child
		c.cancel(false, err)
	}

	ctx.children = map[string]Canceler{}
	ctx.mutex.unlock()

	if remove_from_parent {
		mut cc := &ctx.context
		remove_child(mut cc, ctx)
	}
}

fn propagate_cancel(mut parent Context, mut child Canceler) {
	done := parent.done()
	select {
		_ := <-done {
			// parent is already canceled
			child.cancel(false, parent.err())
			return
		}
	}
	mut p := parent_cancel_context(mut parent) or {
		spawn fn (mut parent Context, mut child Canceler) {
			pdone := parent.done()
			select {
				_ := <-pdone {
					child.cancel(false, parent.err())
				}
			}
		}(mut parent, mut child)
		return
	}

	if p.err is none {
		p.children[child.id] = child
	} else {
		// parent has already been canceled
		child.cancel(false, p.err)
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
	if done.closed {
		return none
	}
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
	p.children.delete(child.id)
}
