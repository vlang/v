module context

import rand
import sync
import time

pub interface Canceler {
	id string
	cancel(remove_from_parent bool, err string)
	done() chan int
	str() string
}

pub fn cancel(mut ctx CancelerContext) {
	match mut ctx {
		CancelContext {
			ctx.cancel(true, canceled)
		}
		TimerContext {
			ctx.cancel(true, canceled)
		}
	}
}

// CancelerContext implements the Canceler intarface for both
// struct types: CancelContext and TimerContext
pub type CancelerContext = CancelContext | TimerContext

pub fn (mut ctx CancelerContext) done() chan int {
	match mut ctx {
		CancelContext {
			return ctx.done()
		}
		TimerContext {
			return ctx.done()
		}
	}
}

pub fn (mut ctx CancelerContext) err() string {
	match mut ctx {
		CancelContext {
			return ctx.err()
		}
		TimerContext {
			return ctx.err()
		}
	}
}

pub fn (ctx CancelerContext) value(key string) ?voidptr {
	match ctx {
		CancelContext {
			return ctx.value(key)
		}
		TimerContext {
			return ctx.value(key)
		}
	}
}

pub fn (mut ctx CancelerContext) cancel(remove_from_parent bool, err string) {
	match mut ctx {
		CancelContext {
			ctx.cancel(remove_from_parent, err)
		}
		TimerContext {
			ctx.cancel(remove_from_parent, err)
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

// A CancelContext can be canceled. When canceled, it also cancels any children
// that implement Canceler.
pub struct CancelContext {
	id string
mut:
	context  Context
	mutex    &sync.Mutex
	done     chan int
	children map[string]Canceler
	err      string
}

// A CancelFunc tells an operation to abandon its work.
// A CancelFunc does not wait for the work to stop.
// A CancelFunc may be called by multiple goroutines simultaneously.
// After the first call, subsequent calls to a CancelFunc do nothing.
// pub type CancelFunc = fn (c Canceler)

// with_cancel returns a copy of parent with a new done channel. The returned
// context's done channel is closed when the returned cancel function is called
// or when the parent context's done channel is closed, whichever happens first.
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete.
pub fn with_cancel(parent Context) &CancelerContext {
	mut c := new_cancel_context(parent)
	propagate_cancel(parent, mut c)
	return c
}

// new_cancel_context returns an initialized CancelContext.
fn new_cancel_context(parent Context) &CancelContext {
	return &CancelContext{
		id: rand.uuid_v4()
		context: parent
		mutex: sync.new_mutex()
	}
}

pub fn (ctx CancelContext) deadline() ?time.Time {
	return none
}

pub fn (mut ctx CancelContext) done() chan int {
	ctx.mutex.@lock()
	done := ctx.done
	ctx.mutex.unlock()
	return done
}

pub fn (mut ctx CancelContext) err() string {
	ctx.mutex.@lock()
	err := ctx.err
	ctx.mutex.unlock()
	return err
}

pub fn (ctx CancelContext) value(key string) ?voidptr {
	if key == cancel_context_key {
		return voidptr(&ctx)
	}
	return ctx.context.value(key)
}

pub fn (ctx CancelContext) str() string {
	return context_name(ctx.context) + '.with_cancel'
}

fn (mut ctx CancelContext) cancel(remove_from_parent bool, err string) {
	if err == '' {
		panic('context: internal error: missing cancel error')
	}

	ctx.mutex.@lock()
	if ctx.err != '' {
		ctx.mutex.unlock()
		// already canceled
		return
	}

	ctx.err = err

	if !ctx.done.closed {
		ctx.done.close()
	}

	for _, child in ctx.children {
		// NOTE: acquiring the child's lock while holding parent's lock.
		child.cancel(false, err)
	}

	ctx.children = map[string]Canceler{}
	ctx.mutex.unlock()

	if remove_from_parent {
		remove_child(ctx.context, ctx)
	}
}

fn propagate_cancel(parent Context, mut child Canceler) {
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
		go fn (parent Context, mut child Canceler) {
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
		p.children[child.id] = child
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
	if p_ptr := parent.value(cancel_context_key) {
		if !isnil(p_ptr) {
			mut p := &CancelContext(p_ptr)
			pdone := p.done()
			if done == pdone {
				return *p
			}
		}
	}
	return none
}

// remove_child removes a context from its parent.
fn remove_child(parent Context, child Canceler) {
	mut p := parent_cancel_context(parent) or { return }
	p.children.delete(child.id)
}
