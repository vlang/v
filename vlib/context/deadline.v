// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import rand
import time

// A TimerContext carries a deadline and a cancelable deadline wait. It embeds
// a CancelContext to implement done and err. Canceling closes the done channel
// and releases its waiting worker before the deadline is reached.
pub struct TimerContext {
	id string
mut:
	cancel_ctx CancelContext
	deadline   time.Time
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
pub fn with_deadline(mut parent Context, d time.Time) (Context, CancelFn) {
	id := rand.uuid_v4()
	if cur := parent.deadline() {
		if cur < d {
			// The current deadline is already sooner than the new one.
			return with_cancel(mut parent)
		}
	}
	cancel_ctx := new_cancel_context(parent)
	mut ctx := &TimerContext{
		cancel_ctx: cancel_ctx
		deadline:   d
		id:         id
	}
	propagate_cancel(mut parent, mut ctx)
	dur := d - time.now()
	if dur.nanoseconds() <= 0 {
		ctx.cancel(true, deadline_exceeded) // deadline has already passed
		cancel_fn := fn [mut ctx] () {
			ctx.cancel(true, canceled)
		}
		return Context(ctx), CancelFn(cancel_fn)
	}

	if ctx.err() is none {
		start_deadline_wait(ctx, dur, deadline_exceeded)
	}

	cancel_fn := fn [mut ctx] () {
		ctx.cancel(true, canceled)
	}
	return Context(ctx), CancelFn(cancel_fn)
}

// with_timeout returns with_deadline(parent, time.now().add(timeout)).
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete
pub fn with_timeout(mut parent Context, timeout time.Duration) (Context, CancelFn) {
	return with_deadline(mut parent, time.now().add(timeout))
}

// deadline returns the deadline for this context.
pub fn (ctx &TimerContext) deadline() ?time.Time {
	return ctx.deadline
}

// done returns the done channel from the embedded CancelContext.
pub fn (mut ctx TimerContext) done() chan int {
	return ctx.cancel_ctx.done()
}

// err returns the error from the embedded CancelContext.
pub fn (mut ctx TimerContext) err() IError {
	return ctx.cancel_ctx.err()
}

// value returns the value associated with the given key by delegating
// to the embedded CancelContext.
pub fn (ctx &TimerContext) value(key Key) ?Any {
	return ctx.cancel_ctx.value(key)
}

// cancel cancels the timer context by delegating to the embedded CancelContext
// and optionally removing itself from its parent's children.
pub fn (mut ctx TimerContext) cancel(remove_from_parent bool, err IError) {
	ctx.cancel_with_cause(remove_from_parent, err, err)
}

fn (mut ctx TimerContext) cancel_with_cause(remove_from_parent bool, err IError, cancellation_cause IError) {
	ctx.cancel_ctx.cancel_with_cause(false, err, cancellation_cause)
	if remove_from_parent {
		// Remove this TimerContext from its parent CancelContext's children.
		mut cc := &ctx.cancel_ctx.context
		remove_child(mut cc, ctx)
	}
}

// str returns a string representation of the TimerContext, showing the
// parent context name, the deadline time, and the duration remaining.
pub fn (ctx &TimerContext) str() string {
	return context_name(ctx.cancel_ctx.context) + '.with_deadline(' + ctx.deadline.str() + ' [' +
		(time.now() - ctx.deadline).str() + '])'
}

// Duration select uses the channel runtime's cancelable monotonic wait. Account
// for worker scheduling time rather than restarting the duration in the worker.
fn start_deadline_wait(child Canceler, duration time.Duration, cancellation_cause IError) {
	mut observed := child
	done := observed.done()
	started := time.sys_mono_now()
	spawn wait_for_deadline(child, done, duration, started, cancellation_cause)
}

fn wait_for_deadline_signal(done chan int, remaining time.Duration) bool {
	defer { release_wait_storage() }
	select {
		_ := <-done {
			return false
		}
		remaining {
			return true
		}
	}
	return false
}

fn wait_for_deadline(child_value Canceler, done chan int, duration time.Duration, started u64, cancellation_cause IError) {
	mut worker := unsafe { &CancellationWorker(vcalloc_heap(int(sizeof(CancellationWorker)))) }
	defer { unsafe { vfree_heap(worker) } }
	worker.child = child_value
	elapsed := time.sys_mono_now() - started
	if elapsed < u64(duration) && !wait_for_deadline_signal(done, duration - time.Duration(elapsed)) {
		return
	}
	cancel_canceler(mut worker.child, true, deadline_exceeded, cancellation_cause)
}
