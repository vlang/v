// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based off:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import rand
import time

// A TimerContext carries a timer and a deadline. It embeds a CancelContext to
// implement done and err. It implements cancel by stopping its timer then
// delegating to CancelContext.cancel
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
pub fn with_deadline(parent Context, d time.Time) Context {
	id := rand.uuid_v4()
	if cur := parent.deadline() {
		if cur < d {
			// The current deadline is already sooner than the new one.
			return with_cancel(parent)
		}
	}
	cancel_ctx := new_cancel_context(parent)
	mut ctx := &TimerContext{
		cancel_ctx: cancel_ctx
		deadline: d
		id: id
	}
	propagate_cancel(parent, mut ctx)
	dur := d - time.now()
	if dur.nanoseconds() <= 0 {
		ctx.cancel(true, deadline_exceeded) // deadline has already passed
		return Context(ctx)
	}

	if ctx.err().str() == 'none' {
		go fn (mut ctx TimerContext, dur time.Duration) {
			time.sleep(dur)
			ctx.cancel(true, deadline_exceeded)
		}(mut ctx, dur)
	}
	return Context(ctx)
}

// with_timeout returns with_deadline(parent, time.now().add(timeout)).
//
// Canceling this context releases resources associated with it, so code should
// call cancel as soon as the operations running in this Context complete
pub fn with_timeout(parent Context, timeout time.Duration) Context {
	return with_deadline(parent, time.now().add(timeout))
}

pub fn (ctx TimerContext) deadline() ?time.Time {
	return ctx.deadline
}

pub fn (mut ctx TimerContext) done() chan int {
	return ctx.cancel_ctx.done()
}

pub fn (mut ctx TimerContext) err() IError {
	return ctx.cancel_ctx.err()
}

pub fn (ctx TimerContext) value(key string) ?voidptr {
	return ctx.cancel_ctx.value(key)
}

pub fn (mut ctx TimerContext) cancel(remove_from_parent bool, err IError) {
	ctx.cancel_ctx.cancel(false, err)
	if remove_from_parent {
		// Remove this TimerContext from its parent CancelContext's children.
		remove_child(ctx.cancel_ctx.context, ctx)
	}
}

pub fn (ctx TimerContext) str() string {
	return context_name(ctx.cancel_ctx.context) + '.with_deadline(' + ctx.deadline.str() + ' [' +
		(time.now() - ctx.deadline).str() + '])'
}
