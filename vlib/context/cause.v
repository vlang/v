// This module defines the Context type, which carries deadlines, cancellation signals,
// and other request-scoped values across API boundaries and between processes.
// Based on:   https://github.com/golang/go/tree/master/src/context
// Last commit: https://github.com/golang/go/commit/52bf14e0e8bdcd73f1ddfb0c4a1d0200097d3ba2
module context

import rand
import time

// cause_context_key is the value-context key used to store the cause in a CauseContext.
const cause_context_key = Key('context.CauseContext')

// CancelCauseFunc is a cancel function that accepts an optional cause error.
// Calling it with a non-none cause records that cause; calling it with none
// is equivalent to calling a plain CancelFn.
pub type CancelCauseFunc = fn (cause IError)

// A CauseContext wraps a CancelContext and records a cause error set via
// CancelCauseFunc. It implements the Context interface by delegating to the
// embedded CancelContext.
@[heap]
struct CauseContext {
	id       string
	deadline time.Time
mut:
	cancel_ctx CancelContext
	cause      IError = none
}

// with_cancel_cause behaves like with_cancel but returns a CancelCauseFunc
// instead of a CancelFn. Calling the returned function with a non-none error
// records that error as the cause, retrievable via cause(ctx). Calling it with
// none is identical to a plain cancel.
//
// Example:
//   mut bg := context.background()
//   mut ctx, cancel := context.with_cancel_cause(mut bg)
//   defer { cancel(none) }
//   cancel(my_err)
//   assert context.cause(ctx).str() == my_err.str()
pub fn with_cancel_cause(mut parent Context) (Context, CancelCauseFunc) {
	id := rand.uuid_v4()
	inner := new_cancel_context(parent)
	mut ctx := &CauseContext{
		id:         id
		cancel_ctx: inner
	}
	propagate_cancel(mut parent, mut ctx)
	cancel_fn := fn [mut ctx] (cause IError) {
		if cause !is none {
			ctx.cancel_ctx.mutex.lock()
			if ctx.cause is none {
				ctx.cause = cause
			}
			ctx.cancel_ctx.mutex.unlock()
		}
		ctx.cancel_ctx.cancel(true, canceled)
	}
	return Context(ctx), CancelCauseFunc(cancel_fn)
}

// with_deadline_cause behaves like with_deadline but accepts a cause error that
// is recorded when the deadline fires (instead of the generic deadline_exceeded).
// If cancel is called before the deadline, that cancellation takes effect instead.
pub fn with_deadline_cause(mut parent Context, d time.Time, deadline_cause IError) (Context, CancelFn) {
	if cur := parent.deadline() {
		if cur < d {
			return with_cancel(mut parent)
		}
	}
	inner := new_cancel_context(parent)
	id := rand.uuid_v4()
	mut ctx := &CauseContext{
		id:         id
		cancel_ctx: inner
		deadline:   d
	}
	propagate_cancel(mut parent, mut ctx)
	dur := d - time.now()
	if dur.nanoseconds() <= 0 {
		// deadline already passed
		ctx.cause = deadline_cause
		ctx.cancel_ctx.cancel(false, deadline_exceeded)
		cancel_fn := fn [mut ctx] () {
			ctx.cancel_ctx.cancel(true, canceled)
		}
		return Context(ctx), CancelFn(cancel_fn)
	}
	spawn fn (mut ctx CauseContext, dur time.Duration, deadline_cause IError) {
		time.sleep(dur)
		ctx.cancel_ctx.mutex.lock()
		// Only record deadline cause if the context wasn't already canceled,
		// so that the first cancellation's cause (manual or from parent) wins.
		if ctx.cancel_ctx.err is none {
			ctx.cause = deadline_cause
		}
		ctx.cancel_ctx.mutex.unlock()
		ctx.cancel_ctx.cancel(true, deadline_exceeded)
	}(mut ctx, dur, deadline_cause)
	cancel_fn := fn [mut ctx] () {
		ctx.cancel_ctx.cancel(true, canceled)
	}
	return Context(ctx), CancelFn(cancel_fn)
}

// with_timeout_cause is a convenience wrapper around with_deadline_cause using
// a relative duration instead of an absolute time.
pub fn with_timeout_cause(mut parent Context, timeout time.Duration, deadline_cause IError) (Context, CancelFn) {
	return with_deadline_cause(mut parent, time.now().add(timeout), deadline_cause)
}

// cause returns the cause of the context's cancellation.
// If ctx was canceled via CancelCauseFunc with a non-none cause, that cause is
// returned. Otherwise, cause returns ctx.err() (i.e. canceled or
// deadline_exceeded).
pub fn cause(ctx Context) IError {
	mut mctx := ctx
	if val := mctx.value(cause_context_key) {
		match val {
			CauseContext {
				val.cancel_ctx.mutex.lock()
				c := val.cause
				val.cancel_ctx.mutex.unlock()
				if c !is none {
					return c
				}
			}
			else {}
		}
	}
	return mctx.err()
}

// --- CauseContext implements Context and Canceler ---

// deadline returns the deadline for the context, or none if no deadline is set.
pub fn (ctx &CauseContext) deadline() ?time.Time {
	if ctx.deadline != time.Time{} {
		return ctx.deadline
	}
	return none
}

// done returns a channel that is closed when the context is canceled.
pub fn (mut ctx CauseContext) done() chan int {
	return ctx.cancel_ctx.done()
}

// err returns the error that canceled the context, or none if not canceled.
pub fn (mut ctx CauseContext) err() IError {
	return ctx.cancel_ctx.err()
}

// value returns the cause context itself when the key matches cause_context_key,
// otherwise delegates to the embedded cancel context.
pub fn (ctx &CauseContext) value(key Key) ?Any {
	if key == cause_context_key {
		return ctx
	}
	return ctx.cancel_ctx.value(key)
}

// str returns a string representation of the context (e.g. 'EmptyContext.with_cancel_cause').
pub fn (ctx &CauseContext) str() string {
	return context_name(ctx.cancel_ctx.context) + '.with_cancel_cause'
}

// cancel cancels the context. If remove_from_parent is true, it also removes
// this context from its parent's children list. The err is recorded as the cause.
pub fn (mut ctx CauseContext) cancel(remove_from_parent bool, err IError) {
	// If being canceled with the generic 'context canceled' error and we
	// have a parent, propagate the parent's cause so children can retrieve it.
	// We walk the value chain directly instead of calling cause(): cause()
	// would fall through to err() on a parent CancelContext, which would
	// deadlock here because this cancel is typically invoked from within a
	// parent's cancel() while that parent's mutex is held. The cause field
	// on a CauseContext is always assigned before its cancel propagates, so
	// reading it without locking is safe along this downward path.
	if err.str() == 'context canceled' {
		match ctx.cancel_ctx.context {
			EmptyContext {}
			else {
				mut parent := ctx.cancel_ctx.context
				if val := parent.value(cause_context_key) {
					match val {
						CauseContext {
							if val.cause !is none {
								ctx.cause = val.cause
							}
						}
						else {}
					}
				}
			}
		}
	}
	ctx.cancel_ctx.cancel(false, err)
	if remove_from_parent {
		mut cc := &ctx.cancel_ctx.context
		remove_child(mut cc, ctx)
	}
}
