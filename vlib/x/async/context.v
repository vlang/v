module async

import context
import sync
import time

// AsyncContext is the small `context.Context` implementation owned by x.async.
//
// The public API still accepts and returns the standard `context.Context`
// interface. Internally, x.async needs a derived context whose cancellation is
// simple, idempotent, and non-blocking for Group and timeout lifecycles.
//
// This is not a scheduler or runtime. It is only the cancellation signal shared
// by jobs launched through this module.
@[heap]
struct AsyncContext {
mut:
	parent       context.Context
	done         chan int
	mutex        &sync.Mutex = sync.new_mutex()
	err          IError      = none
	has_deadline bool
	deadline_at  time.Time
}

fn new_cancel_context(parent context.Context) (&AsyncContext, context.CancelFn) {
	mut ctx := &AsyncContext{
		parent: parent
		done:   chan int{cap: 1}
		mutex:  sync.new_mutex()
	}
	if !ctx.propagate_existing_parent_error() {
		spawn watch_parent_context(mut ctx)
	}
	cancel_fn := fn [mut ctx] () {
		ctx.cancel_with(error(context_canceled))
	}
	return ctx, context.CancelFn(cancel_fn)
}

fn new_timeout_context(parent context.Context, timeout time.Duration) (&AsyncContext, context.CancelFn) {
	mut deadline_at := time.now().add(timeout)
	if parent_deadline := parent.deadline() {
		if parent_deadline < deadline_at {
			deadline_at = parent_deadline
		}
	}
	mut ctx := &AsyncContext{
		parent:       parent
		done:         chan int{cap: 1}
		mutex:        sync.new_mutex()
		has_deadline: true
		deadline_at:  deadline_at
	}
	if !ctx.propagate_existing_parent_error() {
		spawn watch_parent_context(mut ctx)
	}
	if timeout.nanoseconds() <= 0 {
		ctx.cancel_with(error(context_deadline_exceeded))
	} else {
		spawn watch_timeout_context(mut ctx, timeout)
	}
	cancel_fn := fn [mut ctx] () {
		ctx.cancel_with(error(context_canceled))
	}
	return ctx, context.CancelFn(cancel_fn)
}

// deadline returns x.async's own deadline when present, otherwise the parent's
// deadline. This preserves deadline metadata for callers that inspect it.
pub fn (ctx &AsyncContext) deadline() ?time.Time {
	if ctx.has_deadline {
		return ctx.deadline_at
	}
	return ctx.parent.deadline()
}

// value delegates to the parent context. x.async does not add request values.
pub fn (ctx &AsyncContext) value(key context.Key) ?context.Any {
	return ctx.parent.value(key)
}

// done returns the cancellation channel shared by jobs using this context.
pub fn (mut ctx AsyncContext) done() chan int {
	ctx.mutex.lock()
	done := ctx.done
	ctx.mutex.unlock()
	return done
}

// err returns the local cancellation reason, or the parent's reason if the
// parent is already canceled before propagation reaches this context.
pub fn (mut ctx AsyncContext) err() IError {
	ctx.mutex.lock()
	err := ctx.err
	ctx.mutex.unlock()
	if err !is none {
		return err
	}
	return ctx.parent.err()
}

fn (mut ctx AsyncContext) cancel_with(err IError) {
	if err is none {
		return
	}
	ctx.mutex.lock()
	if ctx.err !is none {
		ctx.mutex.unlock()
		return
	}
	ctx.err = err
	if !ctx.done.closed {
		ctx.done <- 0
		ctx.done.close()
	}
	ctx.mutex.unlock()
}

fn (mut ctx AsyncContext) propagate_existing_parent_error() bool {
	mut parent := ctx.parent
	err := parent.err()
	if err !is none {
		ctx.cancel_with(err)
		return true
	}
	return false
}

fn watch_parent_context(mut ctx AsyncContext) {
	mut parent := ctx.parent
	parent_done := parent.done()
	local_done := ctx.done()
	select {
		_ := <-local_done {
			return
		}
		_ := <-parent_done {
			err := parent.err()
			if err !is none {
				ctx.cancel_with(err)
			}
		}
	}
}

fn watch_timeout_context(mut ctx AsyncContext, timeout time.Duration) {
	local_done := ctx.done()
	select {
		_ := <-local_done {
			return
		}
		timeout {
			ctx.cancel_with(error(context_deadline_exceeded))
		}
	}
}
