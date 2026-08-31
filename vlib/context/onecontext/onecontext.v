module onecontext

import context
import sync
import time

// canceled is the error returned when the cancel function is called on a merged context
pub const canceled = error('canceled context')

@[heap]
struct OneContext {
mut:
	ctx        context.Context
	ctxs       []context.Context
	done       chan int
	err        IError           = none
	err_mutex  &sync.Mutex      = sync.new_mutex()
	cancel_fn  context.CancelFn = unsafe { nil }
	cancel_ctx context.Context
}

// merge allows to merge multiple contexts
// it returns the merged context
pub fn merge(ctx context.Context, ctxs ...context.Context) (context.Context, context.CancelFn) {
	mut background := context.background()
	cancel_ctx, cancel := context.with_cancel(mut background)
	mut octx := &OneContext{
		done:       chan int{cap: 3}
		ctx:        ctx
		ctxs:       ctxs
		cancel_fn:  cancel
		cancel_ctx: cancel_ctx
	}
	spawn octx.run()
	return context.Context(octx), context.CancelFn(cancel)
}

// deadline returns the earliest deadline among all merged contexts,
// or none if no context has a deadline set.
pub fn (octx OneContext) deadline() ?time.Time {
	mut min := time.Time{}
	mut found := false

	if deadline := octx.ctx.deadline() {
		min = deadline
		found = true
	}

	for ctx in octx.ctxs {
		if deadline := ctx.deadline() {
			if !found || deadline < min {
				min = deadline
			}
			found = true
		}
	}

	if !found {
		return none
	}

	return min
}

// done returns the done channel, which is closed when the merged context is canceled.
pub fn (octx OneContext) done() chan int {
	return octx.done
}

// err returns the error from the merged context, or `none` if not yet canceled.
pub fn (mut octx OneContext) err() IError {
	octx.err_mutex.lock()
	defer {
		octx.err_mutex.unlock()
	}
	return octx.err
}

// value looks up a value by key across all merged contexts, returning
// the first match found or none if no context holds the key.
pub fn (octx OneContext) value(key context.Key) ?context.Any {
	if value := octx.ctx.value(key) {
		return value
	}

	for ctx in octx.ctxs {
		if value := ctx.value(key) {
			return value
		}
	}

	return none
}

// run starts listening for cancellation signals from all merged contexts.
pub fn (mut octx OneContext) run() {
	if octx.ctxs.len == 1 {
		octx.run_own_two_contexts()
		return
	}

	octx.run_own_context()
	for i in 0 .. octx.ctxs.len {
		octx.run_own_context_at(i)
	}
}

// str returns a string representation of the OneContext.
pub fn (octx OneContext) str() string {
	return ''
}

// cancel cancels the merged context with the given error, closing
// the done channel and propagating cancellation to the underlying context.
pub fn (mut octx OneContext) cancel(err IError) {
	octx.cancel_fn()
	octx.err_mutex.lock()
	if octx.err !is none {
		octx.err_mutex.unlock()
		return
	}
	octx.err = err
	should_close_done := !octx.done.closed
	octx.err_mutex.unlock()
	if should_close_done {
		octx.done <- 0
		octx.done.close()
	}
}

fn (mut octx OneContext) run_own_two_contexts() {
	octx_cancel_done := octx.cancel_ctx.done()
	c1done := octx.ctx.done()
	c2done := octx.ctxs[0].done()
	spawn fn (mut octx OneContext, octx_cancel_done chan int, c1done chan int, c2done chan int) {
		select {
			_ := <-octx_cancel_done {
				octx.cancel(canceled)
			}
			_ := <-c1done {
				octx.cancel(octx.ctx.err())
			}
			_ := <-c2done {
				octx.cancel(octx.ctxs[0].err())
			}
		}
	}(mut &octx, octx_cancel_done, c1done, c2done)
}

fn (mut octx OneContext) run_own_context() {
	octx_cancel_done := octx.cancel_ctx.done()
	cdone := octx.ctx.done()
	spawn fn (mut octx OneContext, octx_cancel_done chan int, cdone chan int) {
		select {
			_ := <-octx_cancel_done {
				octx.cancel(canceled)
			}
			_ := <-cdone {
				octx.cancel(octx.ctx.err())
			}
		}
	}(mut &octx, octx_cancel_done, cdone)
}

fn (mut octx OneContext) run_own_context_at(idx int) {
	octx_cancel_done := octx.cancel_ctx.done()
	cdone := octx.ctxs[idx].done()
	spawn fn (mut octx OneContext, octx_cancel_done chan int, cdone chan int, idx int) {
		select {
			_ := <-octx_cancel_done {
				octx.cancel(canceled)
			}
			_ := <-cdone {
				octx.cancel(octx.ctxs[idx].err())
			}
		}
	}(mut &octx, octx_cancel_done, cdone, idx)
}

// run_two_contexts spawns a listener that cancels the merged context
// when either of the two given contexts is done.
pub fn (mut octx OneContext) run_two_contexts(mut ctx1 context.Context, mut ctx2 context.Context) {
	octx_cancel_done := octx.cancel_ctx.done()
	c1done := ctx1.done()
	c2done := ctx2.done()
	spawn fn (mut octx OneContext, octx_cancel_done chan int, c1done chan int, c2done chan int, mut ctx1 context.Context, mut ctx2 context.Context) {
		select {
			_ := <-octx_cancel_done {
				octx.cancel(canceled)
			}
			_ := <-c1done {
				octx.cancel(ctx1.err())
			}
			_ := <-c2done {
				octx.cancel(ctx2.err())
			}
		}
	}(mut &octx, octx_cancel_done, c1done, c2done, mut ctx1, mut ctx2)
}

// run_multiple_contexts spawns a listener that cancels the merged context
// when the given context is done.
pub fn (mut octx OneContext) run_multiple_contexts(mut ctx context.Context) {
	octx_cancel_done := octx.cancel_ctx.done()
	cdone := ctx.done()
	spawn fn (mut octx OneContext, octx_cancel_done chan int, cdone chan int, mut ctx context.Context) {
		select {
			_ := <-octx_cancel_done {
				octx.cancel(canceled)
			}
			_ := <-cdone {
				octx.cancel(ctx.err())
			}
		}
	}(mut &octx, octx_cancel_done, cdone, mut ctx)
}
