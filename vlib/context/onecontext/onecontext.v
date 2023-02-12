module onecontext

import context
import sync
import time

// canceled is the error returned when the cancel function is called on a merged context
pub const canceled = error('canceled context')

struct OneContext {
mut:
	ctx        context.Context
	ctxs       []context.Context
	done       chan int
	err        IError = none
	err_mutex  sync.Mutex
	cancel_fn  context.CancelFn
	cancel_ctx context.Context
}

// merge allows to merge multiple contexts
// it returns the merged context
pub fn merge(ctx context.Context, ctxs ...context.Context) (context.Context, context.CancelFn) {
	mut background := context.background()
	cancel_ctx, cancel := context.with_cancel(mut &background)
	mut octx := &OneContext{
		done: chan int{cap: 3}
		ctx: ctx
		ctxs: ctxs
		cancel_fn: cancel
		cancel_ctx: cancel_ctx
	}
	spawn octx.run()
	return context.Context(octx), context.CancelFn(cancel)
}

pub fn (octx OneContext) deadline() ?time.Time {
	mut min := time.Time{}

	if deadline := octx.ctx.deadline() {
		min = deadline
	}

	for ctx in octx.ctxs {
		if deadline := ctx.deadline() {
			if min.unix_time() == 0 || deadline < min {
				min = deadline
			}
		}
	}

	if min.unix_time() == 0 {
		return none
	}

	return min
}

pub fn (octx OneContext) done() chan int {
	return octx.done
}

pub fn (mut octx OneContext) err() IError {
	octx.err_mutex.@lock()
	defer {
		octx.err_mutex.unlock()
	}
	return octx.err
}

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

pub fn (mut octx OneContext) run() {
	mut wrapped_ctx := &octx.ctx
	if octx.ctxs.len == 1 {
		mut first_ctx := &octx.ctxs[0]
		octx.run_two_contexts(mut wrapped_ctx, mut first_ctx)
		return
	}

	octx.run_multiple_contexts(mut wrapped_ctx)
	for mut ctx in octx.ctxs {
		octx.run_multiple_contexts(mut &ctx)
	}
}

pub fn (octx OneContext) str() string {
	return ''
}

pub fn (mut octx OneContext) cancel(err IError) {
	octx.cancel_fn()
	octx.err_mutex.@lock()
	octx.err = err
	octx.err_mutex.unlock()
	if !octx.done.closed {
		octx.done <- 0
		octx.done.close()
	}
}

pub fn (mut octx OneContext) run_two_contexts(mut ctx1 context.Context, mut ctx2 context.Context) {
	spawn fn (mut octx OneContext, mut ctx1 context.Context, mut ctx2 context.Context) {
		octx_cancel_done := octx.cancel_ctx.done()
		c1done := ctx1.done()
		c2done := ctx2.done()
		select {
			_ := <-octx_cancel_done {
				octx.cancel(onecontext.canceled)
			}
			_ := <-c1done {
				octx.cancel(ctx1.err())
			}
			_ := <-c2done {
				octx.cancel(ctx1.err())
			}
		}
	}(mut &octx, mut &ctx1, mut &ctx2)
}

pub fn (mut octx OneContext) run_multiple_contexts(mut ctx context.Context) {
	spawn fn (mut octx OneContext, mut ctx context.Context) {
		octx_cancel_done := octx.cancel_ctx.done()
		cdone := ctx.done()
		select {
			_ := <-octx_cancel_done {
				octx.cancel(onecontext.canceled)
			}
			_ := <-cdone {
				octx.cancel(ctx.err())
			}
		}
	}(mut &octx, mut &ctx)
}
