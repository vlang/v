module async

import context

// JobFn is the function signature run by Group and timeout helpers.
//
// The passed context is canceled when the parent context is canceled, when a
// group task fails, or when a timeout expires. Cancellation is cooperative:
// jobs should observe `ctx.done()` and return.
pub type JobFn = fn (mut context.Context) !

// TaskFn is the function signature run by Task.
//
// It mirrors JobFn but returns a value. Like all x.async work, the function
// receives a context and must observe `ctx.done()` for cooperative cancellation.
pub type TaskFn[T] = fn (mut context.Context) !T

// background returns a root context for async helpers.
//
// It is intentionally just a thin wrapper around `context.background()` so code
// can start with `async.background()` and later pass the same context to the
// standard `context` APIs without conversion.
pub fn background() context.Context {
	return context.background()
}

// with_cancel returns a cancellable context derived from background.
//
// Call the returned cancel function when the surrounding operation is done, even
// when all jobs completed successfully. That mirrors `context.with_cancel()` and
// releases parent/child cancellation references promptly.
pub fn with_cancel() (context.Context, context.CancelFn) {
	ctx, cancel := new_cancel_context(context.background())
	return context.Context(ctx), cancel
}
