module veb

import time

// AfterCallback is the deferred work `Context.after` runs no sooner than the
// requested delay after the handler returns. It answers exactly like a
// handler: write one response (`ctx.text`, `ctx.json`, ...) and return.
// `arg` carries whatever the callback needs; only base-Context state travels
// with the timer, so pass an id, or a pointer cast, for app state.
pub type AfterCallback = fn (mut ctx Context, arg u64)

// after answers the request later without holding the worker: the handler
// returns at once, and no sooner than `ms` milliseconds on, `cb` runs and
// writes the single response. `ms <= 0` runs `cb` on the next timer turn.
// `cb` may call `after` again to re-arm the timer; otherwise the connection
// closes once `cb` returns.
//
// Boundaries, compared with a plain handler:
//   - `cb` runs on a dedicated timer thread, not the accept-loop worker, and
//     receives the base `Context`, not the app's wrapper type. The request is
//     still readable (nothing is cleared), but only base state is shared.
//   - The timer closes the connection after the response, and advertises
//     `Connection: close`, so do not pipeline another request behind a
//     deferred one. After-middleware is skipped for deferred responses: at
//     middleware time there is no response yet to frame.
//   - Without a live client transport (unit tests, `render_request`, the SSL
//     path, which has no takeover support) `after` answers inline instead:
//     the response and its timing stay correct, on the caller's thread.
pub fn (mut ctx Context) after(ms int, cb AfterCallback, arg u64) Result {
	if !ctx.has_live_transport() {
		// No client transport outlives the handler here: answer inline, so
		// the response and its timing stay correct on the caller's thread.
		if ms > 0 {
			time.sleep(time.Duration(ms) * time.millisecond)
		}
		cb(mut ctx, arg)
		return no_result()
	}
	// The timer below closes the connection after the single deferred
	// response, so advertise it: a client must not pipeline behind this one.
	ctx.res.header.set(.connection, 'close')
	ctx.takeover_conn()
	ctx.deferred = true
	ctx.deferred_ms = if ms < 0 { 0 } else { ms }
	ctx.deferred_cb = cb
	ctx.deferred_arg = arg
	return no_result()
}

// has_live_transport reports whether the context owns a client transport that
// survives the handler's return: a prepared `TcpConn`, or a real accepted
// socket fd. Unit tests, `render_request`, and the SSL path leave `client_fd`
// at -1 (the in-process test harnesses pass 0), so only a genuine accepted
// socket counts: fds 0-2 are stdio, never a client connection. A wrong answer
// here only costs performance, never correctness: the inline fallback answers
// with the same response and timing, on the caller's thread.
fn (ctx &Context) has_live_transport() bool {
	if ctx.conn != unsafe { nil } {
		return true
	}
	return ctx.client_fd > 2
}

// spawn_after_timer_if_armed starts the timer thread when `after` armed a
// deferred response. Call it once the handled context is preserved on the
// heap, so the timer owns that context instead of a stack-local copy.
fn (mut ctx Context) spawn_after_timer_if_armed() {
	if ctx.deferred {
		spawn after_timer(mut ctx, unsafe { nil })
	}
}

// after_timer owns the taken-over connection: it sleeps out each delay, runs
// the callback's single response, and closes the connection once the callback
// stops re-arming with `after`. The accept-loop worker has been free since
// `after` returned; only this short-lived thread waits.
fn after_timer(mut ctx Context, arena voidptr) {
	defer {
		if msg := recover() {
			eprintln('[veb] deferred response for "${ctx.req.url}" panicked: ${msg}')
		}
		if ctx.conn != unsafe { nil } {
			ctx.conn.close() or {}
		}
		// The append worker left this arena before starting the timer. Free it
		// after the callback and close hook finish using the request context.
		va_scope_free_after(arena)
	}
	for ctx.deferred {
		ms := ctx.deferred_ms
		cb := ctx.deferred_cb
		arg := ctx.deferred_arg
		ctx.deferred = false
		if ms > 0 {
			time.sleep(time.Duration(ms) * time.millisecond)
		}
		cb(mut ctx, arg)
	}
}
