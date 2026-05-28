module async

import context
import time

struct TimeoutResult {
	err IError = none
}

// with_timeout runs f with a background context and returns an error if timeout expires first.
pub fn with_timeout(timeout time.Duration, f JobFn) ! {
	with_timeout_context(context.background(), timeout, f)!
}

// with_timeout_context runs f with a context derived from parent and bounded by timeout.
//
// If timeout expires before f returns, this returns `async: timeout` and
// cancels the derived context. The job must observe the context to stop early;
// x.async does not kill spawned work.
pub fn with_timeout_context(parent context.Context, timeout time.Duration, f JobFn) ! {
	if f == unsafe { nil } {
		return error(err_nil_job)
	}
	async_ctx, cancel := new_timeout_context(parent, timeout)
	mut ctx := context.Context(async_ctx)
	defer {
		cancel()
	}
	initial_err := ctx.err()
	if initial_err !is none {
		if initial_err.msg() == context_deadline_exceeded && async_ctx.was_canceled_by_timeout() {
			return error(err_timeout)
		}
		return initial_err
	}
	// The channel is buffered so a non-cooperative job can still publish its
	// result later without blocking after the caller has returned on timeout.
	result_ch := chan TimeoutResult{cap: 1}
	spawn run_timeout_job(ctx, f, result_ch)
	done_ch := ctx.done()
	select {
		result := <-result_ch {
			if result.err !is none {
				ctx_err := ctx.err()
				if ctx_err !is none && ctx_err.msg() == context_deadline_exceeded
					&& result.err.msg() == context_deadline_exceeded {
					if async_ctx.was_canceled_by_timeout() {
						return error(err_timeout)
					}
				}
				return result.err
			}
			return
		}
		_ := <-done_ch {
			err := ctx.err()
			if err !is none {
				if err.msg() == context_deadline_exceeded && async_ctx.was_canceled_by_timeout() {
					return error(err_timeout)
				}
				return err
			}
			return error(err_timeout)
		}
	}
}

fn run_timeout_job(ctx context.Context, f JobFn, result_ch chan TimeoutResult) {
	// Spawned functions cannot receive mutable non-reference arguments, so the
	// worker creates the mutable context interface value locally.
	mut job_ctx := ctx
	f(mut job_ctx) or {
		result_ch <- TimeoutResult{
			err: err
		}
		return
	}
	result_ch <- TimeoutResult{}
}
