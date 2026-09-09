module async

import context
import sync
import time

enum PeriodicResultKind {
	completed
	job_error
	context_error
}

struct PeriodicResult {
	kind PeriodicResultKind
	err  IError = none
}

// PeriodicHandle owns one detached periodic loop started by start_every().
//
// The handle exposes only explicit lifecycle operations: stop() requests
// cooperative shutdown, and wait() consumes the loop result once.
@[heap]
pub struct PeriodicHandle {
mut:
	parent           context.Context
	ctx              context.Context
	cancel           context.CancelFn = unsafe { nil }
	result_ch        chan PeriodicResult
	mutex            &sync.Mutex = sync.new_mutex()
	stopped          bool
	stop_owns_cancel bool
	waited           bool
}

// every runs f repeatedly with interval spacing until ctx is canceled or f fails.
//
// This helper is intentionally blocking: it does not start a hidden scheduler or
// background loop. The first iteration runs after one interval. Iterations never
// overlap; a slow job delays the next interval. Cancellation is cooperative, so
// a running job must observe `ctx.done()` if it needs to stop before returning.
pub fn every(parent context.Context, interval time.Duration, f JobFn) ! {
	if interval.nanoseconds() <= 0 {
		return error(err_interval_invalid)
	}
	if f == unsafe { nil } {
		return error(err_nil_job)
	}

	mut ctx := parent
	initial_err := ctx.err()
	if initial_err !is none {
		return initial_err
	}

	done_ch := ctx.done()
	mut watch_done := true
	// context.background().done() is closed in V but err() remains none. Treat
	// that as a non-cancelable context instead of returning immediately.
	select {
		_ := <-done_ch {
			err := ctx.err()
			if err !is none {
				return err
			}
			watch_done = false
		}
		else {}
	}

	for {
		if watch_done {
			select {
				_ := <-done_ch {
					err := ctx.err()
					if err !is none {
						return err
					}
					watch_done = false
				}
				interval {
					run_periodic_iteration(mut ctx, f)!
				}
			}
		} else {
			time.sleep(interval)
			run_periodic_iteration(mut ctx, f)!
		}
	}
}

// start_every starts f in one detached periodic loop and returns its lifecycle handle.
//
// The first iteration runs after one interval. Iterations never overlap; a slow
// job delays the next interval. The returned handle must be stopped and waited
// on by its owner, otherwise the detached loop can keep running.
pub fn start_every(parent context.Context, interval time.Duration, f JobFn) !&PeriodicHandle {
	if interval.nanoseconds() <= 0 {
		return error(err_interval_invalid)
	}
	if f == unsafe { nil } {
		return error(err_nil_job)
	}

	mut parent_ctx := parent
	initial_err := parent_ctx.err()
	if initial_err !is none {
		return initial_err
	}

	ctx, cancel := new_cancel_context(parent)
	mut handle := &PeriodicHandle{
		parent:    parent
		ctx:       context.Context(ctx)
		cancel:    cancel
		result_ch: chan PeriodicResult{cap: 1}
		mutex:     sync.new_mutex()
	}
	spawn run_periodic_handle(mut handle, interval, f)
	return handle
}

// stop requests cooperative shutdown of the detached periodic loop.
//
// stop is idempotent and non-blocking. A running job still has to return
// naturally; wait() is responsible for observing the final result.
pub fn (mut h PeriodicHandle) stop() {
	mut parent := h.parent
	parent_err := parent.err()
	h.mutex.lock()
	if h.stopped {
		h.mutex.unlock()
		return
	}
	h.stopped = true
	h.stop_owns_cancel = parent_err is none
	should_cancel := h.stop_owns_cancel
	cancel := h.cancel
	h.mutex.unlock()
	if should_cancel {
		cancel()
	}
}

// wait blocks until the detached periodic loop exits.
//
// wait is one-shot. It returns job errors and parent cancellation errors. A
// normal stop() request is reported as successful completion.
pub fn (mut h PeriodicHandle) wait() ! {
	h.mutex.lock()
	if h.waited {
		h.mutex.unlock()
		return error(err_periodic_wait_called)
	}
	h.waited = true
	h.mutex.unlock()

	result := <-h.result_ch
	if result.err !is none {
		if result.kind == .context_error && h.is_stop_result() {
			return
		}
		return result.err
	}
}

fn run_periodic_iteration(mut ctx context.Context, f JobFn) ! {
	f(mut ctx)!
	err := ctx.err()
	if err !is none {
		return err
	}
}

fn run_periodic_handle(mut h PeriodicHandle, interval time.Duration, f JobFn) {
	mut ctx := h.ctx
	result := run_detached_periodic_loop(mut ctx, interval, f)
	h.cancel()
	h.result_ch <- result
}

fn run_detached_periodic_loop(mut ctx context.Context, interval time.Duration, f JobFn) PeriodicResult {
	done_ch := ctx.done()
	mut watch_done := true
	select {
		_ := <-done_ch {
			err := ctx.err()
			if err !is none {
				return PeriodicResult{
					kind: .context_error
					err:  err
				}
			}
			watch_done = false
		}
		else {}
	}

	for {
		if watch_done {
			select {
				_ := <-done_ch {
					err := ctx.err()
					if err !is none {
						return PeriodicResult{
							kind: .context_error
							err:  err
						}
					}
					watch_done = false
				}
				interval {
					result := run_detached_periodic_iteration(mut ctx, f)
					if result.err !is none {
						return result
					}
				}
			}
		} else {
			time.sleep(interval)
			result := run_detached_periodic_iteration(mut ctx, f)
			if result.err !is none {
				return result
			}
		}
	}
	return PeriodicResult{}
}

fn run_detached_periodic_iteration(mut ctx context.Context, f JobFn) PeriodicResult {
	f(mut ctx) or { return PeriodicResult{
		kind: .job_error
		err:  err
	} }
	err := ctx.err()
	if err !is none {
		return PeriodicResult{
			kind: .context_error
			err:  err
		}
	}
	return PeriodicResult{}
}

fn (mut h PeriodicHandle) is_stop_result() bool {
	h.mutex.lock()
	stop_owns_cancel := h.stop_owns_cancel
	h.mutex.unlock()
	return stop_owns_cancel
}
