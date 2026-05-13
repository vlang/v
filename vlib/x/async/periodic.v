module async

import context
import time

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

fn run_periodic_iteration(mut ctx context.Context, f JobFn) ! {
	f(mut ctx)!
	err := ctx.err()
	if err !is none {
		return err
	}
}
