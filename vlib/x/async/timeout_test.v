import context
import time
import x.async as xasync

fn test_with_timeout_returns_timeout_error() {
	xasync.with_timeout(5 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(50 * time.millisecond)
	}) or {
		assert err.msg() == 'async: timeout'
		return
	}
	assert false
}

fn test_with_timeout_returns_without_waiting_for_ignored_cancellation() {
	stopwatch := time.new_stopwatch()
	xasync.with_timeout(10 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(250 * time.millisecond)
	}) or {
		assert err.msg() == 'async: timeout'
		assert stopwatch.elapsed() < 150 * time.millisecond
		return
	}
	assert false
}

fn test_with_timeout_zero_duration_expires_immediately() {
	xasync.with_timeout(0 * time.millisecond, fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return
			}
		}
	}) or {
		assert err.msg() == 'async: timeout'
		return
	}
	assert false
}

fn test_with_timeout_negative_duration_is_already_expired() {
	xasync.with_timeout(-1 * time.millisecond, fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return
			}
		}
	}) or {
		assert err.msg() == 'async: timeout'
		return
	}
	assert false
}

fn test_with_timeout_success_before_timeout() {
	seen := chan int{cap: 1}
	xasync.with_timeout(1 * time.second, fn [seen] (mut ctx context.Context) ! {
		_ = ctx
		seen <- 7
	})!
	assert <-seen == 7
}

fn test_with_timeout_returns_job_error() {
	xasync.with_timeout(1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
		return error('job failed')
	}) or {
		assert err.msg() == 'job failed'
		return
	}
	assert false
}

fn test_with_timeout_preserves_job_error_matching_context_message_before_timeout() {
	xasync.with_timeout(1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
		return error('context deadline exceeded')
	}) or {
		assert err.msg() == 'context deadline exceeded'
		return
	}
	assert false
}

fn test_with_timeout_refuses_nil_job() {
	nil_job := unsafe { xasync.JobFn(nil) }
	xasync.with_timeout(1 * time.second, nil_job) or {
		assert err.msg() == 'async: job function is nil'
		return
	}
	assert false
}

fn test_with_timeout_returns_timeout_for_cooperative_job() {
	mut timed_out := false
	xasync.with_timeout(50 * time.millisecond, fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('cooperative timeout job was not canceled')
			}
		}
	}) or {
		assert err.msg() == 'async: timeout'
		timed_out = true
	}
	assert timed_out
}

fn test_with_timeout_context_uses_parent_context() {
	mut parent_ctx, cancel := xasync.with_cancel()
	cancel()
	xasync.with_timeout_context(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {}
		}
	}) or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}
