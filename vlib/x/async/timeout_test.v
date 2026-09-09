import context
import sync
import time
import x.async as xasync

@[heap]
struct ControlledDeadlineParent {
	deadline_at time.Time
mut:
	done_ch   chan int
	mutex     &sync.Mutex = sync.new_mutex()
	err_value IError      = none
}

fn new_controlled_deadline_parent(deadline_at time.Time) &ControlledDeadlineParent {
	return &ControlledDeadlineParent{
		deadline_at: deadline_at
		done_ch:     chan int{}
		mutex:       sync.new_mutex()
	}
}

fn (ctx &ControlledDeadlineParent) deadline() ?time.Time {
	return ctx.deadline_at
}

fn (ctx &ControlledDeadlineParent) value(key context.Key) ?context.Any {
	_ = key
	return none
}

fn (mut ctx ControlledDeadlineParent) done() chan int {
	ctx.mutex.lock()
	done := ctx.done_ch
	ctx.mutex.unlock()
	return done
}

fn (mut ctx ControlledDeadlineParent) err() IError {
	ctx.mutex.lock()
	err := ctx.err_value
	ctx.mutex.unlock()
	return err
}

fn (mut ctx ControlledDeadlineParent) close_with_error(err IError) {
	ctx.mutex.lock()
	ctx.err_value = err
	if !ctx.done_ch.closed {
		ctx.done_ch.close()
	}
	ctx.mutex.unlock()
}

fn context_error_or_closed_without_error(mut ctx context.Context) ! {
	err := ctx.err()
	if err !is none {
		return err
	}
	return error('context closed without error')
}

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
				return context_error_or_closed_without_error(mut ctx)
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
				return context_error_or_closed_without_error(mut ctx)
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
				return context_error_or_closed_without_error(mut ctx)
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
				return context_error_or_closed_without_error(mut ctx)
			}
			1 * time.second {}
		}
	}) or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_with_timeout_context_preserves_already_expired_parent_deadline() {
	mut background := context.background()
	parent_ctx, parent_cancel := context.with_timeout(mut background, -1 * time.millisecond)
	defer {
		parent_cancel()
	}

	xasync.with_timeout_context(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
		return error('job should not run with an already expired parent')
	}) or {
		assert err.msg() == 'context deadline exceeded'
		return
	}
	assert false
}

fn test_with_timeout_context_preserves_parent_deadline_that_expires_first() {
	mut background := context.background()
	parent_ctx, parent_cancel := context.with_timeout(mut background, 10 * time.millisecond)
	defer {
		parent_cancel()
	}

	xasync.with_timeout_context(parent_ctx, 1 * time.second, fn (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(100 * time.millisecond)
	}) or {
		assert err.msg() == 'context deadline exceeded'
		return
	}
	assert false
}

fn test_with_timeout_context_waits_for_controlled_parent_done_and_returns_exact_parent_error() {
	mut parent_ctx := new_controlled_deadline_parent(time.now().add(-1 * time.second))
	started := chan bool{cap: 1}
	release := chan bool{cap: 1}
	result := chan string{cap: 1}

	caller := spawn fn [mut parent_ctx, started, release, result] () {
		xasync.with_timeout_context(context.Context(parent_ctx), 1 * time.second, fn [started, release] (mut ctx context.Context) ! {
			_ = ctx
			started <- true
			_ := <-release
		}) or {
			result <- err.msg()
			return
		}
		result <- 'ok'
	}()

	select {
		did_start := <-started {
			assert did_start
		}
		1 * time.second {
			assert false, 'with_timeout_context returned before starting the job for a pending parent'
		}
	}
	select {
		msg := <-result {
			assert false, 'with_timeout_context returned before parent done closed: ${msg}'
		}
		else {}
	}

	parent_ctx.close_with_error(error('controlled parent deadline'))
	select {
		msg := <-result {
			assert msg == 'controlled parent deadline'
		}
		1 * time.second {
			assert false, 'with_timeout_context did not return after controlled parent closed'
		}
	}
	release <- true
	caller.wait()
}

fn test_with_timeout_context_uses_own_timeout_before_parent_deadline() {
	mut background := context.background()
	parent_ctx, parent_cancel := context.with_timeout(mut background, 1 * time.second)
	defer {
		parent_cancel()
	}

	xasync.with_timeout_context(parent_ctx, 10 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(100 * time.millisecond)
	}) or {
		assert err.msg() == 'async: timeout'
		return
	}
	assert false
}
