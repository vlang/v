import context
import time
import x.async as xasync

fn test_pool_rejects_invalid_config() {
	xasync.new_pool(workers: 0, queue_size: 1) or {
		assert err.msg() == 'async: pool worker count must be positive'
		return
	}
	assert false
}

fn test_pool_rejects_invalid_queue_size() {
	xasync.new_pool(workers: 1, queue_size: 0) or {
		assert err.msg() == 'async: pool queue size must be positive'
		return
	}
	assert false
}

fn test_pool_respects_worker_count() {
	mut pool := xasync.new_pool(workers: 2, queue_size: 4)!
	started := chan bool{cap: 4}
	release := chan bool{cap: 4}
	for _ in 0 .. 4 {
		pool.try_submit(fn [started, release] (mut ctx context.Context) ! {
			_ = ctx
			started <- true
			_ := <-release
		})!
	}

	wait_for_bool_signal(started, 'first pool job did not start')
	wait_for_bool_signal(started, 'second pool job did not start')
	select {
		_ := <-started {
			assert false, 'pool started more jobs than worker count before release'
		}
		100 * time.millisecond {}
	}

	for _ in 0 .. 4 {
		release <- true
	}
	pool.close()!
}

fn test_pool_queue_full_returns_backpressure_error() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 2}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	pool.try_submit(blocking_job)!
	wait_for_bool_signal(started, 'blocking pool job did not start')
	pool.try_submit(blocking_job)!
	pool.try_submit(blocking_job) or {
		assert err.msg() == 'async: pool queue is full'
		release <- true
		release <- true
		pool.close()!
		return
	}
	assert false
}

fn test_pool_submit_after_close_is_refused() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	pool.close()!
	pool.try_submit(fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: pool is closed'
		return
	}
	assert false
}

fn test_pool_wait_is_one_shot() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	pool.wait()!
	pool.wait() or {
		assert err.msg() == 'async: pool wait was already called'
		return
	}
	assert false
}

fn test_pool_refuses_nil_job() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	nil_job := unsafe { xasync.JobFn(nil) }
	pool.try_submit(nil_job) or {
		assert err.msg() == 'async: job function is nil'
		pool.close()!
		return
	}
	assert false
}

fn test_pool_close_waits_for_accepted_jobs() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	release := chan bool{cap: 1}
	closed := chan bool{cap: 1}
	pool.try_submit(fn [release] (mut ctx context.Context) ! {
		_ = ctx
		_ := <-release
	})!

	close_thread := spawn fn [mut pool, closed] () {
		pool.close() or {
			closed <- false
			return
		}
		closed <- true
	}()
	select {
		_ := <-closed {
			assert false, 'pool close returned before accepted job completed'
		}
		100 * time.millisecond {}
	}
	release <- true
	select {
		ok := <-closed {
			assert ok
		}
		1 * time.second {
			assert false, 'pool close did not return after accepted job completed'
		}
	}
	close_thread.wait()
}

fn test_pool_first_error_is_propagated() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 2)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 1}
	pool.try_submit(fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
		return error('first pool failure')
	})!
	wait_for_bool_signal(started, 'first pool failure job did not start')
	pool.try_submit(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('second pool failure')
	})!
	release <- true
	pool.close() or {
		assert err.msg() == 'first pool failure'
		return
	}
	assert false
}

fn test_pool_error_does_not_drop_accepted_jobs() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	accepted_job_ran := chan bool{cap: 1}
	pool.try_submit(fn (mut ctx context.Context) ! {
		_ = ctx
		return error('first pool failure')
	})!
	pool.try_submit(fn [accepted_job_ran] (mut ctx context.Context) ! {
		_ = ctx
		accepted_job_ran <- true
	})!
	pool.close() or {
		assert err.msg() == 'first pool failure'
		select {
			did_run := <-accepted_job_ran {
				assert did_run
			}
			1 * time.second {
				assert false, 'accepted pool job did not run after earlier error'
			}
		}
		return
	}
	assert false
}

fn test_pool_concurrent_errors_return_one_error_and_drain_accepted_jobs() {
	error_jobs := 4
	ok_jobs := 8
	mut pool := xasync.new_pool(workers: error_jobs, queue_size: ok_jobs)!
	started := chan bool{cap: error_jobs}
	release := chan bool{cap: error_jobs}
	completed_ok := chan bool{cap: ok_jobs}
	for i in 0 .. error_jobs {
		pool.try_submit(fn [started, release, i] (mut ctx context.Context) ! {
			_ = ctx
			started <- true
			_ := <-release
			return error('pool concurrent failure ${i}')
		})!
	}
	for _ in 0 .. ok_jobs {
		pool.try_submit(fn [completed_ok] (mut ctx context.Context) ! {
			_ = ctx
			completed_ok <- true
		})!
	}
	for _ in 0 .. error_jobs {
		wait_for_bool_signal(started, 'pool error job did not start')
	}
	for _ in 0 .. error_jobs {
		release <- true
	}
	pool.close() or {
		assert err.msg().starts_with('pool concurrent failure ')
		for _ in 0 .. ok_jobs {
			wait_for_bool_signal(completed_ok, 'accepted ok pool job did not complete')
		}
		return
	}
	assert false
}

fn test_pool_close_drains_many_accepted_jobs_while_finishing() {
	jobs := 12
	workers := 3
	mut pool := xasync.new_pool(workers: workers, queue_size: jobs - workers)!
	started := chan bool{cap: jobs}
	release := chan bool{cap: jobs}
	finished := chan bool{cap: jobs}
	closed := chan bool{cap: 1}
	for _ in 0 .. jobs {
		pool.try_submit(fn [started, release, finished] (mut ctx context.Context) ! {
			_ = ctx
			started <- true
			_ := <-release
			finished <- true
		})!
	}
	for _ in 0 .. workers {
		wait_for_bool_signal(started, 'initial pool job did not start')
	}
	close_thread := spawn fn [mut pool, closed] () {
		pool.close() or {
			closed <- false
			return
		}
		closed <- true
	}()
	wait_until_pool_rejects_as_closed(mut pool)
	assert_no_bool_signal(closed, 'pool close returned while accepted jobs were still blocked')

	for _ in 0 .. jobs - 1 {
		release <- true
	}
	for _ in 0 .. jobs - 1 {
		wait_for_bool_signal(finished, 'accepted pool job did not finish')
	}
	assert_no_bool_signal(closed, 'pool close returned before the last accepted job finished')

	release <- true
	wait_for_bool_signal(finished, 'last accepted pool job did not finish')
	wait_for_bool_signal(closed, 'pool close did not drain accepted jobs')
	close_thread.wait()
}

fn test_pool_parent_cancellation_is_observed_by_cooperative_job() {
	parent_ctx, cancel := xasync.with_cancel()
	mut pool := xasync.new_pool_with_context(parent_ctx, workers: 1, queue_size: 1)!
	started := chan bool{cap: 1}
	pool.try_submit(fn [started] (mut ctx context.Context) ! {
		started <- true
		done := ctx.done()
		select {
			_ := <-done {
				return ctx.err()
			}
			1 * time.second {
				return error('pool job did not observe parent cancellation')
			}
		}
		return error('unreachable')
	})!
	wait_for_bool_signal(started, 'pool job did not start before parent cancellation')
	cancel()
	pool.close() or {
		assert err.msg() == 'context canceled'
		return
	}
	assert false
}

fn test_pool_non_cooperative_job_finishes_naturally_after_parent_cancel() {
	parent_ctx, cancel := xasync.with_cancel()
	mut pool := xasync.new_pool_with_context(parent_ctx, workers: 1, queue_size: 1)!
	finished := chan bool{cap: 1}
	pool.try_submit(fn [finished] (mut ctx context.Context) ! {
		_ = ctx
		time.sleep(20 * time.millisecond)
		finished <- true
	})!
	cancel()
	pool.close()!
	assert <-finished
}

fn test_pool_short_stress_many_jobs() {
	jobs := 100
	mut pool := xasync.new_pool(workers: 4, queue_size: jobs)!
	done := chan int{cap: jobs}
	for i in 0 .. jobs {
		pool.try_submit(fn [done, i] (mut ctx context.Context) ! {
			_ = ctx
			done <- i
		})!
	}
	pool.close()!

	mut seen := []bool{len: jobs}
	for _ in 0 .. jobs {
		i := <-done
		assert i >= 0
		assert i < jobs
		assert !seen[i]
		seen[i] = true
	}
	for was_seen in seen {
		assert was_seen
	}
}

fn wait_for_bool_signal(signal chan bool, message string) {
	select {
		ok := <-signal {
			assert ok
		}
		1 * time.second {
			assert false, message
		}
	}
}

fn assert_no_bool_signal(signal chan bool, message string) {
	select {
		_ := <-signal {
			assert false, message
		}
		else {}
	}
}

fn wait_until_pool_rejects_as_closed(mut pool xasync.Pool) {
	probe := fn (mut ctx context.Context) ! {
		_ = ctx
	}
	for _ in 0 .. 100 {
		pool.try_submit(probe) or {
			if err.msg() == 'async: pool is closed' {
				return
			}
			assert err.msg() == 'async: pool queue is full'
			time.sleep(1 * time.millisecond)
			continue
		}
		assert false, 'pool accepted probe while backlog should be full'
	}
	assert false, 'pool close did not start'
}
