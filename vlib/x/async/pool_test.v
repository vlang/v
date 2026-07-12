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

fn test_pool_submit_with_context_waits_until_capacity_is_available() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 2}
	release := chan bool{cap: 2}
	attempting := chan bool{cap: 1}
	accepted := chan bool{cap: 1}
	ran := chan bool{cap: 1}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	pool.try_submit(blocking_job)!
	wait_for_bool_signal(started, 'first blocking pool job did not start')
	pool.try_submit(blocking_job)!

	submit_thread := spawn fn [mut pool, attempting, accepted, ran] () {
		attempting <- true
		pool.submit_with_context(context.background(), fn [ran] (mut ctx context.Context) ! {
			_ = ctx
			ran <- true
		}) or {
			accepted <- false
			return
		}
		accepted <- true
	}()
	wait_for_bool_signal(attempting, 'bounded submit thread did not start')
	select {
		_ := <-accepted {
			assert false, 'bounded submit returned while pool was full'
		}
		50 * time.millisecond {}
	}

	release <- true
	wait_for_bool_signal(accepted, 'bounded submit did not accept after capacity opened')
	release <- true
	wait_for_bool_signal(ran, 'accepted bounded submit job did not run')
	pool.close()!
	submit_thread.wait()
}

fn test_pool_submit_with_timeout_returns_timeout_without_leaking_acceptance() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	first_started := chan bool{cap: 1}
	first_release := chan bool{cap: 1}
	first_finished := chan bool{cap: 1}
	queued_started := chan bool{cap: 1}
	queued_release := chan bool{cap: 1}
	queued_finished := chan bool{cap: 1}
	first_blocking_job := fn [first_started, first_release, first_finished] (mut ctx context.Context) ! {
		_ = ctx
		first_started <- true
		_ := <-first_release
		first_finished <- true
	}
	queued_blocking_job := fn [queued_started, queued_release, queued_finished] (mut ctx context.Context) ! {
		_ = ctx
		queued_started <- true
		_ := <-queued_release
		queued_finished <- true
	}

	pool.try_submit(first_blocking_job)!
	wait_for_bool_signal(first_started, 'first blocking pool job did not start')
	pool.try_submit(queued_blocking_job)!

	pool.submit_with_timeout(20 * time.millisecond, fn (mut ctx context.Context) ! {
		_ = ctx
	}) or {
		assert err.msg() == 'async: timeout'
		first_release <- true
		wait_for_bool_signal(first_finished, 'first blocking pool job did not finish')
		wait_for_bool_signal(queued_started,
			'queued blocking pool job did not start after capacity opened')
		pool.submit_with_timeout(pool_test_signal_timeout(), fn (mut ctx context.Context) ! {
			_ = ctx
		})!
		queued_release <- true
		wait_for_bool_signal(queued_finished, 'queued blocking pool job did not finish')
		pool.close()!
		return
	}
	assert false
}

fn test_pool_submit_with_context_parent_cancel_does_not_accept_job() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 2}
	attempting := chan bool{cap: 1}
	would_run := chan bool{cap: 1}
	result := chan string{cap: 1}
	parent_ctx, cancel := xasync.with_cancel()
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	pool.try_submit(blocking_job)!
	wait_for_bool_signal(started, 'blocking pool job did not start')
	pool.try_submit(blocking_job)!

	submit_thread := spawn fn [mut pool, parent_ctx, attempting, would_run, result] () {
		attempting <- true
		pool.submit_with_context(parent_ctx, fn [would_run] (mut ctx context.Context) ! {
			_ = ctx
			would_run <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	wait_for_bool_signal(attempting, 'bounded submit thread did not start')
	select {
		msg := <-result {
			assert false, 'bounded submit returned before parent cancellation: ${msg}'
		}
		50 * time.millisecond {}
	}

	cancel()
	select {
		msg := <-result {
			assert msg == 'context canceled'
		}
		1 * time.second {
			assert false, 'bounded submit did not return after parent cancellation'
		}
	}
	release <- true
	release <- true
	pool.close()!
	assert_no_bool_signal(would_run, 'job was accepted after parent cancellation')
	submit_thread.wait()
}

fn test_pool_close_wakes_waiting_submitter_without_accepting_job() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	started := chan bool{cap: 1}
	release := chan bool{cap: 2}
	attempting := chan bool{cap: 1}
	would_run := chan bool{cap: 1}
	result := chan string{cap: 1}
	closed := chan bool{cap: 1}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	pool.try_submit(blocking_job)!
	wait_for_bool_signal(started, 'blocking pool job did not start')
	pool.try_submit(blocking_job)!

	submit_thread := spawn fn [mut pool, attempting, would_run, result] () {
		attempting <- true
		pool.submit_with_context(context.background(), fn [would_run] (mut ctx context.Context) ! {
			_ = ctx
			would_run <- true
		}) or {
			result <- err.msg()
			return
		}
		result <- 'accepted'
	}()
	wait_for_bool_signal(attempting, 'bounded submit thread did not start')
	select {
		msg := <-result {
			assert false, 'bounded submit returned before close: ${msg}'
		}
		50 * time.millisecond {}
	}

	close_thread := spawn fn [mut pool, closed] () {
		pool.close() or {
			closed <- false
			return
		}
		closed <- true
	}()
	select {
		msg := <-result {
			assert msg == 'async: pool is closed'
		}
		1 * time.second {
			assert false, 'bounded submit did not return after pool close started'
		}
	}
	release <- true
	release <- true
	wait_for_bool_signal(closed, 'pool close did not finish after accepted jobs were released')
	assert_no_bool_signal(would_run, 'job was accepted after pool close started')
	submit_thread.wait()
	close_thread.wait()
}

fn test_pool_bounded_submit_rejects_nil_job() {
	mut pool := xasync.new_pool(workers: 1, queue_size: 1)!
	nil_job := unsafe { xasync.JobFn(nil) }
	pool.submit_with_context(context.background(), nil_job) or {
		assert err.msg() == 'async: job function is nil'
		pool.submit_with_timeout(20 * time.millisecond, nil_job) or {
			assert err.msg() == 'async: job function is nil'
			pool.close()!
			return
		}
		assert false
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
	mut jobs := 12
	mut workers := 3
	$if windows && (tinyc || gcc) {
		jobs = 6
		workers = 2
	}
	mut pool := xasync.new_pool(workers: workers, queue_size: jobs - workers)!
	started := chan int{cap: jobs}
	finished := chan int{cap: jobs}
	mut releases := []chan bool{cap: jobs}
	for _ in 0 .. jobs {
		releases << chan bool{cap: 1}
	}
	closed := chan bool{cap: 1}
	for i in 0 .. jobs {
		release := releases[i]
		pool.try_submit(fn [started, release, finished, i] (mut ctx context.Context) ! {
			_ = ctx
			started <- i
			_ := <-release
			finished <- i
		})!
	}
	mut active_jobs := []int{cap: workers}
	for _ in 0 .. workers {
		active_jobs << wait_for_int_signal(started, 'initial pool job did not start')
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

	// Drain full worker waves deterministically. Each release opens exactly one
	// worker slot, so wait for the replacement job to start before the next wave.
	for _ in 0 .. (jobs / workers) - 1 {
		for job_id in active_jobs {
			releases[job_id] <- true
		}
		for _ in 0 .. workers {
			wait_for_int_signal(finished, 'accepted pool job did not finish')
		}
		active_jobs = []int{cap: workers}
		for _ in 0 .. workers {
			active_jobs << wait_for_int_signal(started, 'next pool worker wave did not start')
		}
	}
	assert_no_bool_signal(closed, 'pool close returned before the last accepted job finished')

	for job_id in active_jobs {
		releases[job_id] <- true
	}
	for _ in 0 .. workers {
		wait_for_int_signal(finished, 'last accepted pool job did not finish')
	}
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

fn test_pool_bounded_submit_stress_waiting_submitters_are_released() {
	workers := 2
	queue_size := 2
	initial_jobs := workers + queue_size
	mut submitters := 8
	$if windows {
		submitters = initial_jobs
	}
	mut pool := xasync.new_pool(workers: workers, queue_size: queue_size)!
	started := chan bool{cap: initial_jobs}
	release := chan bool{cap: initial_jobs}
	attempting := chan bool{cap: submitters}
	accepted := chan int{cap: submitters}
	ran := chan int{cap: submitters}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	for _ in 0 .. initial_jobs {
		pool.try_submit(blocking_job)!
	}
	for _ in 0 .. workers {
		wait_for_bool_signal(started, 'initial pool worker did not start')
	}

	mut submit_threads := []thread{}
	for i in 0 .. submitters {
		submit_threads << spawn fn [mut pool, attempting, accepted, ran, i] () {
			attempting <- true
			pool.submit_with_context(context.background(), fn [ran, i] (mut ctx context.Context) ! {
				_ = ctx
				ran <- i
			}) or {
				failed_index := -1 - i
				accepted <- failed_index
				return
			}
			accepted <- i
		}()
	}
	for _ in 0 .. submitters {
		wait_for_bool_signal(attempting, 'bounded submitter did not start')
	}

	for _ in 0 .. initial_jobs {
		release <- true
	}

	mut seen_accepted := []bool{len: submitters}
	for _ in 0 .. submitters {
		i := wait_for_int_signal(accepted, 'bounded submitter was not accepted')
		assert i >= 0
		assert i < submitters
		assert !seen_accepted[i]
		seen_accepted[i] = true
	}
	mut seen_ran := []bool{len: submitters}
	for _ in 0 .. submitters {
		i := wait_for_int_signal(ran, 'accepted bounded submitter job did not run')
		assert i >= 0
		assert i < submitters
		assert !seen_ran[i]
		seen_ran[i] = true
	}

	pool.close()!
	for t in submit_threads {
		t.wait()
	}
}

fn test_pool_bounded_submit_stress_waiting_submitters_rejected_on_close() {
	workers := 2
	queue_size := 2
	initial_jobs := workers + queue_size
	mut submitters := 8
	$if windows {
		submitters = initial_jobs
	}
	mut pool := xasync.new_pool(workers: workers, queue_size: queue_size)!
	started := chan bool{cap: initial_jobs}
	release := chan bool{cap: initial_jobs}
	attempting := chan bool{cap: submitters}
	result := chan string{cap: submitters}
	ran := chan bool{cap: submitters}
	closed := chan bool{cap: 1}
	blocking_job := fn [started, release] (mut ctx context.Context) ! {
		_ = ctx
		started <- true
		_ := <-release
	}

	for _ in 0 .. initial_jobs {
		pool.try_submit(blocking_job)!
	}
	for _ in 0 .. workers {
		wait_for_bool_signal(started, 'initial pool worker did not start')
	}

	mut submit_threads := []thread{}
	for i in 0 .. submitters {
		submit_threads << spawn fn [mut pool, attempting, result, ran, i] () {
			attempting <- true
			pool.submit_with_context(context.background(), fn [ran] (mut ctx context.Context) ! {
				_ = ctx
				ran <- true
			}) or {
				result <- err.msg()
				return
			}
			result <- 'accepted ${i}'
		}()
	}
	for _ in 0 .. submitters {
		wait_for_bool_signal(attempting, 'bounded submitter did not start')
	}

	close_thread := spawn fn [mut pool, closed] () {
		pool.close() or {
			closed <- false
			return
		}
		closed <- true
	}()
	for _ in 0 .. submitters {
		msg := wait_for_string_signal(result, 'bounded submitter did not return after close')
		assert msg == 'async: pool is closed'
	}
	assert_no_bool_signal(ran, 'bounded submitter job ran after close started')

	for _ in 0 .. initial_jobs {
		release <- true
	}
	wait_for_bool_signal(closed, 'pool close did not finish after releasing accepted jobs')
	for t in submit_threads {
		t.wait()
	}
	close_thread.wait()
}

fn wait_for_bool_signal(signal chan bool, message string) {
	select {
		ok := <-signal {
			assert ok
		}
		pool_test_signal_timeout() {
			assert false, message
		}
	}
}

fn wait_for_int_signal(signal chan int, message string) int {
	select {
		value := <-signal {
			return value
		}
		pool_test_signal_timeout() {
			assert false, message
		}
	}
	return 0
}

fn wait_for_string_signal(signal chan string, message string) string {
	select {
		value := <-signal {
			return value
		}
		pool_test_signal_timeout() {
			assert false, message
		}
	}
	return ''
}

fn pool_test_signal_timeout() time.Duration {
	$if windows {
		return 5 * time.second
	}
	return 1 * time.second
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
