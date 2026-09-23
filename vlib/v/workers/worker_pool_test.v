module workers

import os
import time

struct PoolTestArg {
mut:
	value int
	// work absorbs a small spin so a task's measured run time is above the
	// 100 ns resolution of the Windows monotonic clock; otherwise the
	// worker_run_ns/utilization_ppm assertions below can see a zero sample.
	work u64
}

fn pool_test_task(arg voidptr) voidptr {
	$if prealloc {
		scope := unsafe { prealloc_scope_begin() }
		defer {
			unsafe {
				prealloc_scope_end(scope)
			}
		}
	}
	mut a := unsafe { &PoolTestArg(arg) }
	mut acc := u64(0)
	for i in 0 .. 20000 {
		acc = acc * 6364136223846793005 + u64(i)
	}
	a.work = acc
	a.value++
	return unsafe { nil }
}

fn test_pool_preserves_recursive_phase_stack_size() {
	assert compiler_worker_stack_size == 64 * 1024 * 1024
}

fn test_worker_stack_size_env_override_and_clamp() {
	os.unsetenv('V3_WORKER_STACK_MB')
	assert worker_stack_size() == usize(compiler_worker_stack_size)

	os.setenv('V3_WORKER_STACK_MB', '16', true)
	assert worker_stack_size() == usize(16 * 1024 * 1024)

	// Below the floor is clamped up to min_worker_stack_size.
	os.setenv('V3_WORKER_STACK_MB', '1', true)
	assert worker_stack_size() == usize(min_worker_stack_size)

	// Non-numeric / non-positive values fall back to the default.
	os.setenv('V3_WORKER_STACK_MB', 'not-a-number', true)
	assert worker_stack_size() == usize(compiler_worker_stack_size)
	os.setenv('V3_WORKER_STACK_MB', '0', true)
	assert worker_stack_size() == usize(compiler_worker_stack_size)

	os.unsetenv('V3_WORKER_STACK_MB')
}

fn test_pool_runs_persistent_batches_and_sync_fallbacks() {
	mut pool := new(2)
	mut args := []&PoolTestArg{cap: 4}
	mut tasks := []Task{cap: 4}
	for idx in 0 .. 4 {
		args << &PoolTestArg{}
		tasks << Task{
			run:        pool_test_task
			arg:        voidptr(args[idx])
			force_sync: idx == 3
		}
	}
	pool.run(tasks)
	pool.run(tasks)
	assert args.map(it.value) == [2, 2, 2, 2]
	assert pool.tasks_run() == 8
	stats := pool.stats()
	assert stats.tasks_run == 8
	assert stats.async_tasks == 6
	assert stats.forced_sync_tasks == 2
	assert stats.fallback_tasks == 0
	assert stats.launch_attempts == 2
	assert stats.launch_failures == 0
	// The waiting caller may run queued tasks itself, so the work can land on
	// either side of the split.
	assert stats.worker_run_ns + stats.caller_run_ns > 0
	pool.close()
	if stats.worker_run_ns > 0 {
		assert pool.stats().utilization_ppm > 0
	}
}

fn test_caller_run_time_is_excluded_from_worker_utilization() {
	mut batch := BatchStats{}
	batch.record_completion(Completion{ queue_wait_ns: 5, run_ns: 7, on_worker: true })
	batch.record_completion(Completion{ queue_wait_ns: 3, run_ns: 60_000_000_000, on_worker: false })
	assert batch.worker_run_ns == 7
	assert batch.caller_run_ns == 60_000_000_000
	assert batch.queue_wait_ns == 8
	// One persistent worker, one second old: a minute of caller-run work must
	// not count as worker utilization.
	mut pool := &Pool{
		launched_thread_count: 1
		started_at_ns:         time.sys_mono_now() - 1_000_000_000
	}
	pool.merge_batch_stats(batch)
	stats := pool.stats()
	assert stats.worker_run_ns == 7
	assert stats.caller_run_ns == 60_000_000_000
	assert stats.utilization_ppm <= 1_000_000
}

fn test_pool_falls_back_for_every_failed_launch_index() {
	old_failure := os.getenv_opt('V3_TEST_PTHREAD_CREATE_FAIL')
	defer {
		if value := old_failure {
			os.setenv('V3_TEST_PTHREAD_CREATE_FAIL', value, true)
		} else {
			os.unsetenv('V3_TEST_PTHREAD_CREATE_FAIL')
		}
	}
	for failure in ['pool:0', 'pool:1', 'pool:2', 'pool:all'] {
		os.setenv('V3_TEST_PTHREAD_CREATE_FAIL', failure, true)
		mut pool := new(3)
		mut args := []&PoolTestArg{cap: 6}
		mut tasks := []Task{cap: 6}
		for _ in 0 .. 6 {
			args << &PoolTestArg{}
			tasks << Task{
				run: pool_test_task
				arg: voidptr(args.last())
			}
		}
		pool.run(tasks)
		assert args.all(it.value == 1), failure
		stats := pool.stats()
		assert stats.launch_attempts == 3
		if failure == 'pool:all' {
			assert stats.launch_failures == 3
			assert stats.fallback_tasks == 6
		} else {
			assert stats.launch_failures == 1
			assert stats.async_tasks == 6
		}
		pool.close()
	}
}

fn test_pool_drains_fast_completions_while_submitting_large_batch() {
	mut pool := new(1)
	mut args := []&PoolTestArg{cap: 256}
	mut tasks := []Task{cap: 256}
	for _ in 0 .. 256 {
		args << &PoolTestArg{}
		tasks << Task{
			run: pool_test_task
			arg: voidptr(args.last())
		}
	}
	assert pool.run(tasks)
	assert args.all(it.value == 1)
	stats := pool.stats()
	assert stats.tasks_run == 256
	assert stats.async_tasks == 256
	pool.close()
}

struct ConcurrentPoolRun {
mut:
	pool &Pool = unsafe { nil }
	args []&PoolTestArg
}

fn concurrent_pool_run_thread(arg voidptr) voidptr {
	mut run := unsafe { &ConcurrentPoolRun(arg) }
	mut tasks := []Task{cap: run.args.len}
	for i, a in run.args {
		tasks << Task{
			run:        pool_test_task
			arg:        voidptr(a)
			force_sync: i == 0
		}
	}
	run.pool.run(tasks)
	return unsafe { nil }
}

fn test_concurrent_batches_account_only_their_own_tasks() {
	// The caller of a batch drains queued tasks while it waits, and may run a
	// task queued by another batch. Every task must still run exactly once and
	// each batch must return only after all of its own tasks finished.
	mut pool := new(3)
	for _ in 0 .. 20 {
		mut first := &ConcurrentPoolRun{
			pool: pool
		}
		mut second := &ConcurrentPoolRun{
			pool: pool
		}
		for _ in 0 .. 40 {
			first.args << &PoolTestArg{}
			second.args << &PoolTestArg{}
		}
		t1 := spawn concurrent_pool_run_thread(voidptr(first))
		t2 := spawn concurrent_pool_run_thread(voidptr(second))
		t1.wait()
		t2.wait()
		for a in first.args {
			assert a.value == 1
		}
		for a in second.args {
			assert a.value == 1
		}
	}
	// Each batch merges its own counters, so concurrent batches lose no updates:
	// 20 rounds of 2 batches, each with 1 caller-run and 39 queued tasks.
	stats := pool.stats()
	assert stats.tasks_run == 20 * 2 * 40
	assert stats.async_tasks == 20 * 2 * 39
	assert stats.forced_sync_tasks == 20 * 2
	assert pool.tasks_run() == 20 * 2 * 40
	pool.close()
}
