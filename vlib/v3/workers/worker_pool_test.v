module workers

import os

struct PoolTestArg {
mut:
	value int
}

fn pool_test_task(arg voidptr) voidptr {
	mut a := unsafe { &PoolTestArg(arg) }
	a.value++
	return unsafe { nil }
}

fn test_pool_preserves_recursive_phase_stack_size() {
	$if !windows {
		assert compiler_worker_stack_size == 64 * 1024 * 1024
	}
}

fn test_worker_stack_size_env_override_and_clamp() {
	$if !windows {
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
	$if windows {
		assert stats.async_tasks == 0
		assert stats.forced_sync_tasks == 2
		assert stats.fallback_tasks == 6
	} $else {
		assert stats.async_tasks == 6
		assert stats.forced_sync_tasks == 2
		assert stats.fallback_tasks == 0
		assert stats.launch_attempts == 2
		assert stats.launch_failures == 0
		assert stats.worker_run_ns > 0
	}
	pool.close()
}

fn test_pool_falls_back_for_every_failed_launch_index() {
	$if !windows {
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
}

fn test_pool_drains_fast_completions_while_submitting_large_batch() {
	$if !windows {
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
}
