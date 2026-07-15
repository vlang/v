module workers

struct PoolTestArg {
mut:
	value int
}

fn pool_test_task(arg voidptr) voidptr {
	mut a := unsafe { &PoolTestArg(arg) }
	a.value++
	return unsafe { nil }
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
