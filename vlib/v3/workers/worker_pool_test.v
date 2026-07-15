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
	pool.close()
}
