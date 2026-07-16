module workers

// Stats is a cumulative snapshot of serial worker-pool activity.
pub struct Stats {
pub:
	tasks_run         u64
	async_tasks       u64
	forced_sync_tasks u64
	fallback_tasks    u64
	launch_attempts   u64
	launch_failures   u64
	queue_wait_ns     u64
	worker_run_ns     u64
	utilization_ppm   u64
}

// Task is one type-erased compiler phase callback submitted to Pool.
pub struct Task {
pub:
	run        fn (voidptr) voidptr = unsafe { nil }
	arg        voidptr
	force_sync bool
}

// Pool is a serial compatibility implementation on Windows.
@[heap]
pub struct Pool {
mut:
	task_count             u64
	forced_sync_task_count u64
	fallback_task_count    u64
}

// new creates a serial compatibility pool.
pub fn new(_ int) &Pool {
	return &Pool{}
}

// size reports the number of persistent workers.
pub fn (p &Pool) size() int {
	return 0
}

// run executes a compiler phase batch synchronously.
pub fn (mut p Pool) run(tasks []Task) bool {
	for task in tasks {
		task.run(task.arg)
		if task.force_sync {
			p.forced_sync_task_count++
		} else {
			p.fallback_task_count++
		}
	}
	p.task_count += u64(tasks.len)
	return false
}

// tasks_run reports the number of completed phase callbacks.
pub fn (p &Pool) tasks_run() u64 {
	return p.task_count
}

// stats returns cumulative serial fallback counters.
pub fn (p &Pool) stats() Stats {
	return Stats{
		tasks_run:         p.task_count
		forced_sync_tasks: p.forced_sync_task_count
		fallback_tasks:    p.fallback_task_count
	}
}

// close releases pool resources.
pub fn (mut p Pool) close() {}
