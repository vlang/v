module workers

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
	task_count u64
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
	}
	p.task_count += u64(tasks.len)
	return false
}

// tasks_run reports the number of completed phase callbacks.
pub fn (p &Pool) tasks_run() u64 {
	return p.task_count
}

// close releases pool resources.
pub fn (mut p Pool) close() {}
