module workers

import os

#include "@VEXEROOT/vlib/v3/pthread_helper.h"

// C generation has deep recursive paths. A bounded 8 MiB stack preserves the
// proven phase requirement while the persistent pool avoids reserving a new
// set of large stacks for every compiler phase.
const compiler_worker_stack_size = 8 * 1024 * 1024

// Task is one type-erased compiler phase callback submitted to Pool.
pub struct Task {
pub:
	run        fn (voidptr) voidptr = unsafe { nil }
	arg        voidptr
	force_sync bool
	stop       bool
}

// C.pthread_t is the platform pthread handle type.
@[typedef]
struct C.pthread_t {}

fn C.pthread_join(thread C.pthread_t, retval voidptr) int
fn C.v3_pthread_create(thread &C.pthread_t, stack_size usize, start_routine fn (voidptr) voidptr, arg voidptr) int

// Pool owns a bounded set of persistent compiler workers. Phase payloads stay
// owned by the submitting thread until run returns.
@[heap]
pub struct Pool {
mut:
	jobs       chan Task
	done       chan bool
	threads    []C.pthread_t
	is_closed  bool
	task_count u64
}

fn pool_worker(arg voidptr) voidptr {
	mut pool := unsafe { &Pool(arg) }
	for {
		task := <-pool.jobs
		if task.stop {
			break
		}
		task.run(task.arg)
		pool.done <- true
	}
	return unsafe { nil }
}

// new creates up to size persistent workers. Failed launches simply reduce
// the available parallelism; run executes synchronously if none launch.
pub fn new(size int) &Pool {
	wanted := if size < 0 { 0 } else { size }
	mut pool := &Pool{
		jobs: chan Task{cap: if wanted > 0 { wanted * 2 } else { 1 }}
		done: chan bool{cap: if wanted > 0 { wanted * 2 } else { 1 }}
	}
	fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
	for idx in 0 .. wanted {
		mut thread_id := C.pthread_t{}
		result := if fail == 'pool:all' || fail == 'pool:${idx}' {
			11
		} else {
			C.v3_pthread_create(&thread_id, compiler_worker_stack_size, pool_worker, voidptr(pool))
		}
		if result == 0 {
			pool.threads << thread_id
		}
	}
	return pool
}

// size reports the number of successfully launched persistent workers.
pub fn (p &Pool) size() int {
	return p.threads.len
}

// run executes one compiler phase batch and waits for every callback. Tasks
// marked force_sync run on the caller while submitted tasks use the pool.
pub fn (mut p Pool) run(tasks []Task) bool {
	if tasks.len == 0 {
		return false
	}
	if p.is_closed || p.threads.len == 0 {
		for task in tasks {
			task.run(task.arg)
		}
		p.task_count += u64(tasks.len)
		return false
	}
	mut submitted := 0
	for task in tasks {
		if !task.force_sync {
			p.jobs <- task
			submitted++
		}
	}
	for task in tasks {
		if task.force_sync {
			task.run(task.arg)
		}
	}
	for _ in 0 .. submitted {
		_ = <-p.done
	}
	p.task_count += u64(tasks.len)
	return submitted > 0
}

// tasks_run reports the number of phase callbacks completed through this pool.
pub fn (p &Pool) tasks_run() u64 {
	return p.task_count
}

// close stops and joins every persistent worker. Join failures are surfaced.
pub fn (mut p Pool) close() {
	if p.is_closed {
		return
	}
	p.is_closed = true
	for _ in p.threads {
		p.jobs <- Task{
			stop: true
		}
	}
	for idx, thread_id in p.threads {
		if C.pthread_join(thread_id, unsafe { nil }) != 0 {
			panic('failed to join compiler worker ${idx}')
		}
	}
	p.threads.clear()
}
