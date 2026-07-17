module workers

import os
import time

#include "@VEXEROOT/vlib/v3/pthread_helper.h"

// Parser, checker, transform, mark-used, and C generation have deep recursive
// paths. Preserve their former 64 MiB worker stack while the persistent pool
// avoids reserving a new set of stacks for every compiler phase.
const compiler_worker_stack_size = 64 * 1024 * 1024

// The pool reserves compiler_worker_stack_size per worker, so a full pool can
// reserve a large amount of address space. V3_WORKER_STACK_MB overrides the
// per-worker stack (in MiB) for hosts that have measured a lower high-water use
// or that need more headroom. Values below min_worker_stack_size are clamped so
// a mistuned setting cannot hand the recursive phases a too-small stack.
const min_worker_stack_size = 4 * 1024 * 1024

// worker_stack_size resolves the per-worker stack size, honoring the
// V3_WORKER_STACK_MB override and falling back to the default when it is unset,
// non-numeric, or non-positive.
fn worker_stack_size() usize {
	requested_mb := os.getenv('V3_WORKER_STACK_MB')
	if requested_mb.len == 0 {
		return usize(compiler_worker_stack_size)
	}
	mb := requested_mb.int()
	if mb <= 0 {
		return usize(compiler_worker_stack_size)
	}
	mut bytes := i64(mb) * 1024 * 1024
	if bytes < min_worker_stack_size {
		bytes = min_worker_stack_size
	}
	return usize(bytes)
}

// Task is one type-erased compiler phase callback submitted to Pool.
pub struct Task {
pub:
	run        fn (voidptr) voidptr = unsafe { nil }
	arg        voidptr
	force_sync bool
	stop       bool
mut:
	queued_at_ns u64
}

// Stats is a cumulative snapshot of persistent worker-pool activity.
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

struct Completion {
	queue_wait_ns u64
	worker_run_ns u64
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
	jobs                   chan Task
	done                   chan Completion
	threads                []C.pthread_t
	is_closed              bool
	task_count             u64
	async_task_count       u64
	forced_sync_task_count u64
	fallback_task_count    u64
	launch_attempt_count   u64
	launch_failure_count   u64
	queue_wait_ns          u64
	worker_run_ns          u64
	started_at_ns          u64
}

fn pool_worker(arg voidptr) voidptr {
	mut pool := unsafe { &Pool(arg) }
	for {
		task := <-pool.jobs
		if task.stop {
			break
		}
		started_at := time.sys_mono_now()
		task.run(task.arg)
		finished_at := time.sys_mono_now()
		pool.done <- Completion{
			queue_wait_ns: if started_at >= task.queued_at_ns {
				started_at - task.queued_at_ns
			} else {
				0
			}
			worker_run_ns: if finished_at >= started_at { finished_at - started_at } else { 0 }
		}
	}
	return unsafe { nil }
}

// new creates up to size persistent workers. Failed launches simply reduce
// the available parallelism; run executes synchronously if none launch.
pub fn new(size int) &Pool {
	wanted := if size < 0 { 0 } else { size }
	mut pool := &Pool{
		jobs:                 chan Task{cap: if wanted > 0 { wanted * 2 } else { 1 }}
		done:                 chan Completion{cap: if wanted > 0 { wanted * 2 } else { 1 }}
		launch_attempt_count: u64(wanted)
		started_at_ns:        time.sys_mono_now()
	}
	fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
	stack_size := worker_stack_size()
	for idx in 0 .. wanted {
		mut thread_id := C.pthread_t{}
		result := if fail == 'pool:all' || fail == 'pool:${idx}' {
			11
		} else {
			C.v3_pthread_create(&thread_id, stack_size, pool_worker, voidptr(pool))
		}
		if result == 0 {
			pool.threads << thread_id
		} else {
			pool.launch_failure_count++
		}
	}
	return pool
}

// size reports the number of successfully launched persistent workers.
pub fn (p &Pool) size() int {
	return p.threads.len
}

fn (mut p Pool) record_completion(completion Completion) {
	p.queue_wait_ns += completion.queue_wait_ns
	p.worker_run_ns += completion.worker_run_ns
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
			if task.force_sync {
				p.forced_sync_task_count++
			} else {
				p.fallback_task_count++
			}
		}
		p.task_count += u64(tasks.len)
		return false
	}
	mut submitted := 0
	mut completed := 0
	for task in tasks {
		if !task.force_sync {
			queued_task := Task{
				run:          task.run
				arg:          task.arg
				queued_at_ns: time.sys_mono_now()
			}
			mut is_submitted := false
			for !is_submitted {
				select {
					p.jobs <- queued_task {
						submitted++
						is_submitted = true
					}
					completion := <-p.done {
						p.record_completion(completion)
						completed++
					}
				}
			}
		}
	}
	for task in tasks {
		if task.force_sync {
			task.run(task.arg)
			p.forced_sync_task_count++
		}
	}
	for completed < submitted {
		completion := <-p.done
		p.record_completion(completion)
		completed++
	}
	p.async_task_count += u64(submitted)
	p.task_count += u64(tasks.len)
	return submitted > 0
}

// tasks_run reports the number of phase callbacks completed through this pool.
pub fn (p &Pool) tasks_run() u64 {
	return p.task_count
}

// stats returns cumulative scheduling and utilization counters.
pub fn (p &Pool) stats() Stats {
	now := time.sys_mono_now()
	elapsed_ns := if now >= p.started_at_ns { now - p.started_at_ns } else { 0 }
	capacity_ns := elapsed_ns * u64(p.threads.len)
	utilization_ppm := if capacity_ns > 0 {
		p.worker_run_ns * 1_000_000 / capacity_ns
	} else {
		0
	}
	return Stats{
		tasks_run:         p.task_count
		async_tasks:       p.async_task_count
		forced_sync_tasks: p.forced_sync_task_count
		fallback_tasks:    p.fallback_task_count
		launch_attempts:   p.launch_attempt_count
		launch_failures:   p.launch_failure_count
		queue_wait_ns:     p.queue_wait_ns
		worker_run_ns:     p.worker_run_ns
		utilization_ppm:   utilization_ppm
	}
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
