@[has_globals]
module workers

import os
import sync
import time

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

// A completion push normally leaves Channel.push within a few instructions of
// its value becoming receivable, so Pool.run spins briefly before yielding.
const completion_push_spins = 100

fn C.atomic_load_u32(voidptr) u32

fn C.atomic_fetch_sub_u32(voidptr, u32) u32

fn C.cpu_relax()

__global v3_pool_size_limit int

// limit_pool_size caps pools created after this call to at most `size` workers.
pub fn limit_pool_size(size int) {
	if size > 0 && (v3_pool_size_limit == 0 || size < v3_pool_size_limit) {
		v3_pool_size_limit = size
	}
}

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
	// done receives the completion of a queued task. Each Pool.run batch has its
	// own channel, so a batch never counts another batch's completions, even
	// when it runs one of their queued tasks itself.
	done chan Completion
	// pushes_in_flight is the batch's count of queued tasks that have not yet
	// returned from their push into `done` (see wait_for_completion_pushes).
	pushes_in_flight &u32 = unsafe { nil }
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
	// caller_run_ns is queued work that a waiting Pool.run caller ran itself. It
	// is excluded from utilization_ppm, which measures the persistent workers.
	caller_run_ns   u64
	utilization_ppm u64
}

struct Completion {
	queue_wait_ns u64
	run_ns        u64
	on_worker     bool // run by a persistent worker rather than the waiting caller
}

// BatchStats accumulates one Pool.run batch's counters without touching the
// shared pool, which concurrent batches would otherwise update racily.
struct BatchStats {
mut:
	tasks             u64
	async_tasks       u64
	forced_sync_tasks u64
	fallback_tasks    u64
	queue_wait_ns     u64
	worker_run_ns     u64
	caller_run_ns     u64
}

fn (mut s BatchStats) record_completion(completion Completion) {
	s.queue_wait_ns += completion.queue_wait_ns
	if completion.on_worker {
		s.worker_run_ns += completion.run_ns
	} else {
		s.caller_run_ns += completion.run_ns
	}
}

// Pool owns a bounded set of persistent compiler workers. Phase payloads stay
// owned by the submitting thread until run returns. Thread creation and
// joining are the only platform-specific parts (thread_nix.c.v and
// thread_windows.c.v); the queueing and accounting are shared.
@[heap]
pub struct Pool {
mut:
	jobs    chan Task
	threads []WorkerThread
	// stats_lock guards the cumulative task and timing counters below; each
	// batch merges its own totals once, since batches may run concurrently.
	stats_lock             &sync.Mutex = sync.new_mutex()
	is_closed              bool
	task_count             u64
	async_task_count       u64
	forced_sync_task_count u64
	fallback_task_count    u64
	launch_attempt_count   u64
	launch_failure_count   u64
	launched_thread_count  u64
	caller_steals          bool
	queue_wait_ns          u64
	worker_run_ns          u64
	caller_run_ns          u64
	started_at_ns          u64
}

fn pool_worker(arg voidptr) voidptr {
	mut pool := unsafe { &Pool(arg) }
	for {
		task := <-pool.jobs
		if task.stop {
			break
		}
		run_queued_task(task, true)
	}
	$if prealloc {
		unsafe {
			prealloc_thread_cleanup()
		}
	}
	return unsafe { nil }
}

// run_queued_task runs a queued task and reports its queue wait and run time on
// the task's batch channel, noting whether a persistent worker or a draining
// caller picked it up.
fn run_queued_task(task Task, on_worker bool) {
	started_at := time.sys_mono_now()
	task.run(task.arg)
	finished_at := time.sys_mono_now()
	task.done <- Completion{
		queue_wait_ns: if started_at >= task.queued_at_ns {
			started_at - task.queued_at_ns
		} else {
			0
		}
		run_ns:        if finished_at >= started_at { finished_at - started_at } else { 0 }
		on_worker:     on_worker
	}
	// Only now has the push stopped touching `done`. This must stay the last
	// access to batch memory: once the count drops to zero, Pool.run returns and
	// its caller may release the `done` channel and the counter itself.
	C.atomic_fetch_sub_u32(task.pushes_in_flight, 1)
}

// wait_for_completion_pushes returns once every queued task of a batch has
// returned from its push into the batch's `done` channel. Receiving the last
// completion is not enough: a buffered Channel.push makes the value receivable
// by posting the reader semaphore, and only then locks the channel's
// `read_sub_mtx` to wake select subscribers; the semaphore post itself can also
// still touch the channel after the token was taken. `done` is allocated by
// the caller of Pool.run, which in the compiler is often a disposable prealloc
// scope that is released right after the stage, so a worker still in that
// window would otherwise write to released memory.
fn wait_for_completion_pushes(pushes_in_flight &u32) {
	mut spins := 0
	for C.atomic_load_u32(pushes_in_flight) != 0 {
		if spins < completion_push_spins {
			spins++
			C.cpu_relax()
		} else {
			// The pushing thread was descheduled inside Channel.push; let it run.
			time.sleep(time.microsecond)
		}
	}
}

// new creates up to size persistent workers. Failed launches simply reduce
// the available parallelism; run executes synchronously if none launch.
pub fn new(size int) &Pool {
	mut wanted := if size < 0 { 0 } else { size }
	if v3_pool_size_limit > 0 && wanted > v3_pool_size_limit {
		wanted = v3_pool_size_limit
	}
	// Compiler phases deliberately oversubscribe the workers with small chunks
	// so that uneven AST bodies do not leave cores idle. Buffer the whole normal
	// batch: otherwise Pool.run has to wait for early completions while it is
	// still submitting work, delaying the caller's force_sync chunk.
	queue_cap := if wanted > 0 { wanted * 16 } else { 1 }
	mut pool := &Pool{
		jobs:                 chan Task{cap: queue_cap}
		launch_attempt_count: u64(wanted)
		started_at_ns:        time.sys_mono_now()
		caller_steals:        os.getenv('V3_NO_POOL_STEAL') == ''
	}
	fail := os.getenv('V3_TEST_PTHREAD_CREATE_FAIL')
	stack_size := worker_stack_size()
	for idx in 0 .. wanted {
		if fail == 'pool:all' || fail == 'pool:${idx}' {
			pool.launch_failure_count++
			continue
		}
		worker, result := worker_thread_create(stack_size, pool_worker, voidptr(pool))
		if result == 0 {
			pool.threads << worker
		} else {
			pool.launch_failure_count++
		}
	}
	pool.launched_thread_count = u64(pool.threads.len)
	return pool
}

// size reports the number of successfully launched persistent workers.
pub fn (p &Pool) size() int {
	return p.threads.len
}

fn (mut p Pool) merge_batch_stats(s BatchStats) {
	p.stats_lock.lock()
	p.task_count += s.tasks
	p.async_task_count += s.async_tasks
	p.forced_sync_task_count += s.forced_sync_tasks
	p.fallback_task_count += s.fallback_tasks
	p.queue_wait_ns += s.queue_wait_ns
	p.worker_run_ns += s.worker_run_ns
	p.caller_run_ns += s.caller_run_ns
	p.stats_lock.unlock()
}

// stats_snapshot copies the cumulative counters under the stats lock.
fn (p &Pool) stats_snapshot() BatchStats {
	mut stats_lock := unsafe { p.stats_lock }
	stats_lock.lock()
	snapshot := BatchStats{
		tasks:             p.task_count
		async_tasks:       p.async_task_count
		forced_sync_tasks: p.forced_sync_task_count
		fallback_tasks:    p.fallback_task_count
		queue_wait_ns:     p.queue_wait_ns
		worker_run_ns:     p.worker_run_ns
		caller_run_ns:     p.caller_run_ns
	}
	stats_lock.unlock()
	return snapshot
}

// run executes one compiler phase batch and waits for every callback. Tasks
// marked force_sync run on the caller while submitted tasks use the pool.
pub fn (mut p Pool) run(tasks []Task) bool {
	if tasks.len == 0 {
		return false
	}
	mut batch := BatchStats{
		tasks: u64(tasks.len)
	}
	if p.is_closed || p.threads.len == 0 {
		for task in tasks {
			task.run(task.arg)
			if task.force_sync {
				batch.forced_sync_tasks++
			} else {
				batch.fallback_tasks++
			}
		}
		p.merge_batch_stats(batch)
		return false
	}
	mut async_count := 0
	for task in tasks {
		if !task.force_sync {
			async_count++
		}
	}
	// Buffered for the whole batch, so a worker never blocks on reporting.
	done := chan Completion{cap: if async_count > 0 { async_count } else { 1 }}
	// Every non-force_sync task is submitted below, and whoever runs it
	// decrements this once its completion push has returned. It lives on this
	// stack frame, which outlives the batch because run waits for it to reach 0.
	mut pushes_in_flight := u32(async_count)
	mut submitted := 0
	mut completed := 0
	for task in tasks {
		if !task.force_sync {
			queued_task := Task{
				run:              task.run
				arg:              task.arg
				queued_at_ns:     time.sys_mono_now()
				done:             done
				pushes_in_flight: &pushes_in_flight
			}
			mut is_submitted := false
			for !is_submitted {
				select {
					p.jobs <- queued_task {
						submitted++
						is_submitted = true
					}
					completion := <-done {
						batch.record_completion(completion)
						completed++
					}
				}
			}
		}
	}
	for task in tasks {
		if task.force_sync {
			task.run(task.arg)
			batch.forced_sync_tasks++
		}
	}
	// Help drain the queue instead of idling: when a worker is descheduled (a
	// loaded host) or the caller's own share finished early, the caller runs
	// queued tasks itself. A task is completed on its own batch's channel, so
	// running one queued by a concurrent batch is still accounted correctly.
	for completed < submitted && !p.caller_steals {
		completion := <-done
		batch.record_completion(completion)
		completed++
	}
	for completed < submitted {
		select {
			completion := <-done {
				batch.record_completion(completion)
				completed++
			}
			task := <-p.jobs {
				if task.stop {
					// Only close() queues stop requests; leave them to the workers.
					p.jobs <- task
					continue
				}
				run_queued_task(task, false)
			}
		}
	}
	// Every completion was received, but the last pushers may still be inside
	// Channel.push; `done` and pushes_in_flight must outlive them.
	wait_for_completion_pushes(&pushes_in_flight)
	done.close()
	batch.async_tasks = u64(submitted)
	p.merge_batch_stats(batch)
	return submitted > 0
}

// tasks_run reports the number of phase callbacks completed through this pool.
pub fn (p &Pool) tasks_run() u64 {
	return p.stats_snapshot().tasks
}

// stats returns cumulative scheduling and utilization counters.
pub fn (p &Pool) stats() Stats {
	counters := p.stats_snapshot()
	now := time.sys_mono_now()
	elapsed_ns := if now >= p.started_at_ns { now - p.started_at_ns } else { 0 }
	capacity_ns := elapsed_ns * p.launched_thread_count
	utilization_ppm := if capacity_ns > 0 {
		counters.worker_run_ns * 1_000_000 / capacity_ns
	} else {
		0
	}
	return Stats{
		tasks_run:         counters.tasks
		async_tasks:       counters.async_tasks
		forced_sync_tasks: counters.forced_sync_tasks
		fallback_tasks:    counters.fallback_tasks
		launch_attempts:   p.launch_attempt_count
		launch_failures:   p.launch_failure_count
		queue_wait_ns:     counters.queue_wait_ns
		worker_run_ns:     counters.worker_run_ns
		caller_run_ns:     counters.caller_run_ns
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
	for idx, worker in p.threads {
		if worker_thread_join(worker) != 0 {
			panic('failed to join compiler worker ${idx}')
		}
	}
	p.threads.clear()
}
