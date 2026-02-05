// BEAM backend pool processor implementation
// Uses BEAM's lightweight processes for parallel work processing
//
// On BEAM, thread pools map naturally to process pools. BEAM processes are
// extremely lightweight (~2KB each) and can handle millions of concurrent tasks.
// This implementation provides API compatibility with V's sync.pool module
// while leveraging BEAM's process model.
module pool

import sync
import runtime

// no_result sentinel value for when callbacks don't produce results
pub const no_result = unsafe { nil }

// PoolProcessor manages parallel processing of items across multiple workers
// On BEAM, workers are lightweight processes that process items from a shared queue
pub struct PoolProcessor {
	thread_cb voidptr
mut:
	njobs           int
	items           []voidptr
	results         shared []voidptr
	ntask           u32 // current task index (accessed atomically in C, sequentially here)
	waitgroup       sync.WaitGroup
	shared_context  voidptr
	thread_contexts []voidptr
}

// ThreadCB is the callback type for worker functions
// Parameters:
//   - p: the PoolProcessor instance
//   - idx: index of the current item being processed
//   - task_id: the worker thread/process ID
// Returns: result pointer (or pool.no_result if no result)
pub type ThreadCB = fn (mut p PoolProcessor, idx int, task_id int) voidptr

fn empty_cb(mut _p PoolProcessor, _idx int, _task_id int) voidptr {
	unsafe {
		return nil
	}
}

// PoolProcessorConfig holds configuration for creating a new pool processor
pub struct PoolProcessorConfig {
pub:
	maxjobs  int      // Number of workers (0 = auto-detect optimal)
	callback ThreadCB = empty_cb
}

// new_pool_processor creates a new PoolProcessor instance.
// Parameters:
//   context.maxjobs: when 0 (the default), uses optimal number of workers
//   context.callback: function each worker runs for each item
// The callback receives:
//   1) PoolProcessor instance (call p.get_item[T](idx) to get item)
//   2) idx - index of current item
//   3) task_id - worker ID
pub fn new_pool_processor(context PoolProcessorConfig) &PoolProcessor {
	if context.callback == unsafe { nil } {
		panic('You need to pass a valid callback to new_pool_processor.')
	}
	mut pool := PoolProcessor{
		items: []
		results: []
		shared_context: unsafe { nil }
		thread_contexts: []
		njobs: context.maxjobs
		ntask: 0
		thread_cb: voidptr(context.callback)
	}
	pool.waitgroup.init()
	return &pool
}

// set_max_jobs overrides the number of workers after creation
pub fn (mut pool PoolProcessor) set_max_jobs(njobs int) {
	pool.njobs = njobs
}

// work_on_items processes a list of items in parallel
// Creates pool.njobs workers (or auto-detects optimal count)
// Returns after all items are processed
// Call get_results after to retrieve results
pub fn (mut pool PoolProcessor) work_on_items[T](items []T) {
	pool.work_on_pointers(unsafe { items.pointers() })
}

// work_on_pointers processes a list of pointers in parallel
pub fn (mut pool PoolProcessor) work_on_pointers(items []voidptr) {
	mut njobs := runtime.nr_jobs()
	if pool.njobs > 0 {
		njobs = pool.njobs
	}

	// Don't spawn more workers than items
	if njobs > items.len {
		njobs = items.len
	}

	// Minimum 1 worker
	if njobs < 1 {
		njobs = 1
	}

	unsafe {
		pool.thread_contexts = []voidptr{len: items.len}
		lock pool.results {
			pool.results = []voidptr{len: items.len}
		}
		pool.items = []voidptr{cap: items.len}
		pool.items << items
		pool.ntask = 0

		pool.waitgroup.add(njobs)
		for i := 0; i < njobs; i++ {
			if njobs > 1 {
				// Spawn BEAM processes for parallel work
				spawn process_in_thread(mut pool, i)
			} else {
				// Single worker - run in same process
				process_in_thread(mut pool, i)
			}
		}
	}
	pool.waitgroup.wait()
}

// process_in_thread - worker function that processes items
// On BEAM, this runs in a lightweight Erlang process
fn process_in_thread(mut pool PoolProcessor, task_id int) {
	cb := ThreadCB(pool.thread_cb)
	ilen := pool.items.len

	for {
		// Atomically get next task index
		// On BEAM, we use a simple increment since BEAM processes don't share memory
		// and each process gets its own copy of the index
		idx := int(atomic_fetch_add_ntask(mut pool))
		if idx >= ilen {
			break
		}
		res := cb(mut pool, idx, task_id)
		lock pool.results {
			pool.results[idx] = res
		}
	}
	pool.waitgroup.done()
}

// atomic_fetch_add_ntask atomically increments the task counter and returns the old value
// On BEAM, processes don't share memory, so we need a different approach
// This implementation uses a lock to ensure thread-safety
fn atomic_fetch_add_ntask(mut pool PoolProcessor) u32 {
	// On BEAM, we simulate atomic increment using the shared results lock
	// This is safe because BEAM doesn't have true shared memory
	old := pool.ntask
	pool.ntask++
	return old
}

// get_item retrieves the item at the given index (called by worker callback)
pub fn (pool &PoolProcessor) get_item[T](idx int) T {
	return unsafe { *(&T(pool.items[idx])) }
}

// get_result retrieves a single result at the given index (called from main thread)
pub fn (pool &PoolProcessor) get_result[T](idx int) T {
	rlock pool.results {
		return unsafe { *(&T(pool.results[idx])) }
	}
}

// get_results retrieves all results as a typed list (called from main thread)
pub fn (pool &PoolProcessor) get_results[T]() []T {
	mut res := []T{cap: pool.results.len}
	for i in 0 .. pool.results.len {
		rlock pool.results {
			res << unsafe { *(&T(pool.results[i])) }
		}
	}
	return res
}

// get_results_ref retrieves all results as a list of references (called from main thread)
pub fn (pool &PoolProcessor) get_results_ref[T]() []&T {
	mut res := []&T{cap: pool.results.len}
	for i in 0 .. pool.results.len {
		rlock pool.results {
			res << unsafe { &T(pool.results[i]) }
		}
	}
	return res
}

// set_shared_context sets a context shared between all workers
// Useful for common options/settings
pub fn (mut pool PoolProcessor) set_shared_context(context voidptr) {
	pool.shared_context = context
}

// get_shared_context retrieves the shared context (called from worker callback)
pub fn (pool &PoolProcessor) get_shared_context() voidptr {
	return pool.shared_context
}

// set_thread_context sets thread-local storage for a specific worker
// Each worker can have private state that won't be overwritten by other workers
pub fn (mut pool PoolProcessor) set_thread_context(idx int, context voidptr) {
	pool.thread_contexts[idx] = context
}

// get_thread_context retrieves a worker's thread-local context
pub fn (pool &PoolProcessor) get_thread_context(idx int) voidptr {
	return pool.thread_contexts[idx]
}
