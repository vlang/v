module pool

import sync

import runtime

[trusted]
fn C.atomic_fetch_add_u32(voidptr, u32) u32

pub const (
	no_result = voidptr(0)
)

pub struct PoolProcessor {
	thread_cb       voidptr
mut:
	njobs           int
	items           []voidptr
	results         []voidptr
	ntask           u32 // reading/writing to this should be atomic
	waitgroup       sync.WaitGroup
	shared_context  voidptr
	thread_contexts []voidptr
}

pub type ThreadCB = fn (p &PoolProcessor, idx int, task_id int) voidptr

pub struct PoolProcessorConfig {
	maxjobs  int
	callback ThreadCB
}

// new_pool_processor returns a new PoolProcessor instance.
// The parameters of new_pool_processor are:
//    context.maxjobs: when 0 (the default), the PoolProcessor will use a
//      number of threads, that is optimal for your system to process your items.
//    context.callback: this should be a callback function, that each worker
//      thread in the pool will run for each item.
//      The callback function will receive as parameters:
//      1) the PoolProcessor instance, so it can call
//            p.get_item<int>(idx) to get the actual item at index idx
//      2) idx - the index of the currently processed item
//      3) task_id - the index of the worker thread in which the callback
//            function is running.
pub fn new_pool_processor(context PoolProcessorConfig) &PoolProcessor {
	if isnil(context.callback) {
		panic('You need to pass a valid callback to new_pool_processor.')
	}
	mut pool := &PoolProcessor {
		items: []
		results: []
		shared_context: voidptr(0)
		thread_contexts: []
		njobs: context.maxjobs
		ntask: 0
		thread_cb: voidptr(context.callback)
	}
	pool.waitgroup.init()
	return pool
}

// set_max_jobs gives you the ability to override the number
// of jobs *after* the PoolProcessor had been created already.
pub fn (mut pool PoolProcessor) set_max_jobs(njobs int) {
	pool.njobs = njobs
}

// work_on_items receives a list of items of type T,
// then starts a work pool of pool.njobs threads, each running
// pool.thread_cb in a loop, untill all items in the list,
// are processed.
// When pool.njobs is 0, the number of jobs is determined
// by the number of available cores on the system.
// work_on_items returns *after* all threads finish.
// You can optionally call get_results after that.
pub fn (mut pool PoolProcessor) work_on_items<T>(items []T) {
	pool.work_on_pointers( unsafe { items.pointers() } )
}

pub fn (mut pool PoolProcessor) work_on_pointers(items []voidptr) {
	mut njobs := runtime.nr_jobs()
	if pool.njobs > 0 {
		njobs = pool.njobs
	}
	pool.items = []
	pool.results = []
	pool.thread_contexts = []
	pool.items << items
	pool.results = []voidptr{len:(pool.items.len)}
	pool.thread_contexts << []voidptr{len:(pool.items.len)}
	pool.waitgroup.add(njobs)
	for i := 0; i < njobs; i++ {
		if njobs > 1 {
			go process_in_thread(mut pool,i)
		} else {
			// do not run concurrently, just use the same thread:
			process_in_thread(mut pool,i)
		}
	}
	pool.waitgroup.wait()
}

// process_in_thread does the actual work of worker thread.
// It is a workaround for the current inability to pass a
// method in a callback.
fn process_in_thread(mut pool PoolProcessor, task_id int) {
	cb := ThreadCB(pool.thread_cb)
	ilen := pool.items.len
	for {
		idx := int(C.atomic_fetch_add_u32(&pool.ntask, 1))
		if idx >= ilen {
			break
		}
		pool.results[idx] = cb(pool, idx, task_id)
	}
	pool.waitgroup.done()
}

// get_item - called by the worker callback.
// Retrieves a type safe instance of the currently processed item
pub fn (pool &PoolProcessor) get_item<T>(idx int) T {
	return *(&T(pool.items[idx]))
}

// get_result - called by the main thread to get a specific result.
// Retrieves a type safe instance of the produced result.
pub fn (pool &PoolProcessor) get_result<T>(idx int) T {
	return *(&T(pool.results[idx]))
}

// get_results - get a list of type safe results in the main thread.
pub fn (pool &PoolProcessor) get_results<T>() []T {
	mut res := []T{}
	for i in 0 .. pool.results.len {
		res << *(&T(pool.results[i]))
	}
	return res
}

// set_shared_context - can be called during the setup so that you can
// provide a context that is shared between all worker threads, like
// common options/settings.
pub fn (mut pool PoolProcessor) set_shared_context(context voidptr) {
	pool.shared_context = context
}

// get_shared_context - can be called in each worker callback, to get
// the context set by pool.set_shared_context
pub fn (pool &PoolProcessor) get_shared_context() voidptr {
	return pool.shared_context
}

// set_thread_context - can be called during the setup at the start of
// each worker callback, so that the worker callback can have some thread
// local storage area where it can write/read information that is private
// to the given thread, without worrying that it will get overwritten by
// another thread
pub fn (mut pool PoolProcessor) set_thread_context(idx int, context voidptr) {
	pool.thread_contexts[idx] = context
}

// get_thread_context - returns a pointer, that was set with
// pool.set_thread_context . This pointer is private to each thread.
pub fn (pool &PoolProcessor) get_thread_context(idx int) voidptr {
	return pool.thread_contexts[idx]
}
