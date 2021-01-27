module sync

import runtime

// * Goal: this file provides a convenient way to run identical tasks over a list
// * of items in parallel, without worrying about waitgroups, mutexes and so on.
// *
// * Usage example:
// *   struct SResult{ s string }
// *   fn sprocess(p &sync.PoolProcessor, idx, wid int) voidptr {
// *       item := p.get_item<string>(idx)
// *       println('idx: $idx, wid: $wid, item: ' + item)
// *       return &SResult{ item.reverse() }
// *   }
// *   pool := sync.new_pool_processor({ callback: sprocess })
// *   pool.work_on_items(['a','b','c','d','e','f','g'])
// *   // optionally, you can iterate over the results too:
// *   for x in pool.get_results<SResult>() {
// *       println('result: $x.s')
// *   }
// *
// * See https://github.com/vlang/v/blob/master/vlib/sync/pool_test.v for a
// * more detailed usage example.
// *
// * After all the work is done in parallel by the worker threads in the pool,
// * pool.work_on_items will return, and you can then call
// * pool.get_results<Result>() to retrieve a list of all the results,
// * that the worker callbacks returned for each item that you passed.
// * The parameters of new_pool_processor are:
// *   context.maxjobs: when 0 (the default), the PoolProcessor will use an
// *       optimal for your system number of threads to process your items
// *   context.callback: this should be a callback function, that each worker
// *       thread in the pool will run for each item.
// *       The callback function will receive as parameters:
// *       1) the PoolProcessor instance, so it can call
// *             p.get_item<int>(idx) to get the actual item at index idx
// *             NB: for now, you are better off calling p.get_string_item(idx)
// *                 or p.get_int_item(idx) ; TODO: vfmt and generics
// *       2) idx - the index of the currently processed item
// *       3) task_id - the index of the worker thread in which the callback
// *             function is running.

pub const (
	no_result = voidptr(0)
)

pub struct PoolProcessor {
	thread_cb       voidptr
mut:
	njobs           int
	items           []voidptr
	results         []voidptr
	ntask           int // writing to this should be locked by ntask_mtx.
	ntask_mtx       &Mutex
	waitgroup       &WaitGroup
	shared_context  voidptr
	thread_contexts []voidptr
}

pub type ThreadCB = fn (p &PoolProcessor, idx int, task_id int) voidptr

pub struct PoolProcessorConfig {
	maxjobs  int
	callback ThreadCB
}

// new_pool_processor returns a new PoolProcessor instance.
pub fn new_pool_processor(context PoolProcessorConfig) &PoolProcessor {
	if isnil(context.callback) {
		panic('You need to pass a valid callback to new_pool_processor.')
	}
	// TODO: remove this call.
	// It prevents a V warning about unused module runtime.
	runtime.nr_jobs()
	pool := &PoolProcessor {
		items: []
		results: []
		shared_context: voidptr(0)
		thread_contexts: []
		njobs: context.maxjobs
		ntask: 0
		ntask_mtx: new_mutex()
		waitgroup: new_waitgroup()
		thread_cb: voidptr(context.callback)
	}
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
// TODO: uncomment, when generics work again
//pub fn (mut pool PoolProcessor) work_on_items<T>(items []T) {
//	pool.work_on_pointers( items.pointers() )
//}

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
	mut idx := 0
	ilen := pool.items.len
	for {
		if pool.ntask >= ilen {
			break
		}
		pool.ntask_mtx.m_lock()
		idx = pool.ntask
		pool.ntask++
		pool.ntask_mtx.unlock()
		if idx >= ilen {
			break
		}
		pool.results[idx] = cb(pool, idx, task_id)
	}
	pool.waitgroup.done()
}

// get_item - called by the worker callback.
// Retrieves a type safe instance of the currently processed item
// TODO: uncomment, when generics work again
//pub fn (pool &PoolProcessor) get_item<T>(idx int) T {
//	return *(&T(pool.items[idx]))
//}

// get_string_item - called by the worker callback.
// It does not use generics so it does not mess up vfmt.
// TODO: remove the need for this when vfmt becomes smarter.
pub fn (pool &PoolProcessor) get_string_item(idx int) string {
   // return *(&string(pool.items[idx]))
   // TODO: the below is a hack, remove it when v2 casting works again
   return *unsafe {&string( pool.items[idx] )}
}

// get_int_item - called by the worker callback.
// It does not use generics so it does not mess up vfmt.
// TODO: remove the need for this when vfmt becomes smarter.
pub fn (pool &PoolProcessor) get_int_item(idx int) int {
	item := pool.items[idx]
	return *unsafe {&int(item)}
}

// TODO: uncomment, when generics work again
//pub fn (pool &PoolProcessor) get_result<T>(idx int) T {
//	return *(&T(pool.results[idx]))
//}

// TODO: uncomment, when generics work again
// get_results - can be called to get a list of type safe results.
//pub fn (pool &PoolProcessor) get_results<T>() []T {
//	mut res := []T{}
//	for i in 0 .. pool.results.len {
//		res << *(&T(pool.results[i]))
//	}
//	return res
//}

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

// TODO: remove everything below this line after generics are fixed:
pub struct SResult {
pub:
	s string
}
pub struct IResult {
pub:
	i int
}

//

pub fn (mut pool PoolProcessor) work_on_items_s(items []string) {
	pool.work_on_pointers( items.pointers() )
}

pub fn (mut pool PoolProcessor) work_on_items_i(items []int) {
	pool.work_on_pointers( items.pointers() )
}

pub fn (pool &PoolProcessor) get_results_s() []SResult {
	mut res := []SResult{}
	for i in 0 .. pool.results.len {
		res << *unsafe {&SResult(pool.results[i])}
	}
	return res
}
pub fn (pool &PoolProcessor) get_results_i() []IResult {
	mut res := []IResult{}
	for i in 0 .. pool.results.len {
		res << *unsafe {&IResult(pool.results[i])}
	}
	return res
}
