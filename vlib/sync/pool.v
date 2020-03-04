module sync

// Goal: this file provides a convenient way to run identical tasks over a list
// of items in parallel, without worrying about waitgroups, mutexes and so on.
//
// Usage example:
//    pool := sync.new_pool_processor({ callback: worker_cb })
//    pool.work_on_items<string>(['a','b','c'])
//    // optionally, you can iterate over the results too:
//    for x in pool.get_results<IResult>() {
//        // do stuff with x
//    }
//
// See https://github.com/vlang/v/blob/master/vlib/sync/pool_test.v for a
// more detailed usage example.
//
// After all the work is done in parallel by the worker threads in the pool,
// pool.work_on_items will return, and you can then call
// pool.get_results<Result>() to retrieve a list of all the results,
// that the worker callbacks returned for each item that you passed.
// The parameters of new_pool_processor are:
//    context.maxjobs: when 0 (the default), the PoolProcessor will use an
//        optimal for your system number of threads to process your items
//    context.callback: this should be a callback function, that each worker
//        thread in the pool will run for each item.
//        The callback function will receive as parameters:
//        1) the PoolProcessor instance, so it can call
//              p.get_item<int>(idx) to get the actual item at index idx
//        2) idx - the index of the currently processed item
//        3) task_id - the index of the worker thread in which the callback
//              function is running.

import runtime

pub const (
	no_result = voidptr(0)
)
pub struct PoolProcessor {
	thread_cb voidptr
mut:
	njobs     int
	items     []voidptr
	results   []voidptr
	ntask     int // writing to this should be locked by ntask_mtx.
	ntask_mtx &sync.Mutex
	waitgroup &sync.WaitGroup
}

pub type ThreadCB fn(p &PoolProcessor, idx int, task_id int) voidptr

pub struct PoolProcessorConfig {
  maxjobs int
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
	pool := &PoolProcessor{
		items: []
		results: []
		njobs: context.maxjobs,
		ntask: 0,
		ntask_mtx: sync.new_mutex(),
		waitgroup: sync.new_waitgroup(),
		thread_cb: context.callback
	}
	return pool
}

// set_max_jobs gives you the ability to override the number
// of jobs *after* the PoolProcessor had been created already.
pub fn (pool mut PoolProcessor) set_max_jobs(njobs int){
	pool.njobs = njobs
}

// work_on_items receives a list of items of type T, then
// starts a work pool of pool.njobs threads, each running
// pool.thread_cb in a loop, untill all items in the list,
// are processed.
// When pool.njobs is 0, the number of jobs is determined
// by the number of available cores on the system.
// work_on_items returns *after* all threads finish.
// You can optionally call get_results after that.
pub fn (pool mut PoolProcessor) work_on_items<T>(items []T){
	mut njobs := runtime.nr_jobs()
	if pool.njobs > 0 {
		njobs = pool.njobs
	}
	pool.items = []
	pool.results = []
	for i in 0..items.len-1{
		pool.items << items.data + i*sizeof(T)
	}
	pool.results = [voidptr(0)].repeat(pool.items.len)
	pool.waitgroup.add( njobs )
	for i:=0; i < njobs; i++ {
		go process_in_thread(pool, i)
	}
	pool.waitgroup.wait()
}

// process_in_thread does the actual work of worker thread.
// It is a workaround for the current inability to pass a
// method in a callback.
fn process_in_thread(pool mut PoolProcessor, task_id int){
	cb := ThreadCB( pool.thread_cb )
	for {
		pool.ntask_mtx.lock()
		pool.ntask++
		idx := pool.ntask-1
		pool.ntask_mtx.unlock()
		if idx >= pool.items.len { break }
		pool.results[ idx ] = cb( pool, idx, task_id )
	}
	pool.waitgroup.done()
}


// get_item - called by the worker callback.
// Retrieves a type safe instance of the currently processed item
pub fn (pool &PoolProcessor) get_item<T>(idx int) T {
	return *(&T( pool.items[ idx ] ))
}

pub fn (pool &PoolProcessor) get_result<T>(idx int) T {
	return *(&T( pool.results[ idx ] ))
}

// get_results - can be called to get a list of type safe results.
pub fn (pool &PoolProcessor) get_results<T>() []T {
	mut res := []T
	for i in 0..pool.results.len {
		res << *(&T( pool.results[ i ] ))
	}
	return res
}
