import time
import sync
import sync.pool

pub struct SResult {
	s string
}

pub struct IResult {
	i int
}

struct SeenContext {
mut:
	mutex &sync.Mutex = sync.new_mutex()
	seen  []int
}

fn worker_s(mut p pool.PoolProcessor, idx int, worker_id int) &SResult {
	item := p.get_item[string](idx)
	println('worker_s worker_id: ${worker_id} | idx: ${idx} | item: ${item}')
	time.sleep(3 * time.millisecond)
	return &SResult{'${item} ${item}'}
}

fn worker_i(mut p pool.PoolProcessor, idx int, worker_id int) &IResult {
	item := p.get_item[int](idx)
	println('worker_i worker_id: ${worker_id} | idx: ${idx} | item: ${item}')
	time.sleep(5 * time.millisecond)
	return &IResult{item * 1000}
}

fn worker_reuse(mut p pool.PoolProcessor, idx int, _ int) voidptr {
	item := p.get_item[int](idx)
	mut ctx := unsafe { &SeenContext(p.get_shared_context()) }
	ctx.mutex.lock()
	ctx.seen << item
	ctx.mutex.unlock()
	return pool.no_result
}

fn test_work_on_strings() {
	mut pool_s := pool.new_pool_processor(
		callback: worker_s
		maxjobs:  8
	)

	pool_s.work_on_items(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'])
	for x in pool_s.get_results[SResult]() {
		println(x.s)
		assert x.s.len > 1
	}
	println('---------- pool_s.get_results_ref: --------------')
	for x in pool_s.get_results_ref[SResult]() {
		println(x.s)
		assert x.s.len > 1
	}
}

fn test_work_on_ints() {
	// Note: since maxjobs is left empty here,
	// the pool processor will use njobs = runtime.nr_jobs so that
	// it will work optimally without overloading the system
	mut pool_i := pool.new_pool_processor(
		callback: worker_i
	)

	pool_i.work_on_items([1, 2, 3, 4, 5, 6, 7, 8])
	for x in pool_i.get_results[IResult]() {
		println(x.i)
		assert x.i > 100
	}
	println('---------- pool_i.get_results_ref: --------------')
	for x in pool_i.get_results_ref[IResult]() {
		println(x.i)
		assert x.i > 100
	}
}

fn test_pool_can_be_reused() {
	mut ctx := &SeenContext{}
	mut pool_i := pool.new_pool_processor(
		callback: worker_reuse
		maxjobs:  2
	)
	pool_i.set_shared_context(ctx)
	pool_i.work_on_items([1, 2, 3])
	ctx.mutex.lock()
	mut first_seen := ctx.seen.clone()
	ctx.seen = []int{}
	ctx.mutex.unlock()
	first_seen.sort()
	assert first_seen == [1, 2, 3]
	pool_i.work_on_items([4, 5])
	ctx.mutex.lock()
	mut second_seen := ctx.seen.clone()
	ctx.mutex.unlock()
	second_seen.sort()
	assert second_seen == [4, 5]
}
