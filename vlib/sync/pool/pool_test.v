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

fn worker_s(mut p pool.PoolProcessor, idx int, _ int) &SResult {
	item := p.get_item[string](idx)
	return &SResult{'${item} ${item}'}
}

fn worker_i(mut p pool.PoolProcessor, idx int, _ int) &IResult {
	item := p.get_item[int](idx)
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

	items := ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']
	expected := items.map('${it} ${it}')
	pool_s.work_on_items(items)
	assert pool_s.get_results[SResult]().map(it.s) == expected
	assert pool_s.get_results_ref[SResult]().map(it.s) == expected
}

fn test_work_on_ints() {
	// Note: since maxjobs is left empty here,
	// the pool processor will use njobs = runtime.nr_jobs so that
	// it will work optimally without overloading the system
	mut pool_i := pool.new_pool_processor(
		callback: worker_i
	)

	items := [1, 2, 3, 4, 5, 6, 7, 8]
	expected := items.map(it * 1000)
	pool_i.work_on_items(items)
	assert pool_i.get_results[IResult]().map(it.i) == expected
	assert pool_i.get_results_ref[IResult]().map(it.i) == expected
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
