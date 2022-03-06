import time
import sync.pool

pub struct SResult {
	s string
}

pub struct IResult {
	i int
}

fn worker_s(p &pool.PoolProcessor, idx int, worker_id int) &SResult {
	item := p.get_item<string>(idx)
	println('worker_s worker_id: $worker_id | idx: $idx | item: $item')
	time.sleep(3 * time.millisecond)
	return &SResult{'$item $item'}
}

fn worker_i(p &pool.PoolProcessor, idx int, worker_id int) &IResult {
	item := p.get_item<int>(idx)
	println('worker_i worker_id: $worker_id | idx: $idx | item: $item')
	time.sleep(5 * time.millisecond)
	return &IResult{item * 1000}
}

fn test_work_on_strings() {
	mut pool_s := pool.new_pool_processor(
		callback: worker_s
		maxjobs: 8
	)

	pool_s.work_on_items(['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'])
	for x in pool_s.get_results<SResult>() {
		println(x.s)
		assert x.s.len > 1
	}
	println('---------- pool_s.get_results_ref: --------------')
	for x in pool_s.get_results_ref<SResult>() {
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
	for x in pool_i.get_results<IResult>() {
		println(x.i)
		assert x.i > 100
	}
	println('---------- pool_i.get_results_ref: --------------')
	for x in pool_i.get_results_ref<IResult>() {
		println(x.i)
		assert x.i > 100
	}
}
