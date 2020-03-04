import sync
import time
import rand
import arrays

struct SResult { s string }
fn worker_s(p &sync.PoolProcessor, idx int, worker_id int) voidptr {
	item := p.get_item<string>(idx)
	println('worker_s worker_id: $worker_id | idx: $idx | item: ${item}')
	time.sleep_ms( rand.next(3) )
	return &SResult{ item + item }
}

struct IResult { i int }
fn worker_i(p &sync.PoolProcessor, idx int, worker_id int) voidptr {
	item := p.get_item<int>(idx)
	println('worker_i worker_id: $worker_id | idx: $idx | item: ${item}')
	time.sleep_ms( rand.next(5) )
	return &IResult{ item * 1000 }
}

fn test_work_on_strings(){
	rand.seed(0)
	mut pool_s := sync.new_pool_processor({ callback: worker_s, maxjobs: 8 })
	pool_s.work_on_items<string>(['a','b','c','d','e','f','g','h','i','j'])
	for x in pool_s.get_results<SResult>() {
		println( x.s )
		assert x.s.len > 1
	}
}

fn test_work_on_ints(){
	rand.seed(0)
	// NB: since maxjobs is left empty here,
	// the pool processor will use njobs = runtime.nr_jobs so that
	// it will work optimally without overloading the system
	mut pool_i := sync.new_pool_processor({ callback: worker_i })
	pool_i.work_on_items<int>(arrays.range<int>(1,8))
	for x in pool_i.get_results<IResult>() {
		println( x.i )
		assert x.i > 100
	}
}
