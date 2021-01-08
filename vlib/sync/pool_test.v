import sync
import time

fn worker_s(p &sync.PoolProcessor, idx int, worker_id int) voidptr {
	// TODO: this works, but confuses vfmt. It should be used instead of
	// p.get_int_item when vfmt becomes smarter.
	// item := p.get_item<string>(idx)
	item := p.get_string_item(idx)
	println('worker_s worker_id: $worker_id | idx: $idx | item: ${item}')
	time.sleep_ms(3)
	return voidptr( &sync.SResult{ '${item} ${item}' } )
}

fn worker_i(p &sync.PoolProcessor, idx int, worker_id int) voidptr {
	// TODO: this works, but confuses vfmt. See the comment above.
	// item := p.get_item<int>(idx)
	item := p.get_int_item(idx)
	println('worker_i worker_id: $worker_id | idx: $idx | item: ${item}')
	time.sleep_ms(5)
	return voidptr( &sync.IResult{ item * 1000 } )
}

fn test_work_on_strings() {
	mut pool_s := sync.new_pool_processor({
		callback: worker_s
		maxjobs: 8
	})

	// TODO: uncomment this when generics work again
	//pool_s.work_on_items(['a','b','c','d','e','f','g','h','i','j'])
	//for x in pool_s.get_results<SResult>() {
	//	println( x.s )
	//	assert x.s.len > 1
	//}
	   
	pool_s.work_on_items_s(['a','b','c','d','e','f','g','h','i','j'])
	for x in pool_s.get_results_s() {
		println( x.s )
		assert x.s.len > 1
	}
}

fn test_work_on_ints() {
	// NB: since maxjobs is left empty here,
	// the pool processor will use njobs = runtime.nr_jobs so that
	// it will work optimally without overloading the system
	mut pool_i := sync.new_pool_processor({
		callback: worker_i
	})
	   
	// TODO: uncomment this when generics work again
	//pool_i.work_on_items([1,2,3,4,5,6,7,8])
	//for x in pool_i.get_results<IResult>() {
	//	println( x.i )
	//	assert x.i > 100
	//}
	   
	pool_i.work_on_items_i([1,2,3,4,5,6,7,8])
	for x in pool_i.get_results_i() {
		println( x.i )
		assert x.i > 100
	}
}
