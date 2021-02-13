
The `sync.pool` module provides a convenient way to run identical tasks over
an array of items *in parallel*, without worrying about thread synchronization,
waitgroups, mutexes etc.., you just need to supply a callback function, that
will be called once per each item in your input array.

After all the work is done in parallel by the worker threads in the pool,
pool.work_on_items will return. You can then call pool.get_results<Result>()
to retrieve a list of all the results, that the worker callbacks returned
for each input item. Example:

```v
import sync.pool

struct SResult {
	s string
}

fn sprocess(pp &pool.PoolProcessor, idx int, wid int) &SResult {
	item := pp.get_item<string>(idx)
	println('idx: $idx, wid: $wid, item: ' + item)
	return &SResult{item.reverse()}
}

fn main() {
	mut pp := pool.new_pool_processor(callback: sprocess)
	pp.work_on_items(['1abc', '2abc', '3abc', '4abc', '5abc', '6abc', '7abc'])
	// optionally, you can iterate over the results too:
	for x in pp.get_results<SResult>() {
		println('result: $x.s')
	}
}
```

See https://github.com/vlang/v/blob/master/vlib/sync/pool/pool_test.v for a
more detailed usage example.
