import math { floor, sqrt }
import arrays { sum }

fn async(arr_size int, init_val f64) f64 {
	mut val_arr := []f64{}
	for _ in 1 .. arr_size {
		val_arr << floor((sqrt(init_val) / 2) * 3)
	}

	return sum(val_arr.map(it / 2)) or { f64(1) }
}

fn test_comptime_if_expr_of_threads() {
	size := 2000_000

	println('Async')
	mut results := []thread f64{cap: 16}
	for num in 0 .. 15 {
		results << spawn async(size, num)
	}
	waited_results := results.wait()

	println(waited_results)
	sum_result := sum(waited_results) or { 1 }
	println(sum_result)
	assert sum_result == 4.7999976e+07
}
