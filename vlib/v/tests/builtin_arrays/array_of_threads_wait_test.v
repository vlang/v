import math { floor, sqrt }

fn async(arr_size int, init_val f64) f64 {
	mut val_arr := []f64{}

	for _ in 1 .. arr_size {
		val_arr << floor((sqrt(init_val) / 2) * 3)
	}

	return sum(val_arr.map(it / 2))
}

fn sum(val_arr []f64) f64 {
	mut result := f64(0)

	for val in val_arr {
		result += val
	}

	return result
}

fn test_array_of_threads_wait() {
	size := 2000_000
	init_val := 123456
	println('Async')
	mut results := []thread f64{len: 16, cap: 16}
	for num in 0 .. 15 {
		results << spawn async(size, init_val + num)
	}
	waited_results := results.wait()

	println(waited_results)
	println(sum(waited_results))
	assert sum(waited_results) == 7.9049960475e+09
}
