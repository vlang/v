module benchmark

import time

// if n == 0, n predict == 1
fn test_predict_n_zero() {
	mut b := Benchmark{
		n:          0
		duration:   0
		bench_time: time.second
		bench_func: fn () ! {}
	}
	expected := 1
	println(b.predict_n())
	assert b.predict_n() == expected
}

// n can't be more 1000000000
fn test_predict_n_limit() {
	mut b := Benchmark{
		n:          10000000000
		duration:   0
		bench_time: time.second
		bench_func: fn () ! {}
	}
	expected := 1000000000
	assert b.predict_n() == expected
}

// test prediction for slow bench function
fn test_slow_fn() {
	mut b := Benchmark{
		duration:   time.second
		bench_func: fn () ! {}
	}
	assert b.predict_n() == 1
}

// if bench_func cause error set failed true, n = 1
fn test_fn_with_error() {
	f := fn () ! {
		return error('error')
	}
	mut bench := setup(f) or {
		eprintln('Error creating benchmark: ${err}')
		return
	}

	bench.run()

	assert bench.failed == true
	assert bench.benchmark_result.n == 1
}

fn test_n_must_be_over_1() {
	f := fn () ! {
		mut i := 0
		i++
	}
	mut bench := setup(f) or {
		eprintln('Error creating benchmark: ${err}')
		return
	}

	bench.run()

	assert bench.benchmark_result.n > 1
}

fn test_n() {
	f := fn () ! {
		mut i := 0
		i++
	}
	mut bench := setup(f, BenchmarkDefaults{
		n: 1000
	}) or {
		eprintln('Error creating benchmark: ${err}')
		return
	}

	bench.run()

	assert bench.benchmark_result.n == 1000
}

fn test_max_bench_time() {
	f := fn () ! {
		time.sleep(500 * time.millisecond)
	}
	mut bench := setup(f) or {
		eprintln('Error creating benchmark: ${err}')
		return
	}

	bench.run()

	assert bench.benchmark_result.n == 3
	assert bench.benchmark_result.t >= time.second
}

fn test_performance() {
	scheduler := [func_1, func_2, func_3]
	expected := [false, false, false]
	mut actual := []bool{}

	for i in scheduler {
		mut bench := setup(i) or {
			eprintln('Error creating benchmark: ${err}')
			return
		}

		bench.run()
		actual << bench.failed
	}

	assert expected.len == actual.len
	for i := 0; i < expected.len; i++ {
		assert expected[i] == actual[i]
	}
}

fn func_1() ! {
	mut arr := []int{}
	appender(mut arr)
	assert arr.len == 10
}

fn appender(mut arr []int) {
	if arr.len == 10 {
		return
	}
	arr << 1
	appender(mut arr)
}

fn func_2() ! {
	target := 2
	arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

	mut left := 0
	mut right := arr.len - 1

	for left <= right {
		mid := left + (right - left) / 2
		if arr[mid] == target {
			return
		}
		if arr[mid] < target {
			left = mid + 1
		}
		if arr[mid] > target {
			right = mid - 1
		}
	}
	return
}

fn func_3() ! {
	mut arr := [10, 2, 13, 4, 5, 16, 7, 1, 9, 20]

	for i := 0; i < arr.len - 1; i++ {
		for j := 0; j < arr.len - i - 1; j++ {
			if arr[j] > arr[j + 1] {
				arr[j], arr[j + 1] = arr[j + 1], arr[j]
			}
		}
	}
}
