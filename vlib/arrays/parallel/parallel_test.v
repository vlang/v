// vtest build: !musl?
import arrays.parallel

$if !native ? {
	import time
}

fn test_parallel_run_with_empty_arrays() {
	parallel.run([]int{}, fn (x int) {})
	parallel.run([]u8{}, fn (x u8) {})
	parallel.run([]u32{}, fn (x u32) {}, workers: 1000)
	assert true
}

fn test_parallel_amap_with_empty_arrays() {
	assert parallel.amap([]int{}, fn (x int) u8 {
		return 0
	}) == []
	assert parallel.amap([]u8{}, fn (x u8) int {
		return 0
	}) == []
	assert parallel.amap([]u8{}, fn (x u8) int {
		return 0
	},
		workers: 1000
	) == []
	assert true
}

fn test_parallel_run() {
	counters := []int{len: 10, init: index}
	mut calls := []int{len: counters.len}
	mut counts := &calls
	parallel.run(counters, fn [mut counts] (i int) {
		// Each input has its own slot in the shared result array.
		unsafe {
			counts[i]++
		}
	})
	assert calls == []int{len: counters.len, init: 1}
}

fn test_parallel_amap() {
	input := [9, 1, 4, 2, 7, 3, 6, 5, 8]
	output := $if native ? {
		parallel.amap(input, fn (i int) int {
			return i * i
		})
	} $else {
		first := input[0]
		third := input[2]
		third_started := chan bool{cap: 1}
		// Hold the first task until the other worker has finished the second.
		parallel.amap(input, fn [first, third, third_started] (i int) int {
			if i == first {
				select {
					_ := <-third_started {
					}
					2 * time.second {
						return -1
					}
				}
			}
			if i == third {
				third_started <- true
			}
			return i * i
		}, workers: 2)
	}
	assert output == input.map(it * it)
}
