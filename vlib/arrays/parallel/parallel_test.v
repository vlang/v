// vtest build: !musl?
import arrays.parallel
import rand
import time

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
	}, workers: 1000) == []
	assert true
}

fn test_parallel_run() {
	counters := []int{len: 10, init: index}
	dump(counters)
	mut res := []string{len: 10}
	mut pres := &res
	parallel.run(counters, fn [mut pres] (i int) {
		delay := rand.intn(250) or { 250 }
		time.sleep(delay * time.millisecond)
		unsafe {
			pres[i] = 'task ${i}, delay=${delay}ms'
		}
		assert true
	})
	dump(res)
	assert res.len == counters.len
}

fn test_parallel_amap() {
	input := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	dump(input)
	dump(input.len)
	output := parallel.amap(input, fn (i int) int {
		delay := rand.intn(250) or { 250 }
		time.sleep(delay * time.millisecond)
		return i * i
	})
	dump(output)
	dump(output.len)
	assert input.len == output.len

	for i, _ in output {
		assert output[i] == input[i] * input[i]
	}

	// unordered output validation
	assert output.len == input.len
	op_sorted := output.sorted()
	dump(op_sorted)
	for i, op in op_sorted {
		assert op == input[i] * input[i]
	}
}
