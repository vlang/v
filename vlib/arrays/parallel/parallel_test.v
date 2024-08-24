import arrays.parallel
import rand
import time

fn test_parallel_run() {
	count := []int{len: 10, init: index}
	mut res := []string{len: 10}
	parallel.run[int](count, 2, fn [mut res] (i int) {
		delay := rand.intn(250) or { 250 }
		time.sleep(delay * time.millisecond)
		res << 'task ${i}, delay=${delay}ms'
	})
	dump(res)
	assert res.len == count.len
}

// todo: rename the function once the implementation is complete
fn test_parallel_amap() {
	input := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	output := parallel.amap[int, int](input, 4, fn (i int) int {
		delay := rand.intn(250) or { 250 }
		time.sleep(delay * time.millisecond)
		return i * i
	})
	dump(output)

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
