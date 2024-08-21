module arrays

import rand
import time

fn test_run_parallel() {
	count := []int{len: 10, init: index}
	mut res := []string{len: 10}
	run_parallel[int](count, 2, fn [mut res] (i int)! {
		delay := rand.intn(1000) or { 1000 }
		time.sleep(delay * time.millisecond)
		res << 'task ${i}, delay=${delay}ms'
	})
	assert res.len == count.len
}

// todo: rename the function once the implementation is complete
fn test_map_parallel() {
	input := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	output := map_parallel[int, int](input, 4, fn (i int) int {
		delay := rand.intn(1000) or { 1000 }
		time.sleep(delay * time.millisecond)
		return i * i
	})

	// for i, _ in output {
	// 	assert output[i] == input[i] * input[i]
	// }

	// unordered output validation
	assert output.len == input.len
	op_sorted := output.sorted()
	for i, op in op_sorted {
		assert op == input[i] * input[i]
	}
}
