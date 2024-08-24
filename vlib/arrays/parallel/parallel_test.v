import arrays.parallel
import rand
import time
import runtime

fn test_parallel_run() {
	count := []int{len: 10, init: index}
	mut res := []string{len: 10}
	mut pres := &res
	parallel.run[int](count, runtime.nr_jobs(), fn [mut pres] (i int) {
		delay := rand.intn(250) or { 250 }
		time.sleep(delay * time.millisecond)
		unsafe {
			pres[i] = 'task ${i}, delay=${delay}ms'
		}
	})
	dump(res)
	assert res.len == count.len
}

// todo: rename the function once the implementation is complete
fn test_parallel_amap() {
	input := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	dump(input)
	dump(input.len)
	output := parallel.amap[int, int](input, runtime.nr_jobs(), fn (i int) int {
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
