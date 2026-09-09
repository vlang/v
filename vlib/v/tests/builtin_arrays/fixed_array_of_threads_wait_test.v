fn foo() bool {
	return true
}

fn test_fixed_array_of_threads_wait() {
	threads := [
		spawn foo(),
		spawn foo(),
	]!

	results := threads.wait()

	println(results)
	assert results == [true, true]
}

fn fixed_ok(n int) !int {
	if n < 0 {
		return error('fixed boom')
	}
	return n
}

fn test_fixed_array_of_threads_wait_result() {
	threads := [
		spawn fixed_ok(1),
		spawn fixed_ok(2),
	]!

	results := threads.wait() or { []int{} }

	assert results == [1, 2]
}
