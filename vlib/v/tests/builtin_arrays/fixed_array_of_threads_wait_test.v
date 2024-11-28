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
