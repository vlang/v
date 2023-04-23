fn async_map[T](arr []T, func fn (T) T) []T {
	mut threads := []thread T{}
	for element in arr {
		threads << spawn func(element)
	}
	return threads.wait()
}

fn test_generic_array_of_threads() {
	arr := [1, 2, 3, 4]
	results := async_map(arr, fn (a int) int {
		return -a
	})
	println(results)
	assert results == [-1, -2, -3, -4]
}
