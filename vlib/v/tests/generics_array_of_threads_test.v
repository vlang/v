fn async_map[T](arr []T, func fn (T) T) []T {
	mut threads := []thread T{}
	for element in arr {
		threads << spawn func(element)
	}
	return threads.wait()
}

fn test_generic_array_of_threads() {
	arr1 := [1, 2, 3, 4]
	results1 := async_map(arr1, fn (a int) int {
		return -a
	})
	println(results1)
	assert results1 == [-1, -2, -3, -4]

	arr2 := [1.0, 2.0, 3.0, 4.0]
	results2 := async_map(arr2, fn (a f64) f64 {
		return -a
	})
	println(results2)
	assert results2 == [-1.0, -2.0, -3.0, -4.0]
}
