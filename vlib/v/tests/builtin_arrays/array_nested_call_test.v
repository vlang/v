fn test_array_nested_call() {
	arr := ['abc', 'def']
	all_is_letter := arr.all(it.bytes().all(it.is_letter()))
	println(all_is_letter)
	assert all_is_letter
}
