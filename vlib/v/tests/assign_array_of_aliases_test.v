fn test_assign_array_of_aliases() {
	mut arr := [u8(1), 2, 3]
	arr = [byte(4), 5, 6]

	println(arr)
	assert arr.len == 3
	assert arr[0] == 4
	assert arr[1] == 5
	assert arr[2] == 6
}
