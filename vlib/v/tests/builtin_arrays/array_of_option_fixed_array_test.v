module builtin_arrays

fn test_array_of_option_fixed_array() {
	mut arr := []?[3]u8{}

	fixed_arr1 := [3]u8{}
	arr << fixed_arr1

	fixed_arr2 := [u8(1), 2, 3]!
	arr << fixed_arr2

	arr << none

	println(arr)

	for i, item in arr {
		if arr1 := item {
			if i == 0 {
				assert arr1 == [3]u8{}
			} else if i == 1 {
				assert arr1 == [u8(1), 2, 3]!
			}
		} else {
			assert item == none
		}
	}

	assert arr.len == 3
	assert '${arr[0]}' == 'Option([0, 0, 0])'
	assert '${arr[1]}' == 'Option([1, 2, 3])'
	assert '${arr[2]}' == 'Option(none)'
}
