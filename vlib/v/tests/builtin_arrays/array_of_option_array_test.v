fn test_array_of_option_array() {
	mut arr := []?[]int{}

	arr1 := []int{}
	arr << arr1

	arr2 := [1, 2, 3]
	arr << arr2

	arr << none

	println(arr)

	for i, item in arr {
		if arr_item := item {
			if i == 0 {
				assert arr_item == []
			} else if i == 1 {
				assert arr_item == [1, 2, 3]
			}
		} else {
			assert item == none
		}
	}

	assert arr.len == 3
	assert '${arr[0]}' == 'Option([])'
	assert '${arr[1]}' == 'Option([1, 2, 3])'
	assert '${arr[2]}' == 'Option(none)'
}
