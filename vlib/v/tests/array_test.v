fn test_for_in_array_named_array() {
	mut array := [1]
	for elem in array {
		assert elem == 1
	}
	for mut elem in array {
		assert *elem == 1
		elem = 2
		assert *elem == 2
	}
}

fn test_for_in_shared_array_named_array() {
	shared array := &[1]
	rlock array {
		for elem in array {
			assert elem == 1
		}
	}
}

fn test_fixed_array_to_dynamic_array() {
	y := [1, 2, 3]!
	mut x := y[..]
	x << 4
	assert x.len == 4
}

fn test_append_array_used_as_fn_param() {
	mut arr1 := [][]string{}
	mut arr2 := [][]string{}
	mut arr3 := []string{}
	arr4 := []string{}
	arr1 << arr2
	arr1 << arr3
	arr3 << arr4
	append_2d_2d(mut arr1, arr2)
	append_2d_1d(mut arr1, arr3)
	append_1d_1d(mut arr3, arr4)
	assert true
}

fn append_2d_2d(mut arr1 [][]string, arr2 [][]string) {
	arr1 << arr2
}

fn append_2d_1d(mut arr1 [][]string, arr2 []string) {
	arr1 << arr2
}

fn append_1d_1d(mut arr1 []string, arr2 []string) {
	arr1 << arr2
}
