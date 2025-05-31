fn test_swap_array() {
	mut array1 := []int{len: 10, init: 1}

	println(array1)
	swaper(mut array1)
	println(array1)

	assert array1 == [22, 2, 2, 2, 2, 2, 2, 2, 2, 2]
}

fn swaper(mut array1 []int) {
	mut array2 := []int{len: 10, init: 2}
	array1[0] = 11
	array2[0] = 22

	unsafe {
		array1, array2 = array2, array1
	}
}
