fn test_array_with_it() {
	assert [0, 1, 2, 3, 4, 5]! == [6]int{init: index}
	a1 := [6]int{init: index}
	assert a1 == [0, 1, 2, 3, 4, 5]!

	assert [0, 1, 4, 9, 16, 25] == []int{len: 6, init: index * index}
	a2 := []int{len: 6, init: index * index}
	assert a2 == [0, 1, 4, 9, 16, 25]

	assert [1, 2, 3, 4, 5] == []int{len: 5, init: index + 1}
	a3 := []int{len: 5, init: index + 1}
	assert a3 == [1, 2, 3, 4, 5]

	assert [5, 4, 3, 2, 1] == []int{len: 5, init: 5 - index}
	a4 := []int{len: 5, init: 5 - index}
	assert a4 == [5, 4, 3, 2, 1]
}

fn test_array_init_with_option() {
	input := [3.1, 1.1]
	arr := []f64{len: 3, init: input[index] or { 0.0 }}
	println(arr)
	assert arr[0] == 3.1
	assert arr[1] == 1.1
	assert arr[2] == 0.0
}
