fn test_array_slice_clone() {
	arr := [1, 2, 3, 4, 5]
	cl := arr[1..].clone()
	assert cl == [2, 3, 4, 5]
}

fn test_array_slice_clone2() {
	arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	cl := arr[1..].clone()[2..].clone()
	assert cl == [4, 5, 6, 7, 8, 9, 10]
}
