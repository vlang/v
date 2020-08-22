fn array_mut_slice(mut a []int) {
	assert a[1..3].map(it) == [3, 5]
}

fn test_array_mut_slice() {
	mut a := [1, 3, 5, 7, 9]
	array_mut_slice(mut a)
}
