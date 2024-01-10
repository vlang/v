fn test_array_slice_assign() {
	xs := [1, 2, 3, 4, 5, 6, 7, 8]

	mut s := xs[1..].clone()

	s.sort(a > b)

	println(s)
	assert s == [8, 7, 6, 5, 4, 3, 2]
	println(xs)
	assert xs == [1, 2, 3, 4, 5, 6, 7, 8]
}
