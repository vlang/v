fn test_main() {
	x := []?int{len: 5, init: 1}
	assert x[0] == x[1]

	y := []?int{len: 4, init: 2}
	assert !(x[0] == y[1])
}
