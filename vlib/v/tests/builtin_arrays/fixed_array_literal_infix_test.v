fn test_fixed_array_literal_infix() {
	mut a := [][2]int{}
	a << [0, 0]!
	println([0, 0]! in a)
	ret := [0, 0]! in a
	println(ret)
	assert ret
}
