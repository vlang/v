fn test_main() {
	mut space := [2][2][]int{}
	space[1][1] << 123
	space[0][0] << 321
	assert space[1][1] == [123]
	assert space[0][0] == [321]
}
