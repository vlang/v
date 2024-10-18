fn test_main() {
	mut a := [][2]int{}
	a << [0, 0]!
	println([0, 0]! in a)
	ret := [0, 0]! in a
	println(ret)
	assert ret
}
