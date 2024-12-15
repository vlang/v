fn test_none_init() {
	a := ?[3]u8(none)
	println(a)
	assert a == none

	b := [3]u8{}
	println(b)
	assert b == none

	c := [3]u8{}
	println(c)
	assert c == none
}
