fn test_main() {
	a := []i64{len: 10, init: index}
	assert a == [i64(0), 1, 2, 3, 4, 5, 6, 7, 8, 9]
	b := []i32{len: 10, init: index}
	assert b == [i32(0), 1, 2, 3, 4, 5, 6, 7, 8, 9]
	c := [10]i32{init: index}
	assert c == [i32(0), 1, 2, 3, 4, 5, 6, 7, 8, 9]!
}
