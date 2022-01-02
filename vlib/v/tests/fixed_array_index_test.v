fn test_fixed_array_index() {
	aa := [byte(0), 0xFF, 0xFF]
	mut bufa := [8]byte{}
	copy(bufa[..], aa)
	println(bufa)
	assert bufa == [byte(0), 0xFF, 0xFF, 0, 0, 0, 0, 0]!

	mut bufb := []byte{len: 8}
	copy(bufb[..], aa)
	println(bufb)
	assert bufb == [byte(0), 0xFF, 0xFF, 0, 0, 0, 0, 0]
}
