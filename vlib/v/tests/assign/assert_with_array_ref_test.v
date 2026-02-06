fn test_main() {
	myu16 := u16(50)
	size_of_u16 := sizeof(u16)

	mut asu8 := unsafe { &u8(malloc(size_of_u16)) }

	asu8 = &[myu16]! // look this

	$if little_endian {
		assert unsafe { asu8.vbytes(int(size_of_u16)) } == [u8(50), 0]
	} $else {
		assert unsafe { asu8.vbytes(int(size_of_u16)) } == [u8(0), 50]
	}
}
