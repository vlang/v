module main

fn test_cast_to_alias() {
	r1 := u8(byte(1))
	println(r1)
	assert '$r1' == '1'

	r2 := u8(byte(true))
	println(r2)
	assert '$r2' == '1'

	r3 := u8(true)
	println(r3)
	assert '$r3' == '1'
}
