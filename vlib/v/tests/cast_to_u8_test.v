module main

fn test_cast_to_alias() {
	r1 := u8(u8(1))
	println(r1)
	assert '${r1}' == '1'

	r2 := u8(u8(true))
	println(r2)
	assert '${r2}' == '1'

	r3 := u8(true)
	println(r3)
	assert '${r3}' == '1'

	r4 := u8(char(1))
	println(r4)
	assert r4 == 1

	r5 := u8(char(-1))
	println(r5)
	assert '${r5}' == '255'

	r6 := u8(i8(-1))
	println(r6)
	assert r6 == 255
}
