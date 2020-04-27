fn test_array_of_floats() {
	// f64 array
	aa := [1.2, 3.4, 5.67]
	assert aa.str() == '[1.2, 3.4, 5.67]'
	assert '$aa' == '[1.2, 3.4, 5.67]'
	// f32 array
	bb := [f32(1.2), 3.4, 5.67]
	assert bb.str() == '[1.2, 3.4, 5.67]'
	assert '$bb' == '[1.2, 3.4, 5.67]'
}

fn test_array_of_bools() {
	aa := [true, false, true]
	assert aa.str() == '[true, false, true]'
	assert '$aa' == '[true, false, true]'
}

fn test_array_of_ints() {
	// int
	a1 := [11, 22, 33]
	assert a1.str() == '[11, 22, 33]'
	assert '$a1' == '[11, 22, 33]'
	// u32
	a2 := [u32(11), 22, 33]
	assert a2.str() == '[11, 22, 33]'
	assert '$a2' == '[11, 22, 33]'
	// i16
	b1 := [i16(11), 22, 33]
	assert b1.str() == '[11, 22, 33]'
	assert '$b1' == '[11, 22, 33]'
	// u16
	b2 := [u16(11), 22, 33]
	assert b2.str() == '[11, 22, 33]'
	assert '$b2' == '[11, 22, 33]'
	// i64
	c1 := [i64(11), 22, 33]
	assert c1.str() == '[11, 22, 33]'
	assert '$c1' == '[11, 22, 33]'
	// u64
	c2 := [u64(11), 22, 33]
	assert c2.str() == '[11, 22, 33]'
	assert '$c2' == '[11, 22, 33]'
}

fn test_array_of_bytes() {
	aa := [`a`, `b`, `c`]
	assert aa.str() == '[a, b, c]'
	assert '$aa' == '[a, b, c]'
}

fn test_array_of_strings() {
	aa := ['aa', 'bb', 'cc']
	assert aa.str() == "['aa', 'bb', 'cc']"
	assert '$aa' == "['aa', 'bb', 'cc']"
}
