fn test_fixed_array_lit_init() {
	a1 := ['1', '2', '3']!
	assert typeof(a1).name == '[3]string'
	assert '$a1' == "['1', '2', '3']"
	a2 := ['a', 'b']!
	assert typeof(a2).name == '[2]string'
	assert '$a2' == "['a', 'b']"
	c1 := [1, 2, 3]!
	assert typeof(c1).name == '[3]int'
	assert '$c1' == '[1, 2, 3]'
	c2 := [i16(1), 2, 3]!
	assert typeof(c2).name == '[3]i16'
	assert '$c2' == '[1, 2, 3]'
	mut c3 := [i64(1), 2, 3]!
	assert typeof(c3).name == '[3]i64'
	assert '$c3' == '[1, 2, 3]'
	mut c4 := [u64(1), 2, 3]!
	assert typeof(c4).name == '[3]u64'
	assert '$c4' == '[1, 2, 3]'
	mut d1 := [1.1, 2.2, 3.3]!
	assert typeof(d1).name == '[3]f64'
	assert '$d1' == '[1.1, 2.2, 3.3]'
	mut d2 := [f32(1.1), 2.2, 3.3]!
	assert typeof(d2).name == '[3]f32'
	assert '$d2' == '[1.1, 2.2, 3.3]'
}

fn test_fixed_type_init() {
	a := [2]int{}
	assert a == [2]int{}
	assert a == [0, 0]!
	assert a == a
	mut c := [3, 3]!
	assert a != c
	assert c == [3, 3]!
	c = [2]int{}
	assert a == c
}

fn test_fixed_custom_init() {
	a := [2]byte{init: 7}
	assert a == [byte(7), 7]!
	mut b := [3]int{}
	assert b == [0, 0, 0]!
	// assign
	b = [3]int{init: 5}
	assert b == [5, 5, 5]!
}
