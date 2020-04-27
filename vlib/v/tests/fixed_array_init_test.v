fn test_fixed_array_init() {
	a1 := ['1', '2', '3']!!
	assert typeof(a1) == '[3]string'
	assert '$a1' == "['1', '2', '3']"

	a2 := ['a', 'b']!!
	assert typeof(a2) == '[2]string'
	assert '$a2' == "['a', 'b']"

	c1 := [1, 2, 3]!!
	assert typeof(c1) == '[3]int'
	assert '$c1' == '[1, 2, 3]'

	c2 := [i16(1), 2, 3]!!
	assert typeof(c2) == '[3]i16'
	assert '$c2' == '[1, 2, 3]'

	mut c3 := [i64(1), 2, 3]!!
	assert typeof(c3) == '[3]i64'
	assert '$c3' == '[1, 2, 3]'

	mut c4 := [u64(1), 2, 3]!!
	assert typeof(c4) == '[3]u64'
	assert '$c4' == '[1, 2, 3]'

	mut d1 := [1.1, 2.2, 3.3]!!
	assert typeof(d1) == '[3]f64'
	assert '$d1' == '[1.1, 2.2, 3.3]'

	mut d2 := [f32(1.1), 2.2, 3.3]!!
	assert typeof(d2) == '[3]f32'
	assert '$d2' == '[1.1, 2.2, 3.3]'
}
