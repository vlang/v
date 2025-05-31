const ca = r'x\n'

const cb = 'x\n'

const cc = ca + cb

const cd = cc + cc

const ce = cd + cd

fn test_raw_string_backslash() {
	assert r'\' == r'\'
}

fn test_raw_string_not_escaped_by_transformer() {
	assert r'a\nb' + r'a\nb' == r'a\nba\nb'
	assert 'a\nb' + r'a\nb' == 'a\nba\\nb'
}

fn test_raw_string_backslash_u() {
	assert r'\u0000004B' == r'\u0000004B'
	println(r'\u0000004B')
}

// this test will cause test failure (see #12604)
// fn test_many_pluses() {
// 	a := r'x\n'
// 	assert a == ca
// 	b := 'x\n'
// 	assert b == cb
// 	c := a + b
// 	assert c == cc // this fails
// 	d := c + c
// 	assert d == cd
// 	e := d + d
// 	assert e == ce
// 	println(e)
// 	result := r'x\nx
// x\nx
// x\nx
// x\nx
// '
// 	assert e == result
// }
