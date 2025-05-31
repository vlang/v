const ca = r'x\n'

const cb = 'x\n'

const cc = ca + cb

const cd = cc + cc

const ce = cd + cd

fn test_many_pluses_with_raw_string_literal() {
	a := r'x\n'
	assert a == ca
	b := 'x\n'
	assert b == cb
	c := a + b
	assert c == cc
	d := c + c
	assert d == cd
	e := d + d
	assert e == ce
	println(e)
	result := r'x\nx
x\nx
x\nx
x\nx
'
	assert e == result
}
