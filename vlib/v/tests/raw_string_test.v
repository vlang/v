fn test_raw_string_backslash() {
	assert r'\' == r'\'
}

fn test_raw_string_not_escaped_by_transformer() {
	assert r'a\nb' + r'a\nb' == r'a\nba\nb'
	assert 'a\nb' + r'a\nb' == 'a\nba\\nb'
}

fn test_many_pluses() {
	a := r'x\n'
	b := 'x\n'
	c := a + b
	d := c + c
	e := d + d
	result := r'x\nx
x\nx
x\nx
x\nx
'
	assert e == result
}
