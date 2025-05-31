fn test_main() {
	a := 'abcd'
	b := a[0].str
	println(typeof(b).name)
	assert b() == u8(`a`).str()

	c := [1]
	d := c[0].str
	println(typeof(d).name)
	assert d() == '1'
}
