fn prompt_enum[T](s string) !T {
	if x := T.from(s) {
		return x
	}
	return error('fail')
}

enum EnumA {
	a
	b
	c
}

enum EnumB {
	x
	y
	z
}

fn test_main() {
	assert prompt_enum[EnumA]('a')! == EnumA.a
	assert prompt_enum[EnumB]('x')! == EnumB.x
}
