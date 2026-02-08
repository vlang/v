fn ret[T](str string) !T {
	val := $if T is i8 {
		T(123)
	} $else {
		T(123)
	}
	return val
}

fn test_main() {
	x := ret[f32]('1.0')!
	assert x == 123.0
}
