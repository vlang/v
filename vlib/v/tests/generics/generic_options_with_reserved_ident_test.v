pub fn f[T](defaults ?T) T {
	default := defaults or { T{} }
	dump(default)
	return default
}

fn test_generic_options_with_reserved_ident() {
	ret1 := f(123)
	println(ret1)
	assert ret1 == 123

	ret2 := f('hello')
	println(ret2)
	assert ret2 == 'hello'
}
