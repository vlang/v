struct SSS<T> {
mut:
	x T
}

fn (s SSS<T>) inner() T {
	return s.x
}

fn (s SSS<T>) outer() string {
	ret := s.inner<T>()
	println(ret)
	return '${ret}'
}

fn test_generics_with_assign_nested_generic_method_call() {
	s1 := SSS<int>{100}
	assert s1.outer() == '100'

	s2 := SSS<string>{'hello'}
	assert s2.outer() == 'hello'
}

fn fn_inner<T>(t T) T {
	return t
}

fn fn_outer<T>(t T) string {
	ret := fn_inner<T>(t)
	println(ret)
	return '${ret}'
}

fn test_generics_with_assign_nested_generic_fn_call() {
	assert fn_outer(100) == '100'
	assert fn_outer('hello') == 'hello'
}
