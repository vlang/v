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
	return '$ret'
}

fn test_generics_with_nested_generic_method_call_assign() {
	s1 := SSS<int>{100}
	s1.outer()
	assert s1.outer() == '100'

	s2 := SSS<string>{'hello'}
	s2.outer()
	assert s2.outer() == 'hello'
}
