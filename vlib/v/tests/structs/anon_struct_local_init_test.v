fn test_anon_struct_as_local_variable() {
	mut s := struct {
		foo string
		bar int
	}{}
	assert s.foo == ''
	assert s.bar == 0
	s.foo = 'foo'
	s.bar = 1
	assert s.foo == 'foo'
	assert s.bar == 1
}
