fn test_anon_struct_as_local_variable() {
	mut s := struct _VAnonStruct1{}
	assert s.foo == ''
	assert s.bar == 0
	s.foo = 'foo'
	s.bar = 1
	assert s.foo == 'foo'
	assert s.bar == 1
}
