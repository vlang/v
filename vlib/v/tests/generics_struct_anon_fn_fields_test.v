struct Scope<T> {
	before fn () T
	specs  []fn (T) T
	after  fn (T)
}

fn test_generics_struct_anon_fn_fields() {
	s1 := Scope<u32>{}
	println(s1)
	ts1 := '${s1}'
	assert ts1.contains('before: fn () u32')
	assert ts1.contains('specs: []')
	assert ts1.contains('after: fn (u32)')

	s2 := Scope<f64>{}
	println(s2)
	ts2 := '${s2}'
	assert ts2.contains('before: fn () f64')
	assert ts2.contains('specs: []')
	assert ts2.contains('after: fn (f64)')

	s3 := Scope<string>{}
	println(s3)
	ts3 := '${s3}'
	assert ts3.contains('before: fn () string')
	assert ts3.contains('specs: []')
	assert ts3.contains('after: fn (string)')

	s4 := Scope<bool>{}
	println(s4)
	ts4 := '${s4}'
	assert ts4.contains('before: fn () bool')
	assert ts4.contains('specs: []')
	assert ts4.contains('after: fn (bool)')
}
