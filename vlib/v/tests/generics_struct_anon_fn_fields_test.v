struct Scope<T> {
	before fn () T
	specs  []fn (T) T
	after  fn (T)
}

fn test_generics_struct_anon_fn_fields() {
	s := Scope<u32>{}
	println(s)
	ts := '$s'
	assert ts.contains('before: fn () u32')
	assert ts.contains('specs: []')
	assert ts.contains('after: fn (u32)')
}
