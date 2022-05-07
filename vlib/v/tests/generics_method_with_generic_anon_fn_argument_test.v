struct Foo<T> {
}

fn (f Foo<T>) do(name string, d fn (T), v T) T {
	println('running ' + name)
	d(v)
	println('ran ' + name)
	return v
}

fn test_generics_method_with_generic_anon_fn_argument() {
	f1 := Foo<string>{}
	r1 := f1.do('foo', fn (s string) {
		println('s value is ' + s)
	}, 'bar')
	assert r1 == 'bar'

	f2 := Foo<int>{}
	r2 := f2.do('bar', fn (s int) {
		println('s value is $s')
	}, 22)
	assert r2 == 22
}
