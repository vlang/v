struct Foo {
	s string
	i int
}

fn (f Foo) get_s() string {
	return f.s
}

fn (f &Foo) get_s_ref() string {
	return f.s
}

fn (f Foo) add(a int) int {
	return a + f.i
}

fn (f &Foo) add_ref(a int) int {
	return a + f.i
}

fn test_methods_as_fields() {
	f := Foo{
		s: 'hello'
		i: 1
	}

	get_s := f.get_s
	get_s_ref := f.get_s_ref
	add := f.add
	add_ref := f.add_ref

	assert typeof(get_s).str() == 'fn () string'
	assert typeof(get_s_ref).str() == 'fn () string'
	assert typeof(add).str() == 'fn (int) int'
	assert typeof(add_ref).str() == 'fn (int) int'

	assert get_s() == 'hello'
	assert get_s_ref() == 'hello'
	assert add(2) == 3
	assert add_ref(2) == 3
}
