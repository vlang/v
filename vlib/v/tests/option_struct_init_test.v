struct Foo {
	a int
	b int
	c int
}

fn add(mut i &int) ?int {
	i++
	return i
}

fn test_struct_init_with_multiple_optionals() {
	mut i := 0
	foo := Foo{
		add(mut &i) or { 0 }
		add(mut &i) or { 0 }
		add(mut &i) or { 0 }
	}

	assert foo == Foo{1, 2, 3}
}
