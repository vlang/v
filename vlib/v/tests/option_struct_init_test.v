struct Foo {
	a int
	b int
	c int
}

struct Holder {
mut:
	i int
}

fn add(mut h Holder) ?int {
	h.i++
	return h.i
}

fn test_struct_init_with_multiple_optionals() {
	mut h := Holder{}
	foo := Foo{add(mut h) or { 0 }, add(mut h) or { 0 }, add(mut h) or { 0 }}

	assert foo == Foo{1, 2, 3}
}
