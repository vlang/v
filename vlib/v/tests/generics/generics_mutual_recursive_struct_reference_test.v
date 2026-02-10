struct Foo[T] {
	bar ?&Bar[T]
}

struct Bar[T] {
	foo ?&Foo[T]
}

fn foo_new[T]() &Foo[T] {
	return &Foo[T]{}
}

fn test_mutual_recursive_generic_struct_reference() {
	foo := foo_new[int]()
	assert foo != unsafe { nil }
}
