struct Child {
}

struct Test[T] {
	child Child
}

fn new[T]() Test[T] {
	return Test[T]{
		child: Child{}
	}
}

fn test_generic_struct_init_with_field_struct_init() {
	t := new[int]()
	println(t)
	assert t.child == Child{}
}
