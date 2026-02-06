struct Foo[T] {
	a T
}

fn test_shared_struct_has_generics() {
	shared foo := Foo[int]{1}
	lock foo {
		assert foo.a == 1
	}
	shared bar := &Foo[int]{1}
	lock bar {
		assert bar.a == 1
	}
}

fn generic_struct_as_parameters(shared arg Foo[int]) {
	rlock arg {
		assert arg.a == 1
	}
}

fn test_generic_struct_as_parameters() {
	shared foo := Foo[int]{1}
	generic_struct_as_parameters(shared foo)
}
