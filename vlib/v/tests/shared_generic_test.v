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
