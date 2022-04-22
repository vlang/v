interface Any {}

struct ConcreteA {
	a int
}

struct ConcreteB {
	b int
}

struct Container {
	concrete_a Any
	concrete_b Any
}

fn cast_struct<T>(any_struct Any) &T {
	if any_struct is T {
		return any_struct
	}
	panic('cannot cast')
}

fn test_generic_empty_interface_to_multi_struct() {
	concrete_a := cast_struct<ConcreteA>(ConcreteA{12345})
	concrete_b := cast_struct<ConcreteB>(ConcreteB{54321})
	println(concrete_a.a)
	println(concrete_b.b)
	assert concrete_a.a == 12345
	assert concrete_b.b == 54321
}
