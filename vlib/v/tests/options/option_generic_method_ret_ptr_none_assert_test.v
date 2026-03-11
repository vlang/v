struct Foo[T] {}

fn (self Foo[T]) bar() ?&Foo[T] {
	return none
}

fn test_generic_method_returning_option_pointer_compares_to_none() {
	assert Foo[int]{}.bar() == none
}
