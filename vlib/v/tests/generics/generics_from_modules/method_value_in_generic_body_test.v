import v.tests.generics.generics_from_modules.genericmodule

// A method of a generic struct of another module named without a call inside a
// generic body: each instance asks for the method of its own receiver.
fn peek[T](b &genericmodule.Box[T]) T {
	getter := b.get
	return getter()
}

fn test_a_method_of_a_generic_struct_of_another_module_named_without_a_call_in_a_generic_body() {
	assert peek(genericmodule.box(6)) == 6
	assert peek(genericmodule.box('six')) == 'six'
}
