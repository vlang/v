module main

import private_mutability

fn test_private_receiver_mutation_on_mutable_binding_outside_module() {
	mut counter := private_mutability.Counter{}
	counter.bump_hidden_via_method()
	counter.bump_hidden_via_helper()
	assert counter.label_text() == ''
}

fn test_array_alias_mut_method_on_mutable_binding() {
	mut values := private_mutability.MyArray([1, 2, 3])
	values.reverse_in_place()
	assert values == private_mutability.MyArray([1, 2, 3])
}
