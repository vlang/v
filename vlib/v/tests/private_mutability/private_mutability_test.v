module main

import private_mutability

// Regression test for issues #24719 and #29233.
fn test_private_receiver_mutation_does_not_require_mut_outside_module() {
	counter := private_mutability.Counter{}
	counter.bump_hidden_via_method()
	counter.bump_hidden_via_helper()
	assert counter.label_text() == ''
}

fn test_array_alias_mut_method_on_mutable_binding() {
	mut values := private_mutability.MyArray([1, 2, 3])
	values.reverse_in_place()
	assert values == private_mutability.MyArray([1, 2, 3])
}
