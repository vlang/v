module main

import private_mutability

// Regression test for issue #24719.
fn test_private_receiver_mutation_does_not_require_mut_outside_module() {
	counter := private_mutability.Counter{}
	counter.bump_hidden_via_method()
	counter.bump_hidden_via_helper()
	assert counter.label_text() == ''
}
