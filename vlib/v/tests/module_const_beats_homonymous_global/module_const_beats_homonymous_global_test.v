module main

import api
import consumer

// Mixing the two declarations (const method on the global's symbol, or the reverse) reads garbage.
fn test_declaring_module_declaration_wins_over_homonymous_global() {
	assert api.current() == 7
	assert consumer.current() == 99
}
