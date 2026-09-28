module main

import api
import consumer

// A bare name resolves to the declaration of its own module: api's `__global`
// wins inside api, consumer's `const` wins inside consumer. Mixing the two
// (const method on the global's symbol, or the reverse) reads garbage.
fn test_declaring_module_declaration_wins_over_homonymous_global() {
	assert api.current() == 7
	assert consumer.current() == 99
}
