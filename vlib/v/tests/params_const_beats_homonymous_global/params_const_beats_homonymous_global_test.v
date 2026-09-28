module main

import consumer
import other

// other's own `__global` wins inside other (99); consumer's `const` wins as the
// default of consumer's own `Opt` field (7).
fn test_params_default_prefers_declaring_module_const_over_homonymous_global() {
	assert other.current() == 99
	assert consumer.current() == 7
}
