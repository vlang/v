module main

import consumer
import other

fn test_params_default_prefers_declaring_module_const_over_homonymous_global() {
	assert other.current() == 99
	assert consumer.current() == 7
}
