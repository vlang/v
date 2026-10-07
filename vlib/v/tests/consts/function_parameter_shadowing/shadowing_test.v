module main

import consumer
import producer

fn test_function_parameter_shadows_foreign_function_constant() {
	assert producer.direct == 6
	assert producer.local == 7
	assert producer.f() == 7
	assert consumer.call(fn () int {
		return 41
	}) == 42
	assert consumer.call_local(fn () int {
		return 40
	}) == 42
}
