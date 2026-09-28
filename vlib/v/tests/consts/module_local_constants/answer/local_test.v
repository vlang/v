module answer

struct Record {
	value int
}

fn test_current_module_qualified_constant() {
	assert answer.value == 42
	assert answer.len == 7
	assert cached_value() == 42
}

fn test_local_value_can_shadow_module_name() {
	answer := Record{ value: 7 }
	assert answer.value == 7
}
