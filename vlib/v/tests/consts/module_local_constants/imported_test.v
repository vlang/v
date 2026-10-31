import answer

const local_value = 17

fn test_imported_module_qualified_constant() {
	assert answer.value == 42
	assert answer.cached_value() == 42
}

fn test_main_qualified_constant() {
	assert main.local_value == 17
}
