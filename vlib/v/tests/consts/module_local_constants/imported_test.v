import answer

fn test_imported_module_qualified_constant() {
	assert answer.value == 42
	assert answer.cached_value() == 42
}
