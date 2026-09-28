import pkg.answer

fn test_nested_current_module_qualified_constant_dependency() {
	assert answer.derived == 42
}
