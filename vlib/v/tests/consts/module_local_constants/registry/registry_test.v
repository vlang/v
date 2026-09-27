module registry

fn test_constant_named_after_module_stays_a_value() {
	assert registry_value() == 42
	assert registry_answer() == 44
	assert registry_derived() == 46
}
