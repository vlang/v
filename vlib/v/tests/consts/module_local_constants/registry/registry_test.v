module registry

fn test_constant_named_after_module_stays_a_value() {
	assert registry_value() == 42
}
