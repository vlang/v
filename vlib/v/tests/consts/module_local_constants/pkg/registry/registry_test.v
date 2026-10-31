module registry

fn test_nested_module_global_receiver_in_const_initializer() {
	assert value == 42
	assert copied != value
	assert registry.value == 7
	assert global_value() == 7
}
