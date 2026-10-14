import fn_value_scan_gate_dep as renamed { original }

fn pass_callback[T](callback fn (T) T, value T) T {
	return callback(value)
}

fn test_generic_values_keep_selective_imports_and_module_aliases() {
	selective := original[int]
	qualified := renamed.original[string]
	assert selective(42) == 42
	assert qualified('value') == 'value'
	assert pass_callback[int](selective, 17) == 17
	assert pass_callback[string](qualified, 'nested') == 'nested'
}

fn test_array_indices_and_generic_values_can_share_the_scan() {
	values := [11, 22]
	callback := renamed.original[int]
	assert callback(values[1]) == 22
	assert values[0] == 11
}
