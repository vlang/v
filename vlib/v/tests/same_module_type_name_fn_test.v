module main

import same_module_type_name_fn

fn test_same_module_function_name_can_match_builtin_type() {
	assert same_module_type_name_fn.i64() == i64(1)
	assert same_module_type_name_fn.get_value() == i64(1)
}
