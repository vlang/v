module same_module_type_name_fn

// i64 returns a value from a same-module function whose name matches a builtin type.
pub fn i64() i64 {
	return 1
}

// vfmt off
const module_value = same_module_type_name_fn.i64()
// vfmt on

// get_value returns the const initialized through the same-module selector call.
pub fn get_value() i64 {
	return module_value
}
