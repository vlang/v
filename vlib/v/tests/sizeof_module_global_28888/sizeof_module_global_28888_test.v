module main

import globals28888

// https://github.com/vlang/v/issues/28888
fn test_sizeof_unqualified_module_global() {
	assert globals28888.bar_size() == 32
	assert globals28888.bar_size() == sizeof([4]u64)
}

fn test_sizeof_struct_module_global() {
	assert globals28888.point_size() == sizeof(globals28888.Point)
}

fn test_sizeof_module_globals_in_expressions() {
	assert globals28888.sum_size() == 34
	assert globals28888.const_size() == 32
	assert globals28888.generic_size(u16(1)) == 34
	assert globals28888.generic_size(u64(1)) == 40
}

fn test_sizeof_exported_module_global() {
	assert globals28888.exported_size() == 10
}
