module ssa

fn test_c_stdio_symbols_are_platform_specific() {
	assert c_stdio_symbol_for_os('stderr', 'linux') == 'stderr'
	assert c_stdio_symbol_for_os('stdout', 'linux') == 'stdout'
	assert c_stdio_symbol_for_os('stdin', 'linux') == 'stdin'
	assert c_stdio_symbol_for_os('stderr', 'macos') == '__stderrp'
	assert c_stdio_symbol_for_os('stdout', 'freebsd') == '__stdoutp'
	assert c_stdio_symbol_for_os('stdin', 'openbsd') == '__stdin'
	assert c_stdio_symbol_needs_load('linux')
	assert !c_stdio_symbol_needs_load('openbsd')
}

fn test_c_float_epsilon_macros_are_inline_constants() {
	flt_width, flt_value := c_float_macro_const('FLT_EPSILON')
	assert flt_width == 32
	assert flt_value == '1.19209290e-07'
	dbl_width, dbl_value := c_float_macro_const('DBL_EPSILON')
	assert dbl_width == 64
	assert dbl_value == '2.2204460492503131e-16'
	unknown_width, unknown_value := c_float_macro_const('OTHER')
	assert unknown_width == 0
	assert unknown_value == ''
}
