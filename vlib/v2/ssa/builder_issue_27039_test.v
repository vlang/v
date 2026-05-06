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

fn test_worker_module_preserves_target_os_for_stdio_symbols() {
	mut m := Module.new('main')
	m.target = TargetData{
		ptr_size:      8
		endian_little: true
		os:            'macos'
	}
	worker := m.new_worker_module()
	assert worker.target.os == 'macos'
	b := Builder.new(worker)
	assert c_stdio_symbol_for_os('stdout', b.c_stdio_target_os()) == '__stdoutp'
}

fn test_merge_worker_module_preserves_external_function_metadata() {
	mut m := Module.new('main')
	mut worker := Module.new('worker')
	func_idx := worker.new_function('C.external_fn', 0, []TypeID{})
	mut f := worker.funcs[func_idx]
	f.is_c_extern = true
	f.linkage = .external
	f.call_conv = .fast_call
	worker.funcs[func_idx] = f

	m.merge_worker_module(worker, []FuncSSAData{}, 1, 0, 0, 0, 0)

	assert m.funcs.len == 1
	assert m.funcs[0].name == 'C.external_fn'
	assert m.funcs[0].is_c_extern
	assert m.funcs[0].linkage == .external
	assert m.funcs[0].call_conv == .fast_call
}
