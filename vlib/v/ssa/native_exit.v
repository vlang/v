module ssa

import v.flat

fn (mut b Builder) register_at_exit_runtime() {
	params := [b.resolve_type(flat.source_type_id('FnExitCb'))]
	if b.m.target.is_linux() && !b.use_native_c_sources {
		// glibc supplies atexit through libc_nonshared.a. Direct ELF output
		// registers executable callbacks with its exported __cxa_atexit ABI.
		ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
		b.register_extern('__cxa_atexit', b.i32_type, [params[0], ptr_i8, ptr_i8])
		atexit_id := b.register_synthetic_c_function('atexit', b.i32_type, params)
		entry := b.m.add_block(atexit_id, 'libc_atexit_entry')
		callback := b.func_add_argument(atexit_id, params[0], 'callback')
		nil_ptr := b.m.get_or_add_const(ptr_i8, '0')
		callee := b.m.add_value(.func_ref, b.i32_type, '__cxa_atexit', b.fn_ids['__cxa_atexit'])
		status := b.block_instr4(.call, entry, b.i32_type, callee, callback, nil_ptr, nil_ptr)
		b.block_instr1(.ret, entry, b.void_type, status)
	} else {
		b.register_extern('atexit', b.i32_type, params)
	}
	result_type := b.option_type_id(flat.source_type_void, true)
	func_id := b.register_synthetic_function('at_exit', result_type, params)
	b.generate_at_exit_body(func_id, result_type, params[0])
}

fn (mut b Builder) generate_at_exit_body(func_id int, result_type TypeID, callback_type TypeID) {
	entry := b.m.add_block(func_id, 'at_exit_entry')
	callback := b.func_add_argument(func_id, callback_type, 'callback')
	atexit_ref := b.m.add_value(.func_ref, b.void_type, 'atexit', b.fn_ids['atexit'])
	status := b.block_instr2(.call, entry, b.i32_type, atexit_ref, callback)
	zero := b.m.get_or_add_const(b.i32_type, '0')
	ok := b.block_instr2(.eq, entry, b.i1_type, status, zero)
	success := b.m.add_block(func_id, 'at_exit_registered')
	failure := b.m.add_block(func_id, 'at_exit_failed')
	b.block_instr3(.br, entry, b.void_type, ok, ValueID(success), ValueID(failure))
	result := b.block_option_value(success, result_type, true, ValueID(0))
	b.block_instr1(.ret, success, b.void_type, result)
	error_result := b.block_option_value(failure, result_type, false, ValueID(0))
	b.block_instr1(.ret, failure, b.void_type, error_result)
}
