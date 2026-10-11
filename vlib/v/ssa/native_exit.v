module ssa

fn (mut b Builder) register_at_exit_runtime() {
	params := [b.resolve_type('FnExitCb')]
	b.register_extern('atexit', b.i32_type, params)
	result_type := b.option_type_id('void', true)
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
