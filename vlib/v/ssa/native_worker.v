module ssa

fn (mut b Builder) register_native_worker_helpers() {
	b.register_extern('pthread_equal', b.i32_type, [b.u64_type, b.u64_type])
	func_id := b.register_synthetic_c_function('v3_pthread_is_current', b.i32_type, [b.u64_type])
	entry := b.m.add_block(func_id, 'pthread_is_current_entry')
	worker_thread := b.func_add_argument(func_id, b.u64_type, 'thread')
	self_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_self', b.fn_ids['pthread_self'])
	self := b.block_instr1(.call, entry, b.u64_type, self_ref)
	equal_ref := b.m.add_value(.func_ref, b.void_type, 'pthread_equal', b.fn_ids['pthread_equal'])
	equal := b.block_instr3(.call, entry, b.i32_type, equal_ref, self, worker_thread)
	b.block_instr1(.ret, entry, b.void_type, equal)
}
