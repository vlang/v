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
	if b.m.target.is_linux() && !b.use_native_c_sources {
		b.register_native_linux_worker_helpers()
	}
}

// Linux compiler helpers call libc directly instead of linking the shipped C
// syscall object. Keep the variadic arguments pointer-width on both native ISAs.
fn (mut b Builder) register_native_linux_worker_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	b.register_extern('prctl', b.i32_type,
		[b.i32_type, b.u64_type, b.u64_type, b.u64_type, b.u64_type])
	b.register_extern('syscall', b.i64_type, [b.i64_type, ptr_i8, b.u64_type])
	b.set_extern_variadic('prctl', 1)
	b.set_extern_variadic('syscall', 1)
	zero := b.m.get_or_add_const(b.u64_type, '0')
	prctl := b.m.add_value(.func_ref, b.i32_type, 'prctl', b.fn_ids['prctl'])
	death_id := b.register_synthetic_c_function('v3_linux_parent_death_signal', b.i32_type,
		[b.i32_type])
	death_entry := b.m.add_block(death_id, 'parent_death_signal_entry')
	signal := b.func_add_argument(death_id, b.i32_type, 'signal')
	wide_signal := b.block_instr1(.zext, death_entry, b.u64_type, signal)
	death_option := b.m.get_or_add_const(b.i32_type, '1') // PR_SET_PDEATHSIG
	death_result := b.m.add_instr(.call, death_entry, b.i32_type,
		[prctl, death_option, wide_signal, zero, zero, zero])
	b.block_instr1(.ret, death_entry, b.void_type, death_result)
	name_id := b.register_synthetic_c_function('v3_linux_name_worker', b.i32_type, [])
	name_entry := b.m.add_block(name_id, 'name_worker_entry')
	name := b.m.add_value(.c_string_literal, ptr_i8, 'v3-pool', 0)
	name_pointer := b.block_instr1(.bitcast, name_entry, b.u64_type, name)
	name_option := b.m.get_or_add_const(b.i32_type, '15') // PR_SET_NAME
	name_result := b.m.add_instr(.call, name_entry, b.i32_type,
		[prctl, name_option, name_pointer, zero, zero, zero])
	b.block_instr1(.ret, name_entry, b.void_type, name_result)
	file_id := b.register_synthetic_c_function('v3_linux_memory_file', b.i32_type, [ptr_i8])
	file_entry := b.m.add_block(file_id, 'memory_file_entry')
	file_name := b.func_add_argument(file_id, ptr_i8, 'name')
	// memfd_create is 319 on AMD64 and 279 on AArch64.
	number := b.m.get_or_add_const(b.i64_type, if b.m.target.arch == 'arm64' {
		'279'
	} else {
		'319'
	})
	syscall := b.m.add_value(.func_ref, b.i64_type, 'syscall', b.fn_ids['syscall'])
	file_result := b.block_instr4(.call, file_entry, b.i64_type, syscall, number, file_name, zero)
	fd := b.block_instr1(.trunc, file_entry, b.i32_type, file_result)
	b.block_instr1(.ret, file_entry, b.void_type, fd)
}
