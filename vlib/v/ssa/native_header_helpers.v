module ssa

// Native output does not include the C headers that define these inline helpers.
fn (mut b Builder) register_native_header_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	get_id := b.register_synthetic_c_function('v_flat_payload_ptr_get', ptr_i8,
		[ptr_i8, b.u64_type])
	set_id := b.register_synthetic_c_function('v_flat_payload_ptr_set', b.void_type,
		[ptr_i8, b.u64_type, ptr_i8])
	for id in [get_id, set_id] {
		entry := b.m.add_block(id, 'payload_pointer_entry')
		base := b.func_add_argument(id, ptr_i8, 'base')
		index := b.func_add_argument(id, b.u64_type, 'index')
		stride := b.m.get_or_add_const(b.u64_type, '8')
		offset := b.block_instr2(.mul, entry, b.u64_type, index, stride)
		slot := b.block_instr2(.get_element_ptr, entry, b.m.type_store.get_ptr(ptr_i8), base,
			offset)
		if id == get_id {
			value := b.block_instr1(.load, entry, ptr_i8, slot)
			b.block_instr1(.ret, entry, b.void_type, value)
		} else {
			value := b.func_add_argument(id, ptr_i8, 'value')
			b.block_instr2(.store, entry, b.void_type, value, slot)
			b.block_instr0(.ret, entry, b.void_type)
		}
	}
	lock_id := b.register_synthetic_c_function('v_filelock_lock', b.i32_type,
		[b.i32_type, b.i32_type, b.i32_type, b.u64_type, b.u64_type])
	b.generate_native_filelock_body(lock_id, false)
	unlock_id := b.register_synthetic_c_function('v_filelock_unlock', b.i32_type,
		[b.i32_type, b.u64_type, b.u64_type])
	b.generate_native_filelock_body(unlock_id, true)
}

fn (mut b Builder) generate_native_filelock_body(func_id int, unlock bool) {
	// Darwin struct flock: off_t start/len, pid_t pid, short type/whence.
	flock_type := b.m.type_store.register(Type{
		kind:        .struct_t
		fields:      [b.i64_type, b.i64_type, b.i32_type, b.u16_type, b.u16_type]
		field_names: ['l_start', 'l_len', 'l_pid', 'l_type', 'l_whence']
	})
	entry := b.m.add_block(func_id, 'filelock_entry')
	fd := b.func_add_argument(func_id, b.i32_type, 'fd')
	zero32 := b.m.get_or_add_const(b.i32_type, '0')
	setlk := b.m.get_or_add_const(b.i32_type, '8')
	mut command := setlk
	mut lock_type := b.m.get_or_add_const(b.u16_type, '2') // F_UNLCK
	if !unlock {
		exclusive := b.func_add_argument(func_id, b.i32_type, 'exclusive')
		immediate := b.func_add_argument(func_id, b.i32_type, 'immediate')
		is_exclusive := b.block_instr2(.ne, entry, b.i1_type, exclusive, zero32)
		exclusive16 := b.block_instr1(.zext, entry, b.u16_type, is_exclusive)
		one16 := b.m.get_or_add_const(b.u16_type, '1')
		two16 := b.m.get_or_add_const(b.u16_type, '2')
		write_bit := b.block_instr2(.mul, entry, b.u16_type, exclusive16, two16)
		lock_type = b.block_instr2(.add, entry, b.u16_type, one16, write_bit) // F_RDLCK or F_WRLCK
		blocking := b.block_instr2(.eq, entry, b.i1_type, immediate, zero32)
		blocking32 := b.block_instr1(.zext, entry, b.i32_type, blocking)
		command = b.block_instr2(.add, entry, b.i32_type, setlk, blocking32) // F_SETLK or F_SETLKW
	}
	start := b.func_add_argument(func_id, b.u64_type, 'start')
	length := b.func_add_argument(func_id, b.u64_type, 'len')
	flock := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(flock_type))
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	bytes := b.block_instr1(.bitcast, entry, ptr_i8, flock)
	zero := b.m.get_or_add_const(b.i64_type, '0')
	size := b.m.get_or_add_const(b.i64_type, '${b.m.type_size(flock_type)}')
	memset := b.m.add_value(.func_ref, ptr_i8, 'memset', b.fn_ids['memset'])
	b.block_instr4(.call, entry, ptr_i8, memset, bytes, zero, size)
	for index, value in [start, length, ValueID(0), lock_type] {
		if index == 2 {
			continue
		}
		field := b.block_struct_field_ptr(entry, flock, flock_type, index)
		b.block_instr2(.store, entry, b.void_type, value, field)
	}
	fcntl := b.m.add_value(.func_ref, b.i32_type, 'fcntl', b.fn_ids['fcntl'])
	result := b.block_instr4(.call, entry, b.i32_type, fcntl, fd, command, bytes)
	b.block_instr1(.ret, entry, b.void_type, result)
}
