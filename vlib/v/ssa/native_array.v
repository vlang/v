module ssa

// register_native_array_growth preserves borrowed buffers and the builtin managed ABI.
fn (mut b Builder) register_native_array_growth() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	func_id := b.register_synthetic_function('__ssa_array_grow', b.void_type,
		[b.m.type_store.get_ptr(b.array_type), b.i64_type])
	entry := b.m.add_block(func_id, 'array_grow_entry')
	arr := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.array_type), 'arr')
	cap := b.func_add_argument(func_id, b.i64_type, 'cap')
	data_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 0)
	offset_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 1)
	len_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 2)
	cap_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 3)
	flags_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 4)
	stride_ptr := b.block_struct_field_ptr(entry, arr, b.array_type, 5)
	old_data := b.block_instr1(.load, entry, ptr_i8, data_ptr)
	len32 := b.block_instr1(.load, entry, b.i32_type, len_ptr)
	len := b.block_instr1(.zext, entry, b.i64_type, len32)
	stride32 := b.block_instr1(.load, entry, b.i32_type, stride_ptr)
	stride := b.block_instr1(.zext, entry, b.i64_type, stride32)
	flags := b.block_instr1(.load, entry, b.u32_type, flags_ptr)
	size := b.block_instr2(.mul, entry, b.i64_type, cap, stride)
	// Match ArrayDataHeader's 16-byte reservation and the builtin data alignment.
	allocation_size := b.block_instr2(.add, entry, b.i64_type, size,
		b.m.get_or_add_const(b.i64_type, '31'))
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	raw := b.block_instr2(.call, entry, ptr_i8, malloc_ref, allocation_size)
	raw_integer := b.block_instr1(.bitcast, entry, b.u64_type, raw)
	biased := b.block_instr2(.add, entry, b.u64_type, raw_integer,
		b.m.get_or_add_const(b.u64_type, '31'))
	aligned := b.block_instr2(.and_, entry, b.u64_type, biased,
		b.m.get_or_add_const(b.u64_type, '18446744073709551600'))
	data := b.block_instr1(.bitcast, entry, ptr_i8, aligned)
	header := b.block_instr2(.sub, entry, ptr_i8, data,
		b.m.get_or_add_const(b.i64_type, '16'))
	zero := b.m.get_or_add_const(b.i64_type, '0')
	memset_ref := b.m.add_value(.func_ref, b.void_type, 'memset', b.fn_ids['memset'])
	b.block_instr4(.call, entry, ptr_i8, memset_ref, header, zero,
		b.m.get_or_add_const(b.i64_type, '16'))
	allocation_ptr := b.block_instr1(.bitcast, entry, b.m.type_store.get_ptr(ptr_i8), header)
	b.block_instr2(.store, entry, b.void_type, raw, allocation_ptr)
	copy_size := b.block_instr2(.mul, entry, b.i64_type, len, stride)
	has_items := b.block_instr2(.gt, entry, b.i1_type, copy_size, zero)
	copy_block := b.m.add_block(func_id, 'array_grow_copy')
	finish := b.m.add_block(func_id, 'array_grow_finish')
	b.block_instr3(.br, entry, b.void_type, has_items, ValueID(copy_block), ValueID(finish))
	memcpy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, copy_block, ptr_i8, memcpy_ref, data, old_data, copy_size)
	b.block_instr1(.jmp, copy_block, b.void_type, ValueID(finish))
	// Existing slices may still reference the old storage. Growth takes a fresh
	// managed allocation, as array.ensure_cap does without the noslices promise.
	b.block_instr2(.store, finish, b.void_type, data, data_ptr)
	b.block_instr2(.store, finish, b.void_type, zero, offset_ptr)
	b.block_instr2(.store, finish, b.void_type, cap, cap_ptr)
	retained_fixed := b.block_instr2(.and_, finish, b.u32_type, flags,
		b.m.get_or_add_const(b.u32_type, '2147483648'))
	is_fixed := b.block_instr2(.ne, finish, b.i1_type, retained_fixed,
		b.m.get_or_add_const(b.u32_type, '0'))
	flags_slot := b.block_instr0(.alloca, finish, b.m.type_store.get_ptr(b.u32_type))
	owned_flags := b.block_instr2(.and_, finish, b.u32_type, flags,
		b.m.get_or_add_const(b.u32_type, '2147483583'))
	managed_flags := b.block_instr2(.or_, finish, b.u32_type, owned_flags,
		b.m.get_or_add_const(b.u32_type, '16'))
	b.block_instr2(.store, finish, b.void_type, managed_flags, flags_slot)
	clear_borrowed := b.m.add_block(func_id, 'array_grow_clear_borrowed')
	done := b.m.add_block(func_id, 'array_grow_done')
	b.block_instr3(.br, finish, b.void_type, is_fixed, ValueID(clear_borrowed), ValueID(done))
	fixed_owned_flags := b.block_instr2(.and_, clear_borrowed, b.u32_type, managed_flags,
		b.m.get_or_add_const(b.u32_type, '4294967287'))
	b.block_instr2(.store, clear_borrowed, b.void_type, fixed_owned_flags, flags_slot)
	b.block_instr1(.jmp, clear_borrowed, b.void_type, ValueID(done))
	final_flags := b.block_instr1(.load, done, b.u32_type, flags_slot)
	b.block_instr2(.store, done, b.void_type, final_flags, flags_ptr)
	b.block_instr0(.ret, done, b.void_type)
}
