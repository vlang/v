module ssa

// register_native_allocation_helpers supplies allocations emitted by the transformer.
fn (mut b Builder) register_native_allocation_helpers() {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	ptr_array := b.m.type_store.get_ptr(b.array_type)
	b.register_extern('posix_memalign', b.i32_type, [b.m.type_store.get_ptr(ptr_i8), b.u64_type,
		b.u64_type])
	array_id := b.register_synthetic_function('v3_heap_array', ptr_array, [b.array_type])
	entry := b.m.add_block(array_id, 'heap_array_entry')
	value := b.func_add_argument(array_id, b.array_type, 'value')
	size := b.m.get_or_add_const(b.u64_type, b.m.type_size(b.array_type).str())
	malloc_ref := b.m.add_value(.func_ref, b.void_type, 'malloc', b.fn_ids['malloc'])
	raw := b.block_instr2(.call, entry, ptr_i8, malloc_ref, size)
	box := b.block_instr1(.bitcast, entry, ptr_array, raw)
	b.block_instr2(.store, entry, b.void_type, value, box)
	b.block_instr1(.ret, entry, b.void_type, box)
	copy_id := b.register_synthetic_function('v3_aligned_memdup', ptr_i8,
		[ptr_i8, b.i64_type, b.u64_type])
	b.generate_native_aligned_memdup(copy_id)
}

fn (mut b Builder) generate_native_aligned_memdup(func_id int) {
	ptr_i8 := b.m.type_store.get_ptr(b.i8_type)
	entry := b.m.add_block(func_id, 'aligned_memdup_entry')
	src := b.func_add_argument(func_id, ptr_i8, 'src')
	size := b.func_add_argument(func_id, b.i64_type, 'size')
	requested_alignment := b.func_add_argument(func_id, b.u64_type, 'alignment')
	minimum_alignment := b.m.get_or_add_const(b.u64_type, '8')
	too_small := b.block_instr2(.ult, entry, b.i1_type, requested_alignment,
		minimum_alignment)
	alignment_slot := b.block_instr0(.alloca, entry, b.m.type_store.get_ptr(b.u64_type))
	b.block_instr2(.store, entry, b.void_type, requested_alignment, alignment_slot)
	minimum_block := b.m.add_block(func_id, 'aligned_memdup_minimum_alignment')
	allocate_block := b.m.add_block(func_id, 'aligned_memdup_allocate')
	b.block_instr3(.br, entry, b.void_type, too_small, ValueID(minimum_block),
		ValueID(allocate_block))
	b.block_instr2(.store, minimum_block, b.void_type, minimum_alignment, alignment_slot)
	b.block_instr1(.jmp, minimum_block, b.void_type, ValueID(allocate_block))
	alignment := b.block_instr1(.load, allocate_block, b.u64_type, alignment_slot)
	nil_value := b.m.get_or_add_const(ptr_i8, '0')
	result_slot := b.block_instr0(.alloca, allocate_block, b.m.type_store.get_ptr(ptr_i8))
	b.block_instr2(.store, allocate_block, b.void_type, nil_value, result_slot)
	allocate_ref := b.m.add_value(.func_ref, b.void_type, 'posix_memalign',
		b.fn_ids['posix_memalign'])
	status := b.block_instr4(.call, allocate_block, b.i32_type, allocate_ref, result_slot,
		alignment, size)
	zero := b.m.get_or_add_const(b.i32_type, '0')
	ok := b.block_instr2(.eq, allocate_block, b.i1_type, status, zero)
	copy_block := b.m.add_block(func_id, 'aligned_memdup_copy')
	failed_block := b.m.add_block(func_id, 'aligned_memdup_failed')
	b.block_instr3(.br, allocate_block, b.void_type, ok, ValueID(copy_block), ValueID(failed_block))
	b.block_instr1(.ret, failed_block, b.void_type, nil_value)
	dst := b.block_instr1(.load, copy_block, ptr_i8, result_slot)
	copy_ref := b.m.add_value(.func_ref, b.void_type, 'memcpy', b.fn_ids['memcpy'])
	b.block_instr4(.call, copy_block, ptr_i8, copy_ref, dst, src, size)
	b.block_instr1(.ret, copy_block, b.void_type, dst)
}
