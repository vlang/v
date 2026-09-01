module arm64

import v3.ssa

fn test_sparse_codegen_slots_only_store_current_function_ids() {
	mut m := ssa.Module.new()
	for i in 0 .. 8 {
		m.add_value(.constant, ssa.TypeID(0), i.str(), 0)
	}
	m.blocks << ssa.BasicBlock{
		instrs: [ssa.ValueID(3), ssa.ValueID(6)]
	}
	func := ssa.Function{
		blocks: [ssa.BlockID(0)]
	}
	mut g := Gen.new(m)
	g.reset_value_slots(&func)
	g.reset_block_offsets(&func)
	g.set_stack_slot(3, -8)
	g.set_alloca_slot(6, -16, 24, 32)
	g.set_block_offset(0, 32)
	assert g.stack_offsets.len == 64
	assert g.slot_value_indices == [0, 3]
	assert g.block_offsets.len == 16
	assert g.stack_slot(3)? == -8
	assert g.alloca_slot(6)? == -16
	assert g.alloca_byte_size(6)? == 24
	assert g.alloca_alignment(6)? == 32
	assert g.block_offset(0)? == 32
	g.reset_value_slots(&func)
	g.reset_block_offsets(&func)
	assert g.slot_value_indices.len == 0
	assert g.block_offset_indices.len == 0
	assert g.stack_slot(3) == none
	assert g.alloca_slot(6) == none
	assert g.alloca_byte_size(6) == none
	assert g.alloca_alignment(6) == none
	assert g.block_offset(0) == none
}

fn test_zero_aggregate_uses_address_fallback_past_immediate_range() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	large_array := m.type_store.get_array(i64_type, 4097)
	mut g := Gen.new(m)
	g.emit_zero_aggregate(9, large_array, 0)
	assert g.macho.text_data.len == (4096 + 3) * 4
	last := g.macho.text_data.len - 4
	assert read_u32_le(g.macho.text_data, last) == asm_str(xzr, Reg(11))
}

fn test_external_global_address_uses_got_load_relocations() {
	mut m := ssa.Module.new()
	i8_type := m.type_store.get_int(8)
	m.add_external_global('environ', m.type_store.get_ptr(m.type_store.get_ptr(i8_type)))
	mut g := Gen.new(m)
	g.emit_global_addr(8, 'environ')
	assert g.macho.relocs.len == 2
	assert g.macho.relocs[0].type_ == arm64_reloc_got_load_page21
	assert g.macho.relocs[1].type_ == arm64_reloc_got_load_pageoff12
	assert read_u32_le(g.macho.text_data, 4) == asm_ldr_pageoff(Reg(8))
}

fn test_aggregate_bitcast_copies_every_word() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	pair_type := m.type_store.get_tuple([i64_type, i64_type])
	func_id := m.new_function('copy_pair', pair_type)
	block_id := m.add_block(func_id, 'entry')
	source := m.add_instr(.struct_init, block_id, pair_type, [])
	result := m.add_instr(.bitcast, block_id, pair_type, [source])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[func_id])
	g.set_stack_slot(source, -16)
	g.set_stack_slot(result, -32)
	g.gen_instr(result)
	assert g.macho.text_data.len == 16
}

fn test_large_c_variadic_aggregate_passes_its_address() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	large_type := m.type_store.get_tuple([i64_type, i64_type, i64_type])
	external_id := m.new_function('consume', ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	external.is_variadic = true
	external.variadic_start = 1
	m.funcs[external_id] = external
	consume := m.add_value(.func_ref, ssa.TypeID(0), 'consume', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	tag := m.add_value(.constant, i64_type, '1', 0)
	large := m.add_instr(.struct_init, block_id, large_type, [])
	call := m.add_instr(.call, block_id, ssa.TypeID(0), [consume, tag, large])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(large, -24)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_sub_imm(Reg(8), fp, 24) in words
	assert asm_str(Reg(8), sp) in words
}

fn test_c_homogeneous_float_aggregate_uses_simd_argument_registers() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	pair_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f64_type, f64_type]
		is_c_struct: true
	})
	external_id := m.new_function('consume_pair', ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	m.funcs[external_id] = external
	consume := m.add_value(.func_ref, ssa.TypeID(0), 'consume_pair', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	pair := m.add_instr(.struct_init, block_id, pair_type, [])
	call := m.add_instr(.call, block_id, ssa.TypeID(0), [consume, pair])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(pair, -16)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_fmov_d_x(0, Reg(9)) in words
	assert asm_fmov_d_x(1, Reg(9)) in words
}

fn test_c_homogeneous_float_aggregate_overflow_uses_the_stack() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	pair_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f64_type, f64_type]
		is_c_struct: true
	})
	external_id := m.new_function('consume_pair', ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	m.funcs[external_id] = external
	consume := m.add_value(.func_ref, ssa.TypeID(0), 'consume_pair', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	mut operands := [consume]
	for i in 1 .. 8 {
		operands << m.add_value(.constant, f64_type, i.str(), 0)
	}
	pair := m.add_instr(.struct_init, block_id, pair_type, [])
	operands << pair
	call := m.add_instr(.call, block_id, ssa.TypeID(0), operands)
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(pair, -16)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_str_imm(Reg(8), sp, 0) in words
	assert asm_str_imm(Reg(8), sp, 1) in words
	assert asm_ldur(Reg(0), fp, -16) !in words
	assert asm_ldur(Reg(1), fp, -8) !in words
}

fn test_overaligned_c_float_aggregate_with_padding_is_not_hfa() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	padded_pair_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f64_type, f64_type]
		is_c_struct: true
		alignment: 32
	})
	aligned_quad_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f64_type, f64_type, f64_type, f64_type]
		is_c_struct: true
		alignment: 32
	})
	g := Gen.new(m)
	assert m.type_size(padded_pair_type) == 32
	assert g.c_homogeneous_float_aggregate(padded_pair_type) == none
	quad := g.c_homogeneous_float_aggregate(aligned_quad_type) or {
		panic('aligned C quad should remain an HFA when it has no padding')
	}
	assert quad.elements.len == 4
}

fn test_c_homogeneous_float_aggregate_return_uses_simd_registers() {
	mut m := ssa.Module.new()
	f64_type := m.type_store.get_float(64)
	triple_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f64_type, f64_type, f64_type]
		is_c_struct: true
	})
	external_id := m.new_function('make_triple', triple_type)
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	m.funcs[external_id] = external
	make_triple := m.add_value(.func_ref, triple_type, 'make_triple', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	call := m.add_instr(.call, block_id, triple_type, [make_triple])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(call, -24)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_fmov_x_d(Reg(8), 0) in words
	assert asm_fmov_x_d(Reg(8), 1) in words
	assert asm_fmov_x_d(Reg(8), 2) in words
	assert asm_sub_imm(Reg(8), fp, 24) !in words
}

fn test_c_f32_homogeneous_float_aggregate_return_uses_simd_registers() {
	mut m := ssa.Module.new()
	f32_type := m.type_store.get_float(32)
	pair_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [f32_type, f32_type]
		is_c_struct: true
	})
	external_id := m.new_function('make_pair', pair_type)
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	m.funcs[external_id] = external
	make_pair := m.add_value(.func_ref, pair_type, 'make_pair', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	call := m.add_instr(.call, block_id, pair_type, [make_pair])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.set_stack_slot(call, -8)
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_fmov_w_s(Reg(8), 0) in words
	assert asm_fmov_w_s(Reg(8), 1) in words
}

fn test_literal_c_variadic_string_stores_both_words() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	i32_type := m.type_store.get_int(32)
	ptr_type := m.type_store.get_ptr(m.type_store.get_int(8))
	string_type := m.type_store.register(ssa.Type{
		kind: .struct_t
		fields: [ptr_type, i32_type, i32_type]
	})
	external_id := m.new_function('consume', ssa.TypeID(0))
	mut external := m.funcs[external_id]
	external.is_c_extern = true
	external.is_variadic = true
	external.variadic_start = 1
	m.funcs[external_id] = external
	consume := m.add_value(.func_ref, ssa.TypeID(0), 'consume', external_id)
	caller_id := m.new_function('caller', ssa.TypeID(0))
	block_id := m.add_block(caller_id, 'entry')
	tag := m.add_value(.constant, i64_type, '1', 0)
	literal := m.add_value(.string_literal, string_type, 'hello', 0)
	call := m.add_instr(.call, block_id, ssa.TypeID(0), [consume, tag, literal])
	mut g := Gen.new(m)
	g.reset_value_slots(&m.funcs[caller_id])
	g.gen_call(int(call), m.instrs[m.values[call].index])
	mut words := []u32{}
	for i := 0; i < g.macho.text_data.len; i += 4 {
		words << read_u32_le(g.macho.text_data, i)
	}
	assert asm_str_imm(Reg(8), sp, 0) in words
	assert asm_str_imm(Reg(10), sp, 1) in words
}
