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
	g.set_alloca_slot(6, -16, 24)
	g.set_block_offset(0, 32)
	assert g.stack_offsets.len == 64
	assert g.slot_value_indices == [0, 3]
	assert g.block_offsets.len == 16
	assert g.stack_slot(3)? == -8
	assert g.alloca_slot(6)? == -16
	assert g.alloca_byte_size(6)? == 24
	assert g.block_offset(0)? == 32
	g.reset_value_slots(&func)
	g.reset_block_offsets(&func)
	assert g.slot_value_indices.len == 0
	assert g.block_offset_indices.len == 0
	assert g.stack_slot(3) == none
	assert g.alloca_slot(6) == none
	assert g.alloca_byte_size(6) == none
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
