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
