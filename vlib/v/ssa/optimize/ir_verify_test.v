module optimize

import v.ssa

// build_verify_sample builds a small valid module with a branch and a phi.
fn build_verify_sample() &ssa.Module {
	mut m := ssa.Module.new()
	void_t := ssa.TypeID(0)
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('f', i32_type)
	entry := m.add_block(fid, 'entry')
	body := m.add_block(fid, 'body')
	exit_blk := m.add_block(fid, 'exit')
	zero := m.get_or_add_const(i32_type, '0')
	one := m.get_or_add_const(i32_type, '1')
	cmp := m.add_instr(.lt, entry, i32_type, [zero, one])
	_ = m.add_instr(.br, entry, void_t, [cmp, ssa.ValueID(body), ssa.ValueID(exit_blk)])
	v := m.add_instr(.add, body, i32_type, [zero, one])
	_ = m.add_instr(.jmp, body, void_t, [ssa.ValueID(exit_blk)])
	phi := m.add_instr(.phi, exit_blk, i32_type, [v, ssa.ValueID(body), zero, ssa.ValueID(entry)])
	_ = m.add_instr(.ret, exit_blk, i32_type, [phi])
	m.block_add_succ(entry, body)
	m.block_add_pred(body, entry)
	m.block_add_succ(entry, exit_blk)
	m.block_add_pred(exit_blk, entry)
	m.block_add_succ(body, exit_blk)
	m.block_add_pred(exit_blk, body)
	return m
}

fn test_verify_accepts_valid_module() {
	m := build_verify_sample()
	errors := verify(m)
	assert errors.len == 0
}

fn test_verify_rejects_missing_terminator() {
	mut m := ssa.Module.new()
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('f', i32_type)
	entry := m.add_block(fid, 'entry')
	zero := m.get_or_add_const(i32_type, '0')
	one := m.get_or_add_const(i32_type, '1')
	_ = m.add_instr(.add, entry, i32_type, [zero, one])
	errors := verify(m)
	assert errors.len > 0
	mut found := false
	for e in errors {
		if e.msg.contains('terminator') {
			found = true
		}
	}
	assert found
}

fn test_verify_rejects_phi_pred_mismatch() {
	mut m := ssa.Module.new()
	void_t := ssa.TypeID(0)
	i32_type := m.type_store.get_int(32)
	fid := m.new_function('f', i32_type)
	entry := m.add_block(fid, 'entry')
	other := m.add_block(fid, 'other')
	exit_blk := m.add_block(fid, 'exit')
	zero := m.get_or_add_const(i32_type, '0')
	_ = m.add_instr(.jmp, entry, void_t, [ssa.ValueID(exit_blk)])
	_ = m.add_instr(.jmp, other, void_t, [ssa.ValueID(exit_blk)])
	phi := m.add_instr(.phi, exit_blk, i32_type, [zero, ssa.ValueID(entry)])
	_ = m.add_instr(.ret, exit_blk, i32_type, [phi])
	m.block_add_succ(entry, exit_blk)
	m.block_add_pred(exit_blk, entry)
	m.block_add_succ(other, exit_blk)
	m.block_add_pred(exit_blk, other)
	errors := verify(m)
	assert errors.len > 0
	mut found := false
	for e in errors {
		if e.msg.contains('predecessor') {
			found = true
		}
	}
	assert found
}

fn test_verify_accepts_text_round_trip() {
	m := build_verify_sample()
	text := ssa.module_to_text(m)!
	mut m2 := ssa.module_from_text(text)!
	assert ssa.module_to_text(m2)! == text
	errors := verify(m2)
	assert errors.len == 0
}
