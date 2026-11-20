module optimize

import v.ssa

fn has_verify_error(errors []VerifyError, needles ...string) bool {
	for err in errors {
		mut all := true
		for needle in needles {
			if !err.msg.contains(needle) {
				all = false
				break
			}
		}
		if all {
			return true
		}
	}
	return false
}

fn test_verify_empty_function() {
	mut m := ssa.Module.new()
	m.new_function('test_fn', 0)

	errors := verify(m)
	assert errors.len > 0
	assert errors[0].msg.contains('no blocks')
}

fn test_verify_missing_terminator() {
	mut m := ssa.Module.new()
	func_id := m.new_function('test_fn', 0)
	m.add_block(func_id, 'entry')

	errors := verify(m)
	assert errors.len > 0
	assert errors[0].msg.contains('no instructions') || errors[0].msg.contains('terminator')
}

fn test_verify_valid_function() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	const_id := m.get_or_add_const(i64_t, '42')
	m.add_instr(.ret, blk_id, 0, [const_id])

	errors := verify(m)
	assert errors.len == 0, 'expected no errors, got: ${errors}'
}

fn test_verify_binary_op_operand_count() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	const1 := m.get_or_add_const(i64_t, '1')
	const2 := m.get_or_add_const(i64_t, '2')
	const3 := m.get_or_add_const(i64_t, '3')

	m.add_instr(.add, blk_id, i64_t, [const1, const2, const3])
	m.add_instr(.ret, blk_id, 0, [])

	assert has_verify_error(verify(m), 'binary op', 'operands'), 'expected binary op operand count error'
}

fn test_verify_load_non_pointer() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	const_id := m.get_or_add_const(i64_t, '42')

	m.add_instr(.load, blk_id, i64_t, [const_id])
	m.add_instr(.ret, blk_id, 0, [])

	assert has_verify_error(verify(m), 'load', 'pointer'), 'expected load non-pointer error'
}

fn test_verify_phi_odd_operands() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	const_id := m.get_or_add_const(i64_t, '1')

	// Phi operands must be (value, block) pairs.
	m.add_instr(.phi, blk_id, i64_t, [const_id])
	m.add_instr(.ret, blk_id, 0, [])

	assert has_verify_error(verify(m), 'phi', 'odd'), 'expected phi odd operands error'
}

fn test_verify_branch_operand_count() {
	mut m := ssa.Module.new()
	bool_t := m.type_store.get_int(1)
	func_id := m.new_function('test_fn', 0)
	blk_id := m.add_block(func_id, 'entry')
	cond := m.get_or_add_const(bool_t, '1')

	m.add_instr(.br, blk_id, 0, [cond])

	assert has_verify_error(verify(m), 'br', 'operands'), 'expected br operand count error'
}

fn test_verify_jump_target_not_block() {
	mut m := ssa.Module.new()
	func_id := m.new_function('test_fn', 0)
	blk_id := m.add_block(func_id, 'entry')

	// Branch targets are raw block ids; this one does not exist.
	m.add_instr(.jmp, blk_id, 0, [ssa.ValueID(m.blocks.len + 10)])

	assert has_verify_error(verify(m), 'jmp', 'invalid block id'), 'expected jmp target not block error'
}

fn test_verify_terminator_not_last() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	target_blk := m.add_block(func_id, 'target')

	m.add_instr(.ret, blk_id, 0, [])
	const_id := m.get_or_add_const(i64_t, '1')
	m.add_instr(.add, blk_id, i64_t, [const_id, const_id])
	m.add_instr(.ret, target_blk, 0, [])

	assert has_verify_error(verify(m), 'terminator', 'end'), 'expected terminator not at end error'
}

fn test_verify_use_def_chain() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t)
	blk_id := m.add_block(func_id, 'entry')
	const1 := m.get_or_add_const(i64_t, '1')
	const2 := m.get_or_add_const(i64_t, '2')

	sum := m.add_instr(.add, blk_id, i64_t, [const1, const2])
	double := m.add_instr(.add, blk_id, i64_t, [sum, sum])
	m.add_instr(.ret, blk_id, 0, [double])

	errors := verify(m)
	assert errors.len == 0, 'expected no errors for valid use-def chain, got: ${errors}'

	// Instruction results record their users once; constants keep no use lists.
	assert m.values[sum].uses == [double]
	assert !m.values[const1].has_use_list()

	mut sum_val := m.values[sum]
	sum_val.uses = []
	m.values[sum] = sum_val
	assert has_verify_error(verify(m), 'not in uses list')
}
