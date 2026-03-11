// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

fn test_verify_empty_function() {
	mut m := ssa.Module.new('test')
	func_id := m.new_function('test_fn', 0, [])

	// Function with no blocks should fail
	errors := verify(m)
	assert errors.len > 0
	assert errors[0].msg.contains('no blocks')
}

fn test_verify_missing_terminator() {
	mut m := ssa.Module.new('test')
	func_id := m.new_function('test_fn', 0, [])
	blk_id := m.add_block(func_id, 'entry')

	// Block with no instructions should fail (missing terminator)
	errors := verify(m)
	assert errors.len > 0
	assert errors[0].msg.contains('no instructions') || errors[0].msg.contains('terminator')
}

fn test_verify_valid_function() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create a constant
	const_id := m.add_value_node(.constant, i64_t, 'const_42', 0)

	// Add ret instruction with constant operand
	m.add_instr(.ret, blk_id, 0, [const_id])

	errors := verify(m)
	assert errors.len == 0, 'expected no errors, got: ${errors}'
}

fn test_verify_binary_op_operand_count() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create constants
	const1 := m.add_value_node(.constant, i64_t, 'const_1', 0)
	const2 := m.add_value_node(.constant, i64_t, 'const_2', 0)
	const3 := m.add_value_node(.constant, i64_t, 'const_3', 0)

	// Add binary op with wrong number of operands (3 instead of 2)
	m.add_instr(.add, blk_id, i64_t, [const1, const2, const3])
	m.add_instr(.ret, blk_id, 0, [])

	errors := verify(m)
	mut found_binary_error := false
	for err in errors {
		if err.msg.contains('binary op') && err.msg.contains('operands') {
			found_binary_error = true
			break
		}
	}
	assert found_binary_error, 'expected binary op operand count error'
}

fn test_verify_load_non_pointer() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create a non-pointer constant
	const_id := m.add_value_node(.constant, i64_t, 'const_42', 0)

	// Try to load from non-pointer (should error)
	m.add_instr(.load, blk_id, i64_t, [const_id])
	m.add_instr(.ret, blk_id, 0, [])

	errors := verify(m)
	mut found_load_error := false
	for err in errors {
		if err.msg.contains('load') && err.msg.contains('pointer') {
			found_load_error = true
			break
		}
	}
	assert found_load_error, 'expected load non-pointer error'
}

fn test_verify_phi_odd_operands() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create constant
	const_id := m.add_value_node(.constant, i64_t, 'const_1', 0)

	// Add phi with odd number of operands (should be pairs)
	m.add_instr(.phi, blk_id, i64_t, [const_id]) // 1 operand instead of pairs
	m.add_instr(.ret, blk_id, 0, [])

	errors := verify(m)
	mut found_phi_error := false
	for err in errors {
		if err.msg.contains('phi') && err.msg.contains('odd') {
			found_phi_error = true
			break
		}
	}
	assert found_phi_error, 'expected phi odd operands error'
}

fn test_verify_branch_operand_count() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create constant for condition
	const_id := m.add_value_node(.constant, i64_t, 'cond', 0)

	// Add br with wrong number of operands (1 instead of 3)
	m.add_instr(.br, blk_id, 0, [const_id])

	errors := verify(m)
	mut found_br_error := false
	for err in errors {
		if err.msg.contains('br') && err.msg.contains('operands') {
			found_br_error = true
			break
		}
	}
	assert found_br_error, 'expected br operand count error'
}

fn test_verify_jump_target_not_block() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create constant (not a block)
	const_id := m.add_value_node(.constant, i64_t, 'not_a_block', 0)

	// Add jmp targeting non-block
	m.add_instr(.jmp, blk_id, 0, [const_id])

	errors := verify(m)
	mut found_jmp_error := false
	for err in errors {
		if err.msg.contains('jmp') && err.msg.contains('basic_block') {
			found_jmp_error = true
			break
		}
	}
	assert found_jmp_error, 'expected jmp target not block error'
}

fn test_verify_terminator_not_last() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create block for jump target
	target_blk := m.add_block(func_id, 'target')
	target_val := m.blocks[target_blk].val_id

	// Add terminator in middle, then another instruction
	m.add_instr(.ret, blk_id, 0, [])

	// Manually add another instruction after terminator
	const_id := m.add_value_node(.constant, i64_t, 'const', 0)
	m.add_instr(.add, blk_id, i64_t, [const_id, const_id])

	// Add terminator to target block
	m.add_instr(.ret, target_blk, 0, [])

	errors := verify(m)
	mut found_term_error := false
	for err in errors {
		if err.msg.contains('terminator') && err.msg.contains('end') {
			found_term_error = true
			break
		}
	}
	assert found_term_error, 'expected terminator not at end error'
}

fn test_verify_use_def_chain() {
	mut m := ssa.Module.new('test')
	i64_t := m.type_store.get_int(64)
	func_id := m.new_function('test_fn', i64_t, [])
	blk_id := m.add_block(func_id, 'entry')

	// Create constants and use them
	const1 := m.add_value_node(.constant, i64_t, 'const_1', 0)
	const2 := m.add_value_node(.constant, i64_t, 'const_2', 0)

	// Add instruction using both constants
	result := m.add_instr(.add, blk_id, i64_t, [const1, const2])
	m.add_instr(.ret, blk_id, i64_t, [result])

	// Verify use-def chains are correct
	errors := verify(m)
	assert errors.len == 0, 'expected no errors for valid use-def chain, got: ${errors}'

	// Check that uses were registered
	assert result in m.values[const1].uses, 'const1 should have result in uses'
	assert result in m.values[const2].uses, 'const2 should have result in uses'
}
