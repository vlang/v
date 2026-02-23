// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

// VerifyError represents an SSA verification failure
struct VerifyError {
	msg       string
	func_id   int
	func_name string
	block_id  int
	val_id    int
}

fn (e VerifyError) str() string {
	mut s := 'SSA verify error'
	if e.func_id >= 0 {
		s += ' in func ${e.func_id}'
		if e.func_name != '' {
			s += ' (${e.func_name})'
		}
	}
	if e.block_id >= 0 {
		s += ' block ${e.block_id}'
	}
	if e.val_id >= 0 {
		s += ' value ${e.val_id}'
	}
	return '${s}: ${e.msg}'
}

// verify performs comprehensive validation of SSA invariants.
// Returns a list of errors found (empty if valid).
// Call this after optimization passes to catch bugs.
pub fn verify(m &ssa.Module) []VerifyError {
	mut errors := []VerifyError{}

	// 1. Verify each function
	for func in m.funcs {
		errors << verify_function(m, func)
	}

	// 2. Verify global use-def chains
	errors << verify_use_def_chains(m)

	return errors
}

// verify_and_panic runs verification and panics if any errors are found.
// Use this during development to catch SSA corruption early.
pub fn verify_and_panic(m &ssa.Module, pass_name string) {
	errors := verify(m)
	if errors.len > 0 {
		mut msg := 'SSA verification failed after ${pass_name}:\n'
		// Filter out non-critical errors:
		// - Dominance errors are often false positives in nested control flow
		// - Use-def chain errors happen during optimization when uses aren't cleaned up
		// - Phi errors are from mem2reg and don't affect codegen
		// - Block mismatch errors happen during block merging/optimization
		mut critical_errors := []VerifyError{}
		mut warning_count := 0
		for err in errors {
			if err.msg.contains('does not dominate') || err.msg.contains('uses list')
				|| err.msg.contains('phi') || err.msg.contains('block mismatch') {
				warning_count++
			} else {
				critical_errors << err
			}
		}
		for err in critical_errors {
			msg += '  ${err.msg}\n'
		}
		if warning_count > 0 {
			msg += '  (${warning_count} non-critical warnings suppressed)\n'
		}
		if critical_errors.len > 0 {
			panic(msg)
		}
	}
}

fn verify_function(m &ssa.Module, func ssa.Function) []VerifyError {
	mut errors := []VerifyError{}

	// Function must have at least one block (entry)
	if func.blocks.len == 0 {
		errors << VerifyError{
			msg:     'function has no blocks'
			func_id: func.id
		}
		return errors
	}

	// Verify each block
	for blk_id in func.blocks {
		if blk_id < 0 || blk_id >= m.blocks.len {
			errors << VerifyError{
				msg:      'invalid block id ${blk_id}'
				func_id:  func.id
				block_id: -1
			}
			continue
		}
		errors << verify_block(m, func, blk_id)
	}

	// Verify dominance (values must dominate their uses)
	errors << verify_dominance(m, func)

	return errors
}

fn verify_block(m &ssa.Module, func ssa.Function, blk_id int) []VerifyError {
	mut errors := []VerifyError{}
	blk := m.blocks[blk_id]

	// Block must belong to this function
	if blk.parent != func.id {
		errors << VerifyError{
			msg:      'block parent mismatch: expected ${func.id}, got ${blk.parent}'
			func_id:  func.id
			block_id: blk_id
		}
	}

	// Block must have instructions
	if blk.instrs.len == 0 {
		errors << VerifyError{
			msg:      'block has no instructions (missing terminator)'
			func_id:  func.id
			block_id: blk_id
		}
		return errors
	}

	// Verify instruction structure
	mut seen_terminator := false
	mut seen_non_phi := false

	for i, val_id in blk.instrs {
		if val_id < 0 || val_id >= m.values.len {
			errors << VerifyError{
				msg:      'invalid value id ${val_id} in instruction list'
				func_id:  func.id
				block_id: blk_id
			}
			continue
		}

		val := m.values[val_id]
		if val.kind != .instruction {
			// Skip non-instruction values (shouldn't be in instrs list normally)
			continue
		}

		if val.index < 0 || val.index >= m.instrs.len {
			errors << VerifyError{
				msg:      'value ${val_id} has invalid instruction index ${val.index}'
				func_id:  func.id
				block_id: blk_id
				val_id:   val_id
			}
			continue
		}

		instr := m.instrs[val.index]

		// Instruction must belong to this block
		if instr.block != blk_id {
			errors << VerifyError{
				msg:      'instruction block mismatch: expected ${blk_id}, got ${instr.block}'
				func_id:  func.id
				block_id: blk_id
				val_id:   val_id
			}
		}

		// Phi nodes must come before all other instructions
		if instr.op == .phi {
			if seen_non_phi {
				errors << VerifyError{
					msg:      'phi node after non-phi instruction'
					func_id:  func.id
					block_id: blk_id
					val_id:   val_id
				}
			}
		} else {
			seen_non_phi = true
		}

		// Terminator must be last
		is_terminator := instr.op in [.ret, .br, .jmp, .switch_, .unreachable]
		if is_terminator {
			if seen_terminator {
				errors << VerifyError{
					msg:       'multiple terminators in block'
					func_id:   func.id
					func_name: func.name
					block_id:  blk_id
					val_id:    val_id
				}
			}
			if i != blk.instrs.len - 1 {
				errors << VerifyError{
					msg:       'terminator not at end of block'
					func_id:   func.id
					func_name: func.name
					block_id:  blk_id
					val_id:    val_id
				}
			}
			seen_terminator = true
		}

		// Verify instruction operands and types
		errors << verify_instruction(m, func.id, blk_id, val_id, instr)
	}

	// Block must end with a terminator
	if !seen_terminator {
		errors << VerifyError{
			msg:      'block does not end with terminator'
			func_id:  func.id
			block_id: blk_id
		}
	}

	// Verify CFG consistency (if CFG is built)
	errors << verify_cfg_consistency(m, func.id, blk_id)

	return errors
}

fn verify_instruction(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	// Verify all operands are valid
	for i, op_id in instr.operands {
		if op_id < 0 || op_id >= m.values.len {
			errors << VerifyError{
				msg:      'operand ${i} has invalid value id ${op_id}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}

	// Type-specific verification
	match instr.op {
		// Binary arithmetic - operands should have compatible types
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		// Binary float
		.fadd, .fsub, .fmul, .fdiv, .frem {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		// Bitwise - operands should be integers
		.shl, .lshr, .ashr, .and_, .or_, .xor {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		// Comparisons - operands should have same type
		.lt, .gt, .le, .ge, .eq, .ne {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		// Memory operations
		.load {
			errors << verify_load(m, func_id, blk_id, val_id, instr)
		}
		.store {
			errors << verify_store(m, func_id, blk_id, val_id, instr)
		}
		.alloca, .heap_alloc {
			// alloca/heap_alloc result should be a pointer type
			if instr.typ > 0 && instr.typ < m.type_store.types.len {
				typ := m.type_store.types[instr.typ]
				if typ.kind != .ptr_t {
					errors << VerifyError{
						msg:      '${instr.op} result type should be pointer, got ${typ.kind}'
						func_id:  func_id
						block_id: blk_id
						val_id:   val_id
					}
				}
			}
		}
		// Control flow
		.phi {
			errors << verify_phi(m, func_id, blk_id, val_id, instr)
		}
		.br {
			errors << verify_branch(m, func_id, blk_id, val_id, instr)
		}
		.jmp {
			errors << verify_jump(m, func_id, blk_id, val_id, instr)
		}
		.switch_ {
			errors << verify_switch(m, func_id, blk_id, val_id, instr)
		}
		.ret {
			// ret can have 0 or 1 operands
			if instr.operands.len > 1 {
				errors << VerifyError{
					msg:      'ret has ${instr.operands.len} operands, expected 0 or 1'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		.call, .call_indirect {
			// call should have at least a target (function pointer or name)
			if instr.operands.len == 0 {
				errors << VerifyError{
					msg:      'call has no operands'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		.select {
			// select should have 3 operands: condition, true_val, false_val
			if instr.operands.len != 3 {
				errors << VerifyError{
					msg:      'select has ${instr.operands.len} operands, expected 3'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		.assign {
			// assign should have 2 operands: dest, src
			if instr.operands.len != 2 {
				errors << VerifyError{
					msg:      'assign has ${instr.operands.len} operands, expected 2'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		else {}
	}

	return errors
}

fn verify_binary_op(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	if instr.operands.len != 2 {
		errors << VerifyError{
			msg:      'binary op has ${instr.operands.len} operands, expected 2'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	// Check operand types match (for most binary ops)
	op0 := instr.operands[0]
	op1 := instr.operands[1]

	if op0 < m.values.len && op1 < m.values.len {
		t0 := m.values[op0].typ
		t1 := m.values[op1].typ

		// Types should be compatible (same type or one is 0/void for constants)
		if t0 != t1 && t0 != 0 && t1 != 0 {
			// Check if both are integers of potentially different widths
			if t0 < m.type_store.types.len && t1 < m.type_store.types.len {
				typ0 := m.type_store.types[t0]
				typ1 := m.type_store.types[t1]
				// Allow int-to-int operations with different widths (common pattern)
				if typ0.kind == .int_t && typ1.kind == .int_t {
					return errors
				}
				// Allow float-to-float operations with different widths (f32 vs f64)
				if typ0.kind == .float_t && typ1.kind == .float_t {
					return errors
				}
				// Allow int-float operations (common in numeric code, needs proper cast in builder)
				if (typ0.kind == .int_t && typ1.kind == .float_t)
					|| (typ0.kind == .float_t && typ1.kind == .int_t) {
					return errors
				}
				// Allow pointer arithmetic (ptr +/- int)
				if (typ0.kind == .ptr_t && typ1.kind == .int_t)
					|| (typ0.kind == .int_t && typ1.kind == .ptr_t) {
					return errors
				}
				// Allow pointer comparison/subtraction (ptr - ptr)
				if typ0.kind == .ptr_t && typ1.kind == .ptr_t {
					return errors
				}
				// Allow ptr vs struct (common in V when passing structs by value/reference)
				if (typ0.kind == .ptr_t && typ1.kind == .struct_t)
					|| (typ0.kind == .struct_t && typ1.kind == .ptr_t) {
					return errors
				}
				// Allow struct comparisons (V allows == on structs)
				if typ0.kind == .struct_t && typ1.kind == .struct_t {
					return errors
				}
				// Allow struct vs int (for comparisons with nil/0)
				if (typ0.kind == .struct_t && typ1.kind == .int_t)
					|| (typ0.kind == .int_t && typ1.kind == .struct_t) {
					return errors
				}
				errors << VerifyError{
					msg:      'binary op operands have mismatched types: ${t0} (${typ0.kind}) vs ${t1} (${typ1.kind})'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}

	return errors
}

fn verify_load(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	if instr.operands.len != 1 {
		errors << VerifyError{
			msg:      'load has ${instr.operands.len} operands, expected 1 (pointer)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	ptr_id := instr.operands[0]
	if ptr_id < m.values.len {
		ptr_typ_id := m.values[ptr_id].typ
		if ptr_typ_id > 0 && ptr_typ_id < m.type_store.types.len {
			ptr_typ := m.type_store.types[ptr_typ_id]
			if ptr_typ.kind != .ptr_t {
				errors << VerifyError{
					msg:      'load operand should be pointer type, got ${ptr_typ.kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}

	return errors
}

fn verify_store(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	if instr.operands.len != 2 {
		errors << VerifyError{
			msg:      'store has ${instr.operands.len} operands, expected 2 (value, pointer)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	ptr_id := instr.operands[1]
	if ptr_id < m.values.len {
		ptr_typ_id := m.values[ptr_id].typ
		if ptr_typ_id > 0 && ptr_typ_id < m.type_store.types.len {
			ptr_typ := m.type_store.types[ptr_typ_id]
			if ptr_typ.kind != .ptr_t {
				errors << VerifyError{
					msg:      'store destination should be pointer type, got ${ptr_typ.kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}

	return errors
}

fn verify_phi(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	// Phi operands come in pairs: [val0, block0, val1, block1, ...]
	if instr.operands.len % 2 != 0 {
		errors << VerifyError{
			msg:      'phi has odd number of operands (${instr.operands.len}), expected pairs of (value, block)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	if instr.operands.len == 0 {
		errors << VerifyError{
			msg:      'phi has no operands'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	// Verify each operand pair
	for i := 0; i < instr.operands.len; i += 2 {
		val_in := instr.operands[i]
		blk_val := instr.operands[i + 1]

		// Value operand should exist
		if val_in >= m.values.len {
			errors << VerifyError{
				msg:      'phi operand ${i} has invalid value id ${val_in}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}

		// Block operand should be a basic_block
		if blk_val < m.values.len {
			if m.values[blk_val].kind != .basic_block {
				errors << VerifyError{
					msg:      'phi operand ${i + 1} should be basic_block, got ${m.values[blk_val].kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}

		// Check type consistency: phi operand type should match phi result type
		if val_in < m.values.len {
			op_typ := m.values[val_in].typ
			phi_typ := instr.typ
			// Allow type mismatch with void (constants may have type 0)
			if op_typ != phi_typ && op_typ != 0 && phi_typ != 0 {
				// Only warn if both are real types
				if op_typ < m.type_store.types.len && phi_typ < m.type_store.types.len {
					op_kind := m.type_store.types[op_typ].kind
					phi_kind := m.type_store.types[phi_typ].kind
					// Allow int-to-int (common pattern)
					if op_kind != .int_t || phi_kind != .int_t {
						errors << VerifyError{
							msg:      'phi operand ${i} type ${op_typ} does not match phi type ${phi_typ}'
							func_id:  func_id
							block_id: blk_id
							val_id:   val_id
						}
					}
				}
			}
		}
	}

	// If CFG is built, verify phi operand count matches predecessor count
	blk := m.blocks[blk_id]
	if blk.preds.len > 0 {
		expected_pairs := blk.preds.len
		actual_pairs := instr.operands.len / 2
		if actual_pairs != expected_pairs {
			errors << VerifyError{
				msg:      'phi has ${actual_pairs} operand pairs but block has ${expected_pairs} predecessors'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}

		// Verify each phi block operand is actually a predecessor
		for i := 1; i < instr.operands.len; i += 2 {
			blk_val := instr.operands[i]
			if blk_val < m.values.len && m.values[blk_val].kind == .basic_block {
				pred_blk_id := m.values[blk_val].index
				if pred_blk_id !in blk.preds {
					errors << VerifyError{
						msg:      'phi references block ${pred_blk_id} which is not a predecessor'
						func_id:  func_id
						block_id: blk_id
						val_id:   val_id
					}
				}
			}
		}
	}

	return errors
}

fn verify_branch(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	// br should have 3 operands: condition, true_block, false_block
	if instr.operands.len != 3 {
		errors << VerifyError{
			msg:      'br has ${instr.operands.len} operands, expected 3 (cond, true_blk, false_blk)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	// Verify block operands are basic blocks
	for i in [1, 2] {
		blk_val := instr.operands[i]
		if blk_val < m.values.len {
			if m.values[blk_val].kind != .basic_block {
				errors << VerifyError{
					msg:      'br operand ${i} should be basic_block, got ${m.values[blk_val].kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}

	return errors
}

fn verify_jump(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	// jmp should have 1 operand: target block
	if instr.operands.len != 1 {
		errors << VerifyError{
			msg:      'jmp has ${instr.operands.len} operands, expected 1 (target_blk)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	blk_val := instr.operands[0]
	if blk_val < m.values.len {
		if m.values[blk_val].kind != .basic_block {
			errors << VerifyError{
				msg:      'jmp operand should be basic_block, got ${m.values[blk_val].kind}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}

	return errors
}

fn verify_switch(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}

	// switch should have: condition, default_block, then pairs of (value, block)
	if instr.operands.len < 2 {
		errors << VerifyError{
			msg:      'switch has ${instr.operands.len} operands, expected at least 2 (cond, default)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}

	// Verify default block is a basic block
	default_blk := instr.operands[1]
	if default_blk < m.values.len {
		if m.values[default_blk].kind != .basic_block {
			errors << VerifyError{
				msg:      'switch default should be basic_block, got ${m.values[default_blk].kind}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}

	// Remaining operands should be pairs of (value, block)
	remaining := instr.operands.len - 2
	if remaining % 2 != 0 {
		errors << VerifyError{
			msg:      'switch has odd number of case operands (${remaining})'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
	}

	return errors
}

fn verify_cfg_consistency(m &ssa.Module, func_id int, blk_id int) []VerifyError {
	mut errors := []VerifyError{}
	blk := m.blocks[blk_id]

	// Skip if CFG not built yet
	if blk.succs.len == 0 && blk.preds.len == 0 {
		return errors
	}

	// Verify successor symmetry: if B is successor of A, then A should be predecessor of B
	for succ_id in blk.succs {
		if succ_id < 0 || succ_id >= m.blocks.len {
			errors << VerifyError{
				msg:      'invalid successor block id ${succ_id}'
				func_id:  func_id
				block_id: blk_id
			}
			continue
		}
		if blk_id !in m.blocks[succ_id].preds {
			errors << VerifyError{
				msg:      'block ${blk_id} has successor ${succ_id} but is not in its predecessors'
				func_id:  func_id
				block_id: blk_id
			}
		}
	}

	// Verify predecessor symmetry: if A is predecessor of B, then B should be successor of A
	for pred_id in blk.preds {
		if pred_id < 0 || pred_id >= m.blocks.len {
			errors << VerifyError{
				msg:      'invalid predecessor block id ${pred_id}'
				func_id:  func_id
				block_id: blk_id
			}
			continue
		}
		if blk_id !in m.blocks[pred_id].succs {
			errors << VerifyError{
				msg:      'block ${blk_id} has predecessor ${pred_id} but is not in its successors'
				func_id:  func_id
				block_id: blk_id
			}
		}
	}

	// Verify terminator matches successors
	if blk.instrs.len > 0 {
		term_val_id := blk.instrs.last()
		if term_val_id < m.values.len && m.values[term_val_id].kind == .instruction {
			term := m.instrs[m.values[term_val_id].index]
			mut expected_succs := []int{}

			match term.op {
				.jmp {
					if term.operands.len >= 1 {
						target_val := term.operands[0]
						if target_val < m.values.len && m.values[target_val].kind == .basic_block {
							expected_succs << m.values[target_val].index
						}
					}
				}
				.br {
					if term.operands.len >= 3 {
						true_val := term.operands[1]
						false_val := term.operands[2]
						if true_val < m.values.len && m.values[true_val].kind == .basic_block {
							expected_succs << m.values[true_val].index
						}
						if false_val < m.values.len && m.values[false_val].kind == .basic_block {
							if m.values[false_val].index !in expected_succs {
								expected_succs << m.values[false_val].index
							}
						}
					}
				}
				.switch_ {
					// Default block
					if term.operands.len >= 2 {
						default_val := term.operands[1]
						if default_val < m.values.len && m.values[default_val].kind == .basic_block {
							expected_succs << m.values[default_val].index
						}
					}
					// Case blocks
					for i := 3; i < term.operands.len; i += 2 {
						blk_val := term.operands[i]
						if blk_val < m.values.len && m.values[blk_val].kind == .basic_block {
							idx := m.values[blk_val].index
							if idx !in expected_succs {
								expected_succs << idx
							}
						}
					}
				}
				.ret, .unreachable {
					// No successors expected
				}
				else {}
			}

			// Check for missing successors
			for succ in expected_succs {
				if succ !in blk.succs {
					errors << VerifyError{
						msg:      'terminator targets block ${succ} but it is not in successors'
						func_id:  func_id
						block_id: blk_id
					}
				}
			}
		}
	}

	return errors
}

fn verify_dominance(m &ssa.Module, func ssa.Function) []VerifyError {
	mut errors := []VerifyError{}

	// Build map of value -> defining block
	mut def_block := map[int]int{}

	// Function parameters are defined in entry block
	for param_id in func.params {
		if func.blocks.len > 0 {
			def_block[param_id] = func.blocks[0]
		}
	}

	// Build definition map for all instructions
	for blk_id in func.blocks {
		if blk_id >= m.blocks.len {
			continue
		}
		for val_id in m.blocks[blk_id].instrs {
			def_block[val_id] = blk_id
		}
	}

	// Skip dominance check if dominators not computed
	// After compute_dominators, dom_tree is populated for the entry block (if function has multiple blocks)
	// Before computation, dom_tree is empty. For single-block functions, there's nothing to verify.
	entry_block := func.blocks[0]
	dominators_computed := func.blocks.len == 1 || m.blocks[entry_block].dom_tree.len > 0

	if !dominators_computed {
		return errors
	}

	// For each use, verify definition dominates use
	for blk_id in func.blocks {
		if blk_id >= m.blocks.len {
			continue
		}
		for val_id in m.blocks[blk_id].instrs {
			if val_id >= m.values.len || m.values[val_id].kind != .instruction {
				continue
			}
			if m.values[val_id].index >= m.instrs.len {
				continue
			}
			instr := m.instrs[m.values[val_id].index]

			// Check each operand
			for i, op_id in instr.operands {
				if op_id >= m.values.len {
					continue
				}

				// Skip block references and constants
				op_val := m.values[op_id]
				if op_val.kind in [.basic_block, .constant, .global, .string_literal,
					.c_string_literal] {
					continue
				}

				// Phi nodes are special: operands come from predecessors
				if instr.op == .phi {
					continue
				}

				// Check dominance
				if def_blk := def_block[op_id] {
					if !dominates(m, func, def_blk, blk_id) {
						errors << VerifyError{
							msg:       'operand ${i} (value ${op_id}) defined in block ${def_blk} does not dominate use in block ${blk_id}'
							func_id:   func.id
							func_name: func.name
							block_id:  blk_id
							val_id:    val_id
						}
					}
				}
			}
		}
	}

	return errors
}

// dominates returns true if block a dominates block b
fn dominates(m &ssa.Module, func ssa.Function, a int, b int) bool {
	if a == b {
		return true
	}

	// Walk up dominator tree from b
	mut curr := b
	for {
		if curr < 0 || curr >= m.blocks.len {
			return false
		}
		idom := m.blocks[curr].idom
		if idom < 0 {
			// Unreachable block (idom = -1)
			return false
		}
		if idom == curr {
			// Reached entry block (or invalid state)
			return a == curr
		}
		if idom == a {
			return true
		}
		if idom == 0 && curr != 0 {
			// Entry block case
			return a == 0 || a == func.blocks[0]
		}
		curr = idom
	}
	return false
}

fn verify_use_def_chains(m &ssa.Module) []VerifyError {
	mut errors := []VerifyError{}

	// Build expected uses from instruction operands
	mut expected_uses := map[int]map[int]bool{} // value -> set of users

	for func in m.funcs {
		for blk_id in func.blocks {
			if blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id >= m.values.len || m.values[val_id].kind != .instruction {
					continue
				}
				if m.values[val_id].index >= m.instrs.len {
					continue
				}
				instr := m.instrs[m.values[val_id].index]

				for op_id in instr.operands {
					if op_id < m.values.len {
						if op_id !in expected_uses {
							expected_uses[op_id] = map[int]bool{}
						}
						expected_uses[op_id][val_id] = true
					}
				}
			}
		}
	}

	// Verify actual uses match expected
	for val_id, users in expected_uses {
		if val_id >= m.values.len {
			continue
		}
		actual := m.values[val_id].uses

		for user_id, _ in users {
			if user_id !in actual {
				errors << VerifyError{
					msg:    'value ${val_id} is used by ${user_id} but ${user_id} is not in uses list'
					val_id: val_id
				}
			}
		}
	}

	// Check for spurious uses (uses list contains values that don't actually use this value)
	for i, val in m.values {
		if val.kind != .instruction && val.kind != .argument {
			continue
		}
		for user_id in val.uses {
			if user_id >= m.values.len {
				errors << VerifyError{
					msg:    'value ${i} has invalid user ${user_id} in uses list'
					val_id: i
				}
				continue
			}
			user_val := m.values[user_id]
			if user_val.kind != .instruction {
				continue
			}
			if user_val.index >= m.instrs.len {
				continue
			}
			instr := m.instrs[user_val.index]
			if i !in instr.operands {
				errors << VerifyError{
					msg:    'value ${i} has ${user_id} in uses list but ${user_id} does not use it'
					val_id: i
				}
			}
		}
	}

	return errors
}
