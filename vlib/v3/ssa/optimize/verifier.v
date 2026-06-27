module optimize

import v3.ssa

// verify_ssa validates verify ssa state for optimize.
fn verify_ssa(m &ssa.Module, stage string) {
	verify_module_indexes(m, stage)
	verify_functions(m, stage)
	verify_uses(m, stage)
}

// verify_module_indexes validates verify module indexes state for optimize.
fn verify_module_indexes(m &ssa.Module, stage string) {
	for vi in 0 .. m.values.len {
		if m.values[vi].id != vi {
			verify_fail(stage, 'value ${vi} has id ${m.values[vi].id}')
		}
		if m.values[vi].kind == .instruction {
			instr_idx := m.values[vi].index
			if instr_idx < 0 || instr_idx >= m.instrs.len {
				verify_fail(stage,
					'instruction value ${vi} has invalid instruction index ${instr_idx}')
			}
		}
	}
	for bi in 0 .. m.blocks.len {
		if m.blocks[bi].id != bi {
			verify_fail(stage, 'block ${bi} has id ${m.blocks[bi].id}')
		}
	}
	for fi in 0 .. m.funcs.len {
		if m.funcs[fi].id != fi {
			verify_fail(stage, 'function ${fi} has id ${m.funcs[fi].id}')
		}
	}
}

// verify_functions validates verify functions state for optimize.
fn verify_functions(m &ssa.Module, stage string) {
	for fi in 0 .. m.funcs.len {
		func := m.funcs[fi]
		mut func_blocks := map[int]bool{}
		for blk_id in func.blocks {
			if !verify_block_id(m, stage, blk_id, fi, 'function block list') {
				continue
			}
			if func_blocks[blk_id] {
				verify_fail(stage, 'function ${fi} contains duplicate block ${blk_id}')
			}
			func_blocks[blk_id] = true
		}
		for blk_id in func.blocks {
			if !verify_block_id(m, stage, blk_id, fi, 'function block list') {
				continue
			}
			blk := m.blocks[blk_id]
			if blk.parent != fi {
				verify_fail(stage, 'block ${blk_id} parent is ${blk.parent}, expected ${fi}')
			}
			verify_block_instrs(m, stage, fi, blk_id, func_blocks)
			verify_cfg_edges(m, stage, fi, blk_id, func_blocks)
		}
	}
}

// verify_block_instrs validates verify block instrs state for optimize.
fn verify_block_instrs(m &ssa.Module, stage string, fi int, blk_id int, func_blocks map[int]bool) {
	blk := m.blocks[blk_id]
	if blk.instrs.len == 0 {
		return
	}
	for ii, val_id in blk.instrs {
		if val_id <= 0 || val_id >= m.values.len {
			verify_fail(stage, 'block ${blk_id} contains invalid instruction value ${val_id}')
		}
		val := m.values[val_id]
		if val.kind != .instruction {
			verify_fail(stage, 'block ${blk_id} contains non-instruction value ${val_id}')
		}
		if val.index < 0 || val.index >= m.instrs.len {
			verify_fail(stage,
				'instruction value ${val_id} has invalid instruction index ${val.index}')
		}
		instr := m.instrs[val.index]
		if instr.block != blk_id {
			verify_fail(stage,
				'instruction value ${val_id} is in block ${blk_id} but records block ${instr.block}')
		}
		verify_instruction_operands(m, stage, fi, blk_id, val_id, instr, func_blocks,
			ii == blk.instrs.len - 1)
	}
}

// verify_instruction_operands validates verify instruction operands state for optimize.
fn verify_instruction_operands(m &ssa.Module, stage string, fi int, blk_id int, val_id int, instr ssa.Instruction, func_blocks map[int]bool, is_final bool) {
	for op_id in instr.value_operands() {
		if op_id <= 0 || op_id >= m.values.len {
			verify_fail(stage,
				'instruction value ${val_id} in block ${blk_id} uses invalid value ${op_id}')
		}
		if m.values[op_id].kind == .unknown {
			verify_fail(stage,
				'instruction value ${val_id} in block ${blk_id} uses unknown value ${op_id}')
		}
	}
	match instr.op {
		.ret {
			if instr.operands.len > 1 {
				verify_fail(stage,
					'ret value ${val_id} in block ${blk_id} has ${instr.operands.len} operands')
			}
		}
		.jmp {
			if instr.operands.len != 1 {
				verify_fail(stage,
					'jmp value ${val_id} in block ${blk_id} has ${instr.operands.len} operands')
			} else if is_final {
				verify_target_block(m, stage, fi, blk_id, int(instr.operands[0]), func_blocks,
					'jmp')
			}
		}
		.unreachable {
			if instr.operands.len != 0 {
				verify_fail(stage,
					'unreachable value ${val_id} in block ${blk_id} has ${instr.operands.len} operands')
			}
		}
		.br {
			if instr.operands.len != 3 {
				verify_fail(stage,
					'br value ${val_id} in block ${blk_id} has ${instr.operands.len} operands')
			} else if is_final {
				verify_target_block(m, stage, fi, blk_id, int(instr.operands[1]), func_blocks,
					'br true')
				verify_target_block(m, stage, fi, blk_id, int(instr.operands[2]), func_blocks,
					'br false')
			}
		}
		.phi {
			if instr.operands.len == 0 || instr.operands.len % 2 != 0 {
				verify_fail(stage,
					'phi value ${val_id} in block ${blk_id} has ${instr.operands.len} operands')
			}
			for oi := 1; oi < instr.operands.len; oi += 2 {
				verify_target_block(m, stage, fi, blk_id, int(instr.operands[oi]), func_blocks,
					'phi predecessor')
			}
		}
		else {}
	}
}

// verify_target_block validates verify target block state for optimize.
fn verify_target_block(m &ssa.Module, stage string, fi int, blk_id int, target_id int, func_blocks map[int]bool, label string) {
	if !verify_block_id(m, stage, target_id, fi, '${label} target from block ${blk_id}') {
		return
	}
	if !func_blocks[target_id] {
		verify_fail(stage,
			'${label} target ${target_id} from block ${blk_id} is outside function ${fi}')
	}
}

// verify_cfg_edges validates verify cfg edges state for optimize.
fn verify_cfg_edges(m &ssa.Module, stage string, fi int, blk_id int, func_blocks map[int]bool) {
	blk := m.blocks[blk_id]
	mut expected_succs := []int{}
	if blk.instrs.len > 0 {
		term_val_id := blk.instrs.last()
		term := m.instrs[m.values[term_val_id].index]
		match term.op {
			.jmp {
				if term.operands.len == 1 {
					expected_succs << int(term.operands[0])
				}
			}
			.br {
				if term.operands.len == 3 {
					expected_succs << int(term.operands[1])
					if term.operands[2] != term.operands[1] {
						expected_succs << int(term.operands[2])
					}
				}
			}
			else {}
		}
	}
	verify_same_blocks(stage, 'successors for block ${blk_id}', expected_succs, blk.succs)

	for succ in blk.succs {
		if !verify_block_id(m, stage, succ, fi, 'successor of block ${blk_id}') {
			continue
		}
		if !func_blocks[succ] {
			verify_fail(stage, 'successor ${succ} from block ${blk_id} is outside function ${fi}')
		}
		if !arr_contains(m.blocks[succ].preds, blk_id) {
			verify_fail(stage, 'successor ${succ} is missing predecessor ${blk_id}')
		}
	}
	for pred in blk.preds {
		if !verify_block_id(m, stage, pred, fi, 'predecessor of block ${blk_id}') {
			continue
		}
		if !func_blocks[pred] {
			verify_fail(stage, 'predecessor ${pred} of block ${blk_id} is outside function ${fi}')
		}
		if !arr_contains(m.blocks[pred].succs, blk_id) {
			verify_fail(stage, 'predecessor ${pred} is missing successor ${blk_id}')
		}
	}
}

// verify_uses validates verify uses state for optimize.
fn verify_uses(m &ssa.Module, stage string) {
	mut expected_uses := map[int][]int{}
	for func in m.funcs {
		for blk_id in func.blocks {
			if blk_id < 0 || blk_id >= m.blocks.len {
				continue
			}
			for val_id in m.blocks[blk_id].instrs {
				if val_id <= 0 || val_id >= m.values.len || m.values[val_id].kind != .instruction {
					continue
				}
				instr := m.instrs[m.values[val_id].index]
				for op_id in instr.value_operands() {
					if op_id > 0 && op_id < m.values.len {
						if !arr_contains(expected_uses[op_id], val_id) {
							expected_uses[op_id] << val_id
						}
					}
				}
			}
		}
	}
	for vi in 0 .. m.values.len {
		verify_same_blocks(stage, 'uses for value ${vi}', expected_uses[vi], m.values[vi].uses)
	}
}

// verify_block_id validates verify block id state for optimize.
fn verify_block_id(m &ssa.Module, stage string, blk_id int, fi int, context string) bool {
	if blk_id < 0 || blk_id >= m.blocks.len {
		verify_fail(stage, '${context} references invalid block ${blk_id} in function ${fi}')
		return false
	}
	return true
}

// verify_same_blocks validates verify same blocks state for optimize.
fn verify_same_blocks(stage string, label string, expected []int, actual []int) {
	if expected.len != actual.len {
		verify_fail(stage, '${label}: expected ${expected}, got ${actual}')
	}
	for v in expected {
		if !arr_contains(actual, v) {
			verify_fail(stage, '${label}: expected ${expected}, got ${actual}')
		}
	}
	for v in actual {
		if !arr_contains(expected, v) {
			verify_fail(stage, '${label}: expected ${expected}, got ${actual}')
		}
	}
}

// verify_fail validates verify fail state for optimize.
fn verify_fail(stage string, msg string) {
	panic('SSA verifier failed after ${stage}: ${msg}')
}
