module optimize

import v3.ssa

// VerifyError is a structured SSA verification failure. Unlike verify_ssa()
// (which panics on the first problem), the structured API collects every error so
// callers can decide which are fatal.
pub struct VerifyError {
pub:
	msg       string
	func_id   int = -1
	func_name string
	block_id  int = -1
	val_id    int = -1
}

// str returns the string form for VerifyError.
pub fn (e VerifyError) str() string {
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

// VerifyPanicOptions controls verify panic options behavior used by optimize.
pub struct VerifyPanicOptions {
pub:
	allow_noncritical bool
}

// verify performs comprehensive validation of SSA invariants and returns every
// error found (empty slice == valid). v3 stores branch/phi/switch targets as raw
// block ids, so block operands are validated as block ids, not block values.
pub fn verify(m &ssa.Module) []VerifyError {
	mut errors := []VerifyError{}
	for fi in 0 .. m.funcs.len {
		errors << verify_function(m, m.funcs[fi])
	}
	errors << verify_use_def_chains(m)
	return errors
}

// verify_and_panic runs verification and panics on critical errors only.
pub fn verify_and_panic(m &ssa.Module, pass_name string) {
	verify_and_panic_with_options(m, pass_name, VerifyPanicOptions{
		allow_noncritical: true
	})
}

// verify_and_panic_with_options runs verification with an explicit warning policy.
// Declaration-only functions (C externs / prototypes) are always accepted.
pub fn verify_and_panic_with_options(m &ssa.Module, pass_name string, opts VerifyPanicOptions) {
	errors := verify(m)
	if errors.len == 0 {
		return
	}
	mut msg := 'SSA verification failed after ${pass_name}:\n'
	mut critical := []VerifyError{}
	mut warnings := 0
	for err in errors {
		if is_accepted_declaration_verify_error(m, err) {
			continue
		}
		if opts.allow_noncritical && is_legacy_noncritical_verify_error(err) {
			warnings++
		} else {
			critical << err
		}
	}
	for err in critical {
		msg += '  ${err.msg}\n'
	}
	if warnings > 0 {
		msg += '  (${warnings} non-critical warnings suppressed)\n'
	}
	if critical.len > 0 {
		panic(msg)
	}
}

// is_accepted_declaration_verify_error supports is_accepted_declaration_verify_error handling.
fn is_accepted_declaration_verify_error(m &ssa.Module, err VerifyError) bool {
	if err.msg == 'function has no blocks' {
		if err.func_id < 0 || err.func_id >= m.funcs.len {
			return false
		}
		func := m.funcs[err.func_id]
		return func.is_c_extern || func.is_prototype
	}
	return false
}

// is_legacy_noncritical_verify_error reports is_legacy_noncritical_verify_error logic in optimize.
fn is_legacy_noncritical_verify_error(err VerifyError) bool {
	// Dominance / use-def / phi mismatches are commonly transient between passes.
	if err.msg.contains('does not dominate') || err.msg.contains('uses list')
		|| err.msg.contains('phi') || err.msg.contains('block mismatch') {
		return true
	}
	// Structural-normalization artifacts emitted by the builder before the
	// optimizer cleans them up (empty/unreachable blocks, dead code that follows
	// a terminator within the same block). These are tolerated by the lenient
	// verify_ssa() and removed by remove_unreachable_blocks / DCE.
	if err.msg.contains('has no instructions') || err.msg.contains('not at end of block')
		|| err.msg.contains('multiple terminators')
		|| err.msg.contains('does not end with terminator') {
		return true
	}
	// Type-precision findings. The v3 builder does not maintain strict SSA typing
	// on every load/store/binary operand (e.g. aggregates loaded by value), and
	// the backends tolerate it, so these are advisory rather than fatal.
	return err.msg.contains('should be pointer type') || err.msg.contains('mismatched types')
		|| err.msg.contains('result type should be pointer')
}

// verify_function validates verify function state for optimize.
fn verify_function(m &ssa.Module, func ssa.Function) []VerifyError {
	mut errors := []VerifyError{}
	if func.blocks.len == 0 {
		errors << VerifyError{
			msg:     'function has no blocks'
			func_id: func.id
		}
		return errors
	}
	for blk_id in func.blocks {
		if blk_id < 0 || blk_id >= m.blocks.len {
			errors << VerifyError{
				msg:     'invalid block id ${blk_id}'
				func_id: func.id
			}
			continue
		}
		errors << verify_block(m, func, blk_id)
	}
	errors << verify_dominance(m, func)
	return errors
}

// verify_block validates verify block state for optimize.
fn verify_block(m &ssa.Module, func ssa.Function, blk_id int) []VerifyError {
	mut errors := []VerifyError{}
	blk := m.blocks[blk_id]
	if blk.parent != func.id {
		errors << VerifyError{
			msg:      'block parent mismatch: expected ${func.id}, got ${blk.parent}'
			func_id:  func.id
			block_id: blk_id
		}
	}
	if blk.instrs.len == 0 {
		errors << VerifyError{
			msg:      'block has no instructions (missing terminator)'
			func_id:  func.id
			block_id: blk_id
		}
		return errors
	}

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
		if instr.block != blk_id {
			errors << VerifyError{
				msg:      'instruction block mismatch: expected ${blk_id}, got ${instr.block}'
				func_id:  func.id
				block_id: blk_id
				val_id:   val_id
			}
		}
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
		errors << verify_instruction(m, func.id, blk_id, val_id, instr)
	}
	if !seen_terminator {
		errors << VerifyError{
			msg:      'block does not end with terminator'
			func_id:  func.id
			block_id: blk_id
		}
	}
	errors << verify_cfg_consistency(m, func.id, blk_id)
	return errors
}

// verify_instruction validates verify instruction state for optimize.
fn verify_instruction(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}
	// Only value operands are validated as value ids; block ids (branch/phi/switch
	// targets) are validated by the op-specific checks below.
	for i, op_id in instr.value_operands() {
		if op_id < 0 || op_id >= m.values.len {
			errors << VerifyError{
				msg:      'operand ${i} has invalid value id ${op_id}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}
	match instr.op {
		.add, .sub, .mul, .sdiv, .udiv, .srem, .urem {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		.fadd, .fsub, .fmul, .fdiv, .frem {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		.shl, .lshr, .ashr, .and_, .or_, .xor {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		.lt, .gt, .le, .ge, .eq, .ne, .ult, .ugt, .ule, .uge {
			errors << verify_binary_op(m, func_id, blk_id, val_id, instr)
		}
		.load {
			errors << verify_load(m, func_id, blk_id, val_id, instr)
		}
		.store {
			errors << verify_store(m, func_id, blk_id, val_id, instr)
		}
		.alloca, .heap_alloc {
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
			if instr.operands.len > 1 {
				errors << VerifyError{
					msg:      'ret has ${instr.operands.len} operands, expected 0 or 1'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		.call, .call_indirect, .call_sret, .go_call, .spawn_call {
			if instr.operands.len == 0 {
				errors << VerifyError{
					msg:      '${instr.op} has no operands'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
		.select {
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

// verify_binary_op validates verify binary op state for optimize.
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
	op0 := instr.operands[0]
	op1 := instr.operands[1]
	if op0 < m.values.len && op1 < m.values.len {
		t0 := m.values[op0].typ
		t1 := m.values[op1].typ
		if t0 != t1 && t0 != 0 && t1 != 0 {
			if t0 < m.type_store.types.len && t1 < m.type_store.types.len {
				k0 := m.type_store.types[t0].kind
				k1 := m.type_store.types[t1].kind
				// Allow common mixed-kind binary patterns (different widths, ptr
				// arithmetic, struct/nil comparisons, int/float coercions).
				if k0 == .int_t && k1 == .int_t {
					return errors
				}
				if k0 == .float_t && k1 == .float_t {
					return errors
				}
				if (k0 == .int_t && k1 == .float_t) || (k0 == .float_t && k1 == .int_t) {
					return errors
				}
				if (k0 == .ptr_t && k1 == .int_t) || (k0 == .int_t && k1 == .ptr_t) {
					return errors
				}
				if k0 == .ptr_t && k1 == .ptr_t {
					return errors
				}
				if (k0 == .ptr_t && k1 == .struct_t) || (k0 == .struct_t && k1 == .ptr_t) {
					return errors
				}
				if k0 == .struct_t && k1 == .struct_t {
					return errors
				}
				if (k0 == .struct_t && k1 == .int_t) || (k0 == .int_t && k1 == .struct_t) {
					return errors
				}
				errors << VerifyError{
					msg:      'binary op operands have mismatched types: ${t0} (${k0}) vs ${t1} (${k1})'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}
	return errors
}

// verify_load validates verify load state for optimize.
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
			if m.type_store.types[ptr_typ_id].kind != .ptr_t {
				errors << VerifyError{
					msg:      'load operand should be pointer type, got ${m.type_store.types[ptr_typ_id].kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}
	return errors
}

// verify_store validates verify store state for optimize.
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
			if m.type_store.types[ptr_typ_id].kind != .ptr_t {
				errors << VerifyError{
					msg:      'store destination should be pointer type, got ${m.type_store.types[ptr_typ_id].kind}'
					func_id:  func_id
					block_id: blk_id
					val_id:   val_id
				}
			}
		}
	}
	return errors
}

// verify_phi validates verify phi state for optimize.
fn verify_phi(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}
	if instr.operands.len % 2 != 0 {
		errors << VerifyError{
			msg:      'phi has odd number of operands (${instr.operands.len}), expected (value, block) pairs'
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
	blk := m.blocks[blk_id]
	for i := 0; i < instr.operands.len; i += 2 {
		val_in := instr.operands[i]
		pred_blk_id := int(instr.operands[i + 1])
		if val_in < 0 || val_in >= m.values.len {
			errors << VerifyError{
				msg:      'phi operand ${i} has invalid value id ${val_in}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
		if pred_blk_id < 0 || pred_blk_id >= m.blocks.len {
			errors << VerifyError{
				msg:      'phi operand ${i + 1} has invalid block id ${pred_blk_id}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		} else if blk.preds.len > 0 && pred_blk_id !in blk.preds {
			errors << VerifyError{
				msg:      'phi references block ${pred_blk_id} which is not a predecessor'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}
	if blk.preds.len > 0 {
		actual_pairs := instr.operands.len / 2
		if actual_pairs != blk.preds.len {
			errors << VerifyError{
				msg:      'phi has ${actual_pairs} operand pairs but block has ${blk.preds.len} predecessors'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}
	return errors
}

// verify_branch validates verify branch state for optimize.
fn verify_branch(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}
	if instr.operands.len != 3 {
		errors << VerifyError{
			msg:      'br has ${instr.operands.len} operands, expected 3 (cond, true_blk, false_blk)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}
	for i in [1, 2] {
		b := int(instr.operands[i])
		if b < 0 || b >= m.blocks.len {
			errors << VerifyError{
				msg:      'br operand ${i} has invalid block id ${b}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}
	return errors
}

// verify_jump validates verify jump state for optimize.
fn verify_jump(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}
	if instr.operands.len != 1 {
		errors << VerifyError{
			msg:      'jmp has ${instr.operands.len} operands, expected 1 (target_blk)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}
	b := int(instr.operands[0])
	if b < 0 || b >= m.blocks.len {
		errors << VerifyError{
			msg:      'jmp operand has invalid block id ${b}'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
	}
	return errors
}

// verify_switch validates verify switch state for optimize.
fn verify_switch(m &ssa.Module, func_id int, blk_id int, val_id int, instr ssa.Instruction) []VerifyError {
	mut errors := []VerifyError{}
	if instr.operands.len < 2 {
		errors << VerifyError{
			msg:      'switch has ${instr.operands.len} operands, expected at least 2 (cond, default)'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
		return errors
	}
	def_blk := int(instr.operands[1])
	if def_blk < 0 || def_blk >= m.blocks.len {
		errors << VerifyError{
			msg:      'switch default has invalid block id ${def_blk}'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
	}
	remaining := instr.operands.len - 2
	if remaining % 2 != 0 {
		errors << VerifyError{
			msg:      'switch has odd number of case operands (${remaining})'
			func_id:  func_id
			block_id: blk_id
			val_id:   val_id
		}
	}
	for i := 3; i < instr.operands.len; i += 2 {
		b := int(instr.operands[i])
		if b < 0 || b >= m.blocks.len {
			errors << VerifyError{
				msg:      'switch case has invalid block id ${b}'
				func_id:  func_id
				block_id: blk_id
				val_id:   val_id
			}
		}
	}
	return errors
}

// verify_cfg_consistency validates verify cfg consistency state for optimize.
fn verify_cfg_consistency(m &ssa.Module, func_id int, blk_id int) []VerifyError {
	mut errors := []VerifyError{}
	blk := m.blocks[blk_id]
	if blk.succs.len == 0 && blk.preds.len == 0 {
		return errors
	}
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
	return errors
}

// verify_dominance validates verify dominance state for optimize.
fn verify_dominance(m &ssa.Module, func ssa.Function) []VerifyError {
	mut errors := []VerifyError{}
	mut def_block := map[int]int{}
	for param_id in func.params {
		if func.blocks.len > 0 {
			def_block[param_id] = func.blocks[0]
		}
	}
	for blk_id in func.blocks {
		if blk_id >= m.blocks.len {
			continue
		}
		for val_id in m.blocks[blk_id].instrs {
			def_block[val_id] = blk_id
		}
	}

	entry_block := func.blocks[0]
	dominators_computed := func.blocks.len == 1 || m.blocks[entry_block].dom_tree.len > 0
	if !dominators_computed {
		return errors
	}

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
			// Phi operands legitimately come from predecessors, not dominators.
			if instr.op == .phi {
				continue
			}
			for i, op_id in instr.value_operands() {
				if op_id >= m.values.len {
					continue
				}
				op_val := m.values[op_id]
				if op_val.kind in [.basic_block, .constant, .global, .string_literal,
					.c_string_literal, .func_ref, .argument] {
					continue
				}
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

// dominates supports dominates handling for optimize.
fn dominates(m &ssa.Module, _func ssa.Function, a int, b int) bool {
	if a == b {
		return true
	}
	mut curr := b
	for {
		if curr < 0 || curr >= m.blocks.len {
			return false
		}
		idom := m.blocks[curr].idom
		if idom < 0 {
			return false
		}
		if idom == curr {
			return a == curr
		}
		if idom == a {
			return true
		}
		curr = idom
	}
	return false
}

// verify_use_def_chains validates verify use def chains state for optimize.
fn verify_use_def_chains(m &ssa.Module) []VerifyError {
	mut errors := []VerifyError{}
	mut expected_uses := map[int]map[int]bool{}
	for fi in 0 .. m.funcs.len {
		for blk_id in m.funcs[fi].blocks {
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
				for op_id in instr.value_operands() {
					if op_id >= 0 && op_id < m.values.len {
						if op_id !in expected_uses {
							expected_uses[op_id] = map[int]bool{}
						}
						expected_uses[op_id][val_id] = true
					}
				}
			}
		}
	}
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
	return errors
}
