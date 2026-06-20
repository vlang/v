import v3.ssa
import v3.ssa.optimize

// Build a function body inside a worker module and merge it back, exercising the
// raw-block-id operand remapping (branch/phi block operands must shift by the
// block offset, value operands by the value offset).
fn test_worker_clone_merge_remaps_block_and_value_operands() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	// A pre-existing (seed) constant and function registered serially.
	_ := m.get_or_add_const(i64t, '111')
	fidx := m.new_function('compute', i64t)

	// Clone a worker and snapshot the seed sizes.
	mut w := m.new_worker_module()
	seed_values := m.values.len
	seed_instrs := m.instrs.len
	seed_blocks := m.blocks.len
	seed_types := m.type_store.types.len
	seed_funcs := m.funcs.len

	// Build a diamond + merge phi entirely inside the worker.
	entry := w.add_block(fidx, 'entry')
	then_b := w.add_block(fidx, 'then')
	else_b := w.add_block(fidx, 'else')
	merge := w.add_block(fidx, 'merge')

	cond := w.get_or_add_const(w.type_store.get_int(1), '1')
	w.add_instr(.br, entry, ssa.TypeID(0), [cond, ssa.ValueID(then_b), ssa.ValueID(else_b)])

	ten := w.get_or_add_const(i64t, '10')
	w.add_instr(.jmp, then_b, ssa.TypeID(0), [ssa.ValueID(merge)])
	twenty := w.get_or_add_const(i64t, '20')
	w.add_instr(.jmp, else_b, ssa.TypeID(0), [ssa.ValueID(merge)])

	// phi: [ten, then_b, twenty, else_b]  (odd operands are raw block ids)
	phi := w.add_instr(.phi, merge, i64t, [ten, ssa.ValueID(then_b), twenty, ssa.ValueID(else_b)])
	w.add_instr(.ret, merge, ssa.TypeID(0), [phi])

	fd := ssa.FuncSSAData{
		func_idx: fidx
		blocks:   w.funcs[fidx].blocks
		params:   w.funcs[fidx].params
	}

	m.merge_worker_module(w, [fd], seed_values, seed_instrs, seed_blocks, seed_types, seed_funcs)

	// The function now has its 4 blocks, all parented correctly.
	assert m.funcs[fidx].blocks.len == 4
	for blk_id in m.funcs[fidx].blocks {
		assert blk_id >= 0 && blk_id < m.blocks.len
		assert m.blocks[blk_id].parent == fidx
	}

	// The merged branch must target real blocks inside the function (this fails if
	// block operands were remapped as values).
	entry_id := m.funcs[fidx].blocks[0]
	term_id := m.blocks[entry_id].instrs.last()
	term := m.instrs[m.values[term_id].index]
	assert term.op == .br
	for oi in [1, 2] {
		tgt := int(term.operands[oi])
		assert tgt >= 0 && tgt < m.blocks.len
		assert tgt in m.funcs[fidx].blocks
	}

	// The whole thing must verify and optimize cleanly (optimize() panics on a
	// broken CFG / dangling phi predecessor, so reaching the asserts proves the
	// remapping is consistent).
	optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
		mem2reg:        false
		eliminate_phis: true
	})
	errors := optimize.verify(m)
	for e in errors {
		assert !e.msg.contains('invalid')
		assert !e.msg.contains('outside function')
	}
}
