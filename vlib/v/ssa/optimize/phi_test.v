module optimize

import v.ssa

struct TestParallelCopy {
	dest int
	src  int
}

fn resolve_test_parallel_copies(mut m ssa.Module, blk_id int, copies []TestParallelCopy) {
	mut dests := []int{cap: copies.len}
	mut srcs := []int{cap: copies.len}
	for copy in copies {
		dests << copy.dest
		srcs << copy.src
	}
	resolve_parallel_copies(mut m, blk_id, dests, srcs)
}

fn collect_parallel_assigns(m &ssa.Module, blk_id int) []TestParallelCopy {
	mut assigns := []TestParallelCopy{}
	for val_id in m.blocks[blk_id].instrs {
		if m.values[val_id].kind != .instruction {
			continue
		}
		instr := m.instrs[m.values[val_id].index]
		if instr.op != .assign {
			continue
		}
		assert instr.operands.len == 2
		assert instr.operands[0] != 0
		assert instr.operands[1] != 0
		assigns << TestParallelCopy{
			dest: instr.operands[0]
			src:  instr.operands[1]
		}
	}
	return assigns
}

fn block_has_phi_temp(m &ssa.Module, blk_id int) bool {
	for val_id in m.blocks[blk_id].instrs {
		if m.values[val_id].name.starts_with('phi_tmp_') {
			return true
		}
	}
	return false
}

fn test_parallel_copy_cycle_materializes_temp_in_block() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0)
	blk_id := m.add_block(fn_id, 'entry')

	c1 := m.get_or_add_const(i64_t, '1')
	c2 := m.get_or_add_const(i64_t, '2')
	a := m.add_instr(.add, blk_id, i64_t, [c1, c2])
	b := m.add_instr(.sub, blk_id, i64_t, [a, c1])
	ret := m.add_instr(.ret, blk_id, 0, [])

	resolve_test_parallel_copies(mut m, blk_id, [
		TestParallelCopy{
			dest: a
			src:  b
		},
		TestParallelCopy{
			dest: b
			src:  a
		},
	])

	mut temp_ids := []int{}
	for val_id in m.blocks[blk_id].instrs {
		if m.values[val_id].name.starts_with('phi_tmp_') {
			temp_ids << val_id
		}
	}
	assigns := collect_parallel_assigns(m, blk_id)

	assert temp_ids.len == 1
	assert assigns.len == 2
	// The temp and the copies are placed before the terminator.
	assert m.blocks[blk_id].instrs.last() == ret

	temp_id := temp_ids[0]
	mut has_temp_src := false
	for copy in assigns {
		if copy.src == temp_id {
			has_temp_src = true
		}
	}
	assert has_temp_src
}

fn test_parallel_copy_acyclic_chain_does_not_create_temp() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0)
	blk_id := m.add_block(fn_id, 'entry')

	c1 := m.get_or_add_const(i64_t, '1')
	c2 := m.get_or_add_const(i64_t, '2')
	b := m.add_instr(.add, blk_id, i64_t, [c1, c2])
	a := m.add_instr(.sub, blk_id, i64_t, [b, c1])
	c := m.add_instr(.mul, blk_id, i64_t, [a, c2])
	m.add_instr(.ret, blk_id, 0, [])

	resolve_test_parallel_copies(mut m, blk_id, [
		TestParallelCopy{
			dest: a
			src:  b
		},
		TestParallelCopy{
			dest: c
			src:  a
		},
	])

	assigns := collect_parallel_assigns(m, blk_id)

	assert !block_has_phi_temp(m, blk_id)
	assert assigns.len == 2
	// `c` must read the old `a` before `a` is overwritten.
	assert assigns[0].dest == c
	assert assigns[0].src == a
	assert assigns[1].dest == a
	assert assigns[1].src == b
}

fn test_parallel_copy_skips_self_copies() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0)
	blk_id := m.add_block(fn_id, 'entry')

	c1 := m.get_or_add_const(i64_t, '1')
	a := m.add_instr(.add, blk_id, i64_t, [c1, c1])
	m.add_instr(.ret, blk_id, 0, [])
	before := m.blocks[blk_id].instrs.clone()

	resolve_test_parallel_copies(mut m, blk_id, [
		TestParallelCopy{
			dest: a
			src:  a
		},
	])

	assert m.blocks[blk_id].instrs == before
}

fn test_eliminate_phi_nodes_cycle_assign_dests_are_phi_results() {
	mut m := ssa.Module.new()
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0)
	entry := m.add_block(fn_id, 'entry')
	join := m.add_block(fn_id, 'join')

	m.add_instr(.jmp, entry, 0, [ssa.ValueID(join)])

	zero := m.get_or_add_const(i64_t, '0')
	one := m.get_or_add_const(i64_t, '1')

	// Phi operands are (value, predecessor block id) pairs.
	phi_a := m.add_instr(.phi, join, i64_t, [zero, ssa.ValueID(entry)])
	phi_b := m.add_instr(.phi, join, i64_t, [phi_a, ssa.ValueID(entry)])
	// Make it a true cycle: a <- b, b <- a.
	m.instrs[m.values[phi_a].index].operands[0] = phi_b
	m.add_instr(.ret, join, 0, [one])
	build_cfg(mut m)

	eliminate_phi_nodes(mut m)

	mut saw_assign := false
	mut saw_temp := false
	for blk_id in m.funcs[fn_id].blocks {
		for val_id in m.blocks[blk_id].instrs {
			if m.values[val_id].name.starts_with('phi_tmp_') {
				saw_temp = true
			}
			if m.values[val_id].kind != .instruction {
				continue
			}
			instr := m.instrs[m.values[val_id].index]
			assert instr.op != .phi
			if instr.op == .assign {
				saw_assign = true
				// Phi destinations stay virtual values that only the copies define.
				dest := instr.operands[0]
				assert dest in [phi_a, phi_b]
				assert m.values[dest].kind == .phi_result
			}
		}
	}

	assert saw_assign
	assert saw_temp
	for err in verify(m) {
		assert !err.msg.contains('assign'), err.str()
	}
}
