// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module optimize

import v2.ssa

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
	mut src_ref_count := []int{len: m.values.len + copies.len + 8}
	mut touched_ids := []int{cap: copies.len + 4}
	resolve_parallel_copies_flat(mut m, blk_id, dests, srcs, mut src_ref_count, mut touched_ids)
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

fn test_parallel_copy_cycle_materializes_temp_in_block() {
	mut m := ssa.Module.new('test_cycle')
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0, [])
	blk_id := m.add_block(fn_id, 'entry')

	c1 := m.get_or_add_const(i64_t, '1')
	c2 := m.get_or_add_const(i64_t, '2')
	a := m.add_instr(.add, blk_id, i64_t, [c1, c2])
	b := m.add_instr(.sub, blk_id, i64_t, [a, c1])
	m.add_instr(.ret, blk_id, 0, [])

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

	assert temp_ids.len > 0
	assert assigns.len == 2

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
	mut m := ssa.Module.new('test_chain')
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0, [])
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

	mut has_temp := false
	for val_id in m.blocks[blk_id].instrs {
		if m.values[val_id].name.starts_with('phi_tmp_') {
			has_temp = true
		}
	}
	assigns := collect_parallel_assigns(m, blk_id)

	assert !has_temp
	assert assigns.len == 2
	assert assigns[0].dest == c
	assert assigns[0].src == a
	assert assigns[1].dest == a
	assert assigns[1].src == b
}

fn test_eliminate_phi_nodes_cycle_assign_dests_are_materialized() {
	mut m := ssa.Module.new('test_phi_elim_cycle')
	i64_t := m.type_store.get_int(64)
	fn_id := m.new_function('f', 0, [])
	entry := m.add_block(fn_id, 'entry')
	join := m.add_block(fn_id, 'join')
	block_val_ids := build_block_val_ids(m)

	join_val := get_block_val_id(m, join, block_val_ids)
	m.add_instr(.jmp, entry, 0, [join_val])

	entry_val := get_block_val_id(m, entry, block_val_ids)
	zero := m.get_or_add_const(i64_t, '0')
	one := m.get_or_add_const(i64_t, '1')

	phi_a := m.add_instr(.phi, join, i64_t, [zero, entry_val])
	phi_b := m.add_instr(.phi, join, i64_t, [phi_a, entry_val])
	// Make it a true cycle: a <- b, b <- a.
	m.instrs[m.values[phi_a].index].operands[0] = phi_b
	m.add_instr(.ret, join, 0, [one])

	eliminate_phi_nodes(mut m)

	mut materialized := map[int]bool{}
	for blk_id in m.funcs[fn_id].blocks {
		for val_id in m.blocks[blk_id].instrs {
			materialized[val_id] = true
		}
	}

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
			if instr.op == .assign {
				saw_assign = true
				dest := instr.operands[0]
				assert dest in materialized
			}
		}
	}

	assert saw_assign
	assert saw_temp
}
