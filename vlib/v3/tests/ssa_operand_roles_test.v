import v3.ssa
import v3.ssa.optimize

fn test_replace_uses_only_rewrites_value_operands_when_ids_collide() {
	mut m := ssa.Module.new()
	i1 := m.type_store.get_int(1)
	fidx := m.new_function('operand_roles', i1)
	entry := m.add_block(fidx, 'entry')
	other := m.add_block(fidx, 'other')
	join := m.add_block(fidx, 'join')

	dummy := m.add_value(.constant, i1, '0', 0)
	old_value := m.add_value(.argument, i1, 'old', 0)
	new_value := m.add_value(.argument, i1, 'new', 1)
	assert int(old_value) == int(join)

	br_value := m.add_instr(.br, entry, 0, [old_value, ssa.ValueID(join), ssa.ValueID(other)])
	switch_value := m.add_instr(.switch_, other, 0, [old_value, ssa.ValueID(join), old_value,
		ssa.ValueID(entry)])
	phi_value := m.add_instr(.phi, join, i1,
		[old_value, ssa.ValueID(join), dummy, ssa.ValueID(other)])

	m.replace_uses(old_value, new_value)

	br := m.instrs[m.values[br_value].index]
	assert br.operands == [new_value, ssa.ValueID(join), ssa.ValueID(other)]
	switch_instr := m.instrs[m.values[switch_value].index]
	assert switch_instr.operands == [new_value, ssa.ValueID(join), new_value, ssa.ValueID(entry)]
	phi := m.instrs[m.values[phi_value].index]
	assert phi.operands == [new_value, ssa.ValueID(join), dummy, ssa.ValueID(other)]
	assert m.values[old_value].uses.len == 0
	assert br_value in m.values[new_value].uses
	assert switch_value in m.values[new_value].uses
	assert phi_value in m.values[new_value].uses
}

// test_replace_uses_preserves_assign_destination validates that lowered copy
// destinations are definitions rather than replaceable value uses.
fn test_replace_uses_preserves_assign_destination() {
	mut m := ssa.Module.new()
	i64t := m.type_store.get_int(64)
	func_id := m.new_function('assign_definition', i64t)
	entry := m.add_block(func_id, 'entry')
	dest := m.add_value(.phi_result, i64t, 'dest', -1)
	src := m.get_or_add_const(i64t, '1')
	replacement := m.get_or_add_const(i64t, '2')
	assign := m.add_instr(.assign, entry, i64t, [dest, src])
	m.add_instr(.ret, entry, ssa.TypeID(0), [dest])

	m.replace_uses(dest, replacement)

	instr := m.instrs[m.values[assign].index]
	assert instr.operands[0] == dest
	ret_id := m.blocks[entry].instrs.last()
	ret := m.instrs[m.values[ret_id].index]
	assert ret.operands[0] == replacement
}

fn test_critical_edge_split_preserves_colliding_branch_condition() {
	mut m := ssa.Module.new()
	i1 := m.type_store.get_int(1)
	fidx := m.new_function('branch_collision', i1)
	entry := m.add_block(fidx, 'entry')
	other := m.add_block(fidx, 'other')
	join := m.add_block(fidx, 'join')

	dummy := m.add_value(.constant, i1, '0', 0)
	cond := m.add_value(.argument, i1, 'cond', 0)
	m.func_add_param(fidx, cond)
	assert int(cond) == int(join)

	branch := m.add_instr(.br, entry, 0, [cond, ssa.ValueID(join), ssa.ValueID(other)])
	m.add_instr(.jmp, other, 0, [ssa.ValueID(join)])
	phi := m.add_instr(.phi, join, i1, [cond, ssa.ValueID(entry), dummy, ssa.ValueID(other)])
	m.add_instr(.ret, join, 0, [phi])

	optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
		eliminate_phis: true
	})

	branch_instr := m.instrs[m.values[branch].index]
	assert branch_instr.op == .br
	assert branch_instr.operands[0] == cond
}

fn test_critical_edge_split_preserves_colliding_switch_values() {
	mut m := ssa.Module.new()
	i1 := m.type_store.get_int(1)
	fidx := m.new_function('switch_collision', i1)
	entry := m.add_block(fidx, 'entry')
	other := m.add_block(fidx, 'other')
	join := m.add_block(fidx, 'join')

	dummy := m.add_value(.constant, i1, '0', 0)
	cond := m.add_value(.argument, i1, 'cond', 0)
	m.func_add_param(fidx, cond)
	assert int(cond) == int(join)

	switch_value := m.add_instr(.switch_, entry, 0,
		[cond, ssa.ValueID(join), cond, ssa.ValueID(other)])
	m.add_instr(.jmp, other, 0, [ssa.ValueID(join)])
	phi := m.add_instr(.phi, join, i1, [cond, ssa.ValueID(entry), dummy, ssa.ValueID(other)])
	m.add_instr(.ret, join, 0, [phi])

	optimize.optimize_with_options(mut m, optimize.OptimizeOptions{
		eliminate_phis: true
	})

	switch_instr := m.instrs[m.values[switch_value].index]
	assert switch_instr.op == .switch_
	assert switch_instr.operands[0] == cond
	assert switch_instr.operands[2] == cond
}
