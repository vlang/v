import v3.ssa
import v3.ssa.optimize

// test_optimize_verifies_and_preserves_valid_ssa validates this v3 regression case.
fn test_optimize_verifies_and_preserves_valid_ssa() {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	func_id := m.new_function('main', i64_type)
	entry := m.add_block(func_id, 'entry')
	two := m.get_or_add_const(i64_type, '2')
	three := m.get_or_add_const(i64_type, '3')
	add := m.add_instr(.add, entry, i64_type, [two, three])
	m.add_instr(.ret, entry, ssa.TypeID(0), [add])

	optimize.optimize(mut m)

	assert m.funcs[func_id].blocks == [entry]
	assert m.blocks[entry].instrs.len == 1
	ret_id := m.blocks[entry].instrs[0]
	ret := m.instrs[m.values[ret_id].index]
	assert ret.op == .ret
	assert ret.operands.len == 1
	ret_operand := m.values[ret.operands[0]]
	assert ret_operand.kind == .constant
	assert ret_operand.name == '5'
}
