module optimize

import v.ssa

fn assert_comparison_preserved(mut m ssa.Module, op ssa.OpCode, lhs ssa.ValueID, rhs ssa.ValueID) {
	bool_type := m.type_store.get_int(1)
	func_id := m.new_function('compare', bool_type)
	entry := m.add_block(func_id, 'entry')
	comparison := m.add_instr(op, entry, bool_type, [lhs, rhs])
	ret := m.add_instr(.ret, entry, ssa.TypeID(0), [comparison])

	optimize_with_options(mut m, OptimizeOptions{
		mem2reg:        true
		eliminate_phis: true
		strict_verify:  true
	})

	assert m.blocks[entry].instrs == [comparison, ret]
	assert m.instrs[m.values[comparison].index].op == op
	assert m.instrs[m.values[comparison].index].operands == [lhs, rhs]
	assert m.instrs[m.values[ret].index].operands == [comparison]
}

fn test_constant_fold_preserves_float_comparisons() {
	for width in [32, 64] {
		for op in [ssa.OpCode.eq, .ne, .lt, .gt, .le, .ge] {
			mut m := ssa.Module.new()
			float_type := m.type_store.get_float(width)
			lhs := m.get_or_add_const(float_type, '1.25')
			rhs := m.get_or_add_const(float_type, '1.5')
			assert_comparison_preserved(mut m, op, lhs, rhs)
		}
	}
}

fn test_constant_fold_preserves_float_precision() {
	// These differ as f64 values but round to the same f32 value.
	for width in [32, 64] {
		mut m := ssa.Module.new()
		float_type := m.type_store.get_float(width)
		lhs := m.get_or_add_const(float_type, '16777216')
		rhs := m.get_or_add_const(float_type, '16777217')
		assert_comparison_preserved(mut m, .eq, lhs, rhs)
	}
}

fn test_constant_fold_preserves_mixed_float_integer_comparisons() {
	for float_on_left in [false, true] {
		mut m := ssa.Module.new()
		float_type := m.type_store.get_float(64)
		int_type := m.type_store.get_int(64)
		float_value := m.get_or_add_const(float_type, '1.5')
		int_value := m.get_or_add_const(int_type, '1')
		lhs := if float_on_left { float_value } else { int_value }
		rhs := if float_on_left { int_value } else { float_value }
		assert_comparison_preserved(mut m, .eq, lhs, rhs)
	}
}

fn test_constant_fold_still_folds_integer_comparisons() {
	ops := [ssa.OpCode.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge]
	expected := ['0', '1', '1', '0', '1', '0', '0', '1', '0', '1']
	for i, op in ops {
		mut m := ssa.Module.new()
		bool_type := m.type_store.get_int(1)
		int_type := m.type_store.get_int(64)
		func_id := m.new_function('compare', bool_type)
		entry := m.add_block(func_id, 'entry')
		lhs := m.get_or_add_const(int_type, '-1')
		rhs := m.get_or_add_const(int_type, '0')
		comparison := m.add_instr(op, entry, bool_type, [lhs, rhs])
		ret := m.add_instr(.ret, entry, ssa.TypeID(0), [comparison])

		optimize(mut m)

		assert m.blocks[entry].instrs == [ret]
		ret_operand := m.values[m.instrs[m.values[ret].index].operands[0]]
		assert ret_operand.kind == .constant
		assert ret_operand.typ == bool_type
		assert ret_operand.name == expected[i]
	}
}
