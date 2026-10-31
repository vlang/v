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

fn assert_integer_fold(op ssa.OpCode, lhs_name string, rhs_name string, expected string, unsigned bool) {
	mut m := ssa.Module.new()
	int_type := if unsigned { m.type_store.get_uint(64) } else { m.type_store.get_int(64) }
	result_type := if op in [.eq, .ne, .lt, .gt, .le, .ge, .ult, .ugt, .ule, .uge] {
		m.type_store.get_int(1)
	} else {
		int_type
	}
	func_id := m.new_function('fold', result_type)
	entry := m.add_block(func_id, 'entry')
	lhs := m.get_or_add_const(int_type, lhs_name)
	rhs := m.get_or_add_const(int_type, rhs_name)
	operation := m.add_instr(op, entry, result_type, [lhs, rhs])
	ret := m.add_instr(.ret, entry, ssa.TypeID(0), [operation])

	optimize_with_options(mut m, OptimizeOptions{
		strict_verify: true
	})

	assert m.blocks[entry].instrs == [ret]
	result := m.values[m.instrs[m.values[ret].index].operands[0]]
	assert result.kind == .constant
	assert result.typ == result_type
	assert result.name == expected, '${op}: ${lhs_name}, ${rhs_name}'
}

fn test_constant_fold_preserves_full_width_integer_bits() {
	for unsigned in [false, true] {
		assert_integer_fold(.eq, '9223372036854775808', '9223372036854775809', '0', unsigned)
		assert_integer_fold(.ne, '9223372036854775808', '9223372036854775809', '1', unsigned)
		assert_integer_fold(.eq, '18446744073709551615', '-1', '1', unsigned)
		assert_integer_fold(.add, '18446744073709551615', '3', '2', unsigned)
		assert_integer_fold(.sub, '18446744073709551615', '2', '-3', unsigned)
		assert_integer_fold(.mul, '18446744073709551615', '3', '-3', unsigned)
		assert_integer_fold(.and_, '18446744073709551615', '0xff', '255', unsigned)
	}
	assert_integer_fold(.lt, '9223372036854775807', '9223372036854775808', '0', false)
	assert_integer_fold(.lt, '9223372036854775808', '9223372036854775809', '1', false)
	assert_integer_fold(.ult, '9223372036854775807', '9223372036854775808', '1', true)
	assert_integer_fold(.ugt, '18446744073709551615', '9223372036854775808', '1', true)
	assert_integer_fold(.udiv, '18446744073709551615', '3', '6148914691236517205', true)
	assert_integer_fold(.urem, '18446744073709551615', '2', '1', true)
	assert_integer_fold(.sdiv, '-9223372036854775808', '2', '-4611686018427387904', false)
}

fn test_constant_fold_parses_integer_radices_and_separators() {
	for name in ['0xffffffffffffffff', '0xFFFF_FFFF_FFFF_FFFF', '0o1777777777777777777777',
		'0b' + '1'.repeat(64), '18_446_744_073_709_551_615'] {
		assert_integer_fold(.eq, name, '18446744073709551615', '1', true)
		assert_integer_fold(.add, name, '3', '2', true)
	}
	assert_integer_fold(.ne, '0x8000000000000000', '0x8000000000000001', '1', false)
	assert_integer_fold(.eq, '+9223372036854775808', '-9223372036854775808', '1', false)
	assert_integer_fold(.add, '010', '5', '15', false)
}

fn test_integer_constant_values_use_operand_width_and_boolean_representation() {
	mut m := ssa.Module.new()
	i8_type := m.type_store.get_int(8)
	u8_type := m.type_store.get_uint(8)
	bool_type := m.type_store.get_int(1)
	for name in ['128', '0x80', '-128'] {
		value := m.get_or_add_const(i8_type, name)
		assert integer_constant_value(m, m.values[value])? == -128
	}
	value := m.get_or_add_const(u8_type, '256')
	assert integer_constant_value(m, m.values[value])? == 0
	true_value := m.get_or_add_const(bool_type, '1')
	assert integer_constant_value(m, m.values[true_value])? == 1
	false_value := m.get_or_add_const(bool_type, '0')
	assert integer_constant_value(m, m.values[false_value])? == 0
}

fn test_constant_fold_observes_narrow_result_width() {
	for unsigned in [false, true] {
		mut m := ssa.Module.new()
		int_type := if unsigned { m.type_store.get_uint(8) } else { m.type_store.get_int(8) }
		bool_type := m.type_store.get_int(1)
		func_id := m.new_function('overflow', bool_type)
		entry := m.add_block(func_id, 'entry')
		lhs := m.get_or_add_const(int_type, if unsigned { '255' } else { '127' })
		one := m.get_or_add_const(int_type, '1')
		zero := m.get_or_add_const(int_type, '0')
		sum := m.add_instr(.add, entry, int_type, [lhs, one])
		comparison := m.add_instr(if unsigned { ssa.OpCode.eq } else { ssa.OpCode.lt },
			entry, bool_type, [sum, zero])
		ret := m.add_instr(.ret, entry, ssa.TypeID(0), [comparison])

		optimize_with_options(mut m, OptimizeOptions{
			strict_verify: true
		})

		assert m.blocks[entry].instrs == [ret]
		result := m.values[m.instrs[m.values[ret].index].operands[0]]
		assert result.kind == .constant
		assert result.name == '1'
	}
}

fn test_constant_fold_logical_right_shift_uses_operand_width() {
	expected := ['127', '32767', '2147483647', '9223372036854775807']
	for i, width in [8, 16, 32, 64] {
		mut m := ssa.Module.new()
		int_type := m.type_store.get_int(width)
		func_id := m.new_function('logical_shift', int_type)
		entry := m.add_block(func_id, 'entry')
		lhs := m.get_or_add_const(int_type, '-1')
		one := m.get_or_add_const(int_type, '1')
		shift := m.add_instr(.lshr, entry, int_type, [lhs, one])
		ret := m.add_instr(.ret, entry, ssa.TypeID(0), [shift])

		optimize_with_options(mut m, OptimizeOptions{
			strict_verify: true
		})

		assert m.blocks[entry].instrs == [ret]
		result := m.values[m.instrs[m.values[ret].index].operands[0]]
		assert result.kind == .constant
		assert result.name == expected[i]
	}
}

fn test_constant_fold_logical_shift_after_narrow_overflow() {
	maximums := ['127', '32767', '2147483647']
	expected := ['64', '16384', '1073741824']
	for i, width in [8, 16, 32] {
		mut m := ssa.Module.new()
		int_type := m.type_store.get_int(width)
		func_id := m.new_function('logical_shift', int_type)
		entry := m.add_block(func_id, 'entry')
		lhs := m.get_or_add_const(int_type, maximums[i])
		one := m.get_or_add_const(int_type, '1')
		sum := m.add_instr(.add, entry, int_type, [lhs, one])
		shift := m.add_instr(.lshr, entry, int_type, [sum, one])
		ret := m.add_instr(.ret, entry, ssa.TypeID(0), [shift])

		optimize_with_options(mut m, OptimizeOptions{
			strict_verify: true
		})

		assert m.blocks[entry].instrs == [ret]
		result := m.values[m.instrs[m.values[ret].index].operands[0]]
		assert result.kind == .constant
		assert result.name == expected[i]
	}
}

fn test_constant_fold_preserves_oversized_shifts() {
	for width in [8, 16, 32, 64] {
		for count in [width, width + 1] {
			for op in [ssa.OpCode.shl, .ashr, .lshr] {
				mut m := ssa.Module.new()
				int_type := m.type_store.get_int(width)
				count_type := m.type_store.get_int(64)
				func_id := m.new_function('oversized_shift', int_type)
				entry := m.add_block(func_id, 'entry')
				lhs := m.get_or_add_const(int_type, '-1')
				rhs := m.get_or_add_const(count_type, count.str())
				shift := m.add_instr(op, entry, int_type, [lhs, rhs])
				ret := m.add_instr(.ret, entry, ssa.TypeID(0), [shift])

				optimize_with_options(mut m, OptimizeOptions{
					strict_verify: true
				})

				assert m.blocks[entry].instrs == [shift, ret]
				assert m.instrs[m.values[ret].index].operands == [shift]
			}
		}
	}
}

fn test_constant_fold_declines_invalid_and_wider_constants() {
	for name in ['18446744073709551616', '0x10000000000000000', 'not_an_integer', 'undef'] {
		mut m := ssa.Module.new()
		int_type := m.type_store.get_int(64)
		lhs := m.get_or_add_const(int_type, name)
		rhs := m.get_or_add_const(int_type, '1')
		assert_comparison_preserved(mut m, .eq, lhs, rhs)
	}
}

fn test_constant_fold_declines_signed_division_overflow() {
	minimums := ['-128', '-32768', '-2147483648', '-9223372036854775808']
	for i, width in [8, 16, 32, 64] {
		for op in [ssa.OpCode.sdiv, .srem] {
			mut m := ssa.Module.new()
			int_type := m.type_store.get_int(width)
			func_id := m.new_function('overflow', int_type)
			entry := m.add_block(func_id, 'entry')
			lhs := m.get_or_add_const(int_type, minimums[i])
			rhs := m.get_or_add_const(int_type, '-1')
			operation := m.add_instr(op, entry, int_type, [lhs, rhs])
			ret := m.add_instr(.ret, entry, ssa.TypeID(0), [operation])

			optimize_with_options(mut m, OptimizeOptions{
				strict_verify: true
			})

			assert m.blocks[entry].instrs == [operation, ret]
			assert m.instrs[m.values[ret].index].operands == [operation]
		}
	}
}

fn test_algebraic_simplification_parses_full_width_constants() {
	for name in ['18446744073709551615', '0xffffffffffffffff'] {
		mut m := ssa.Module.new()
		int_type := m.type_store.get_uint(64)
		value := m.get_or_add_const(int_type, name)
		other := m.add_value(.argument, int_type, 'value', 0)
		instr := ssa.Instruction{
			op:       .mul
			operands: [other, value]
		}
		replacement, needs_zero := try_algebraic_simplify(m, 0, instr, m.values[other],
			m.values[value])
		assert replacement == -1
		assert !needs_zero
	}
	mut m := ssa.Module.new()
	int_type := m.type_store.get_int(64)
	one := m.get_or_add_const(int_type, '0x1')
	other := m.add_value(.argument, int_type, 'value', 0)
	instr := ssa.Instruction{
		op:       .mul
		operands: [other, one]
	}
	replacement, needs_zero := try_algebraic_simplify(m, 0, instr, m.values[other], m.values[one])
	assert replacement == other
	assert !needs_zero
}
