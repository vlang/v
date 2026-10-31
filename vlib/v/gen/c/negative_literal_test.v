module c

import v.flat
import v.types

fn test_negated_wide_literals_remain_signed_through_parentheses() {
	for spelling in ['2147483648', '3_000_000_000', '0x80000000', '0o20000000000'] {
		for depth in 0 .. 3 {
			mut a := flat.FlatAst.new()
			mut child := a.add_val(.int_literal, spelling)
			for _ in 0 .. depth {
				start := a.begin_children()
				a.add_child(child)
				child = a.add_node(flat.Node{ kind: .paren, children_start: start, children_count: 1 })
			}
			start := a.begin_children()
			a.add_child(child)
			prefix := a.add_node(flat.Node{ kind: .prefix, op: .minus, children_start: start, children_count: 1 })
			mut tc := types.TypeChecker.new(&a)
			mut g := FlatGen.new()
			g.a = &a
			g.tc = &tc
			g.gen_prefix_op_operand(.minus, child)
			expected := '-' + numeric_literal_c_text(&a.nodes[0]) + 'LL'
			assert g.sb.str() == expected
			assert g.const_expr_to_string(prefix, []) == expected
		}
	}
}

fn test_wide_literal_suffix_respects_signed_range() {
	for spelling in ['0', '2147483647', '9223372036854775808', '18446744073709551616'] {
		node := flat.Node{ kind: .int_literal, value: spelling }
		assert !int_literal_needs_long_long_suffix(&node)
	}
	for spelling in ['2147483648', '3_000_000_000', '0x80000000', '0o20000000000', '9223372036854775807'] {
		node := flat.Node{ kind: .int_literal, value: spelling }
		assert int_literal_needs_long_long_suffix(&node)
	}
}

fn test_decimal_literals_above_the_signed_range_are_spelled_unsigned() {
	for spelling in ['9223372036854775809', '10000000000000000000', '18446744073709551615',
		'18_446_744_073_709_551_615'] {
		node := flat.Node{ kind: .int_literal, value: spelling }
		assert numeric_literal_c_text(&node) == spelling.replace('_', '') + 'ULL'
	}
	// 2^63 is min_i64's negated operand; hex and octal are unsigned in C by the standard
	for spelling in ['9223372036854775808', '9223372036854775807', '1000000000', '0xFFFFFFFFFFFFFFFF',
		'0o1777777777777777777777'] {
		node := flat.Node{ kind: .int_literal, value: spelling }
		assert !numeric_literal_c_text(&node).ends_with('ULL'), spelling
	}
}
