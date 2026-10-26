module c

import v.flat
import v.token
import v.types

fn expected_scalar_alias(typ types.Type) types.Type {
	return types.Alias{
		name_id:   flat.source_type_id('ScalarAlias')
		base_type: typ
	}
}

fn expected_scalar_output(mut g FlatGen, id flat.NodeId, expected types.Type, actual types.Type, known bool) string {
	old_expected := types.Type(types.string_)
	old_enum := flat.source_type_id('PreviousEnum')
	g.expected_expr_type = old_expected
	g.expected_enum = old_enum
	g.known_expr_type = actual
	g.known_expr_type_id = if known { int(id) } else { -1 }
	output := g.expr_to_string_with_expected_type(id, expected)
	assert g.expected_expr_type == old_expected
	assert g.expected_enum == old_enum
	assert g.known_expr_type_id == -1
	return output
}

fn expected_scalar_node(mut a flat.FlatAst, kind flat.NodeKind, op flat.Op, children []flat.NodeId) flat.NodeId {
	start := a.children.len
	a.children << children
	return a.add_node(flat.Node{
		kind:           kind
		op:             op
		children_start: start
		children_count: children.len
	})
}

fn test_expected_scalar_literals_preserve_fallback_output_and_context() {
	for kind, values in {
		flat.NodeKind.int_literal:   ['0', '1_000', '0o7_7', '18446744073709551615']
		flat.NodeKind.float_literal: ['0.0', '1_000.5', '1.0e+1_0']
		flat.NodeKind.bool_literal:  ['true', 'false']
	} {
		for value in values {
			actual := if kind == .float_literal {
				types.Type(types.f64_)
			} else if kind == .bool_literal {
				types.Type(types.bool_)
			} else {
				types.Type(types.u64_)
			}
			expected_types := if kind == .bool_literal {
				[types.Type(types.bool_)]
			} else if kind == .float_literal {
				[types.Type(types.f64_)]
			} else {
				[types.Type(types.int_), types.Type(types.u64_), types.Type(types.f32_),
					types.Type(types.f64_)]
			}
			mut a := flat.FlatAst.new()
			id := a.add_val(kind, value)
			mut tc := types.TypeChecker.new(&a)
			tc.register_synth_type(id, actual)
			mut g := FlatGen.new()
			g.a = &a
			g.tc = &tc
			for expected in expected_types {
				for known in [false, true] {
					fallback := expected_scalar_output(mut g, id, expected_scalar_alias(expected), actual, known)
					assert expected_scalar_output(mut g, id, expected, actual, known) == fallback, value
				}
			}
			for replacement in ['assert_literal', 'callback_literal'] {
				g.assert_expr_overrides.clear()
				g.callback_target_overrides.clear()
				if replacement == 'assert_literal' {
					g.assert_expr_overrides[int(id)] = replacement
				} else {
					g.callback_target_overrides[int(id)] = replacement
				}
				assert expected_scalar_output(mut g, id, actual, actual, true) == replacement
			}
		}
	}
}

fn test_expected_scalar_expressions_preserve_fallback_and_unrelated_known_type() {
	mut a := flat.FlatAst.new()
	left := a.add_val(.int_literal, '3')
	right := a.add_val(.int_literal, '4')
	value := a.add_val(.ident, 'value')
	sum := expected_scalar_node(mut a, .infix, .plus, [left, right])
	paren := expected_scalar_node(mut a, .paren, .none, [sum])
	negative := expected_scalar_node(mut a, .prefix, .minus, [paren])
	mut tc := types.TypeChecker.new(&a)
	int_type := types.Type(types.int_)
	tc.cur_scope.insert('value', int_type)
	for id in [left, right, value, sum, paren, negative] {
		tc.register_synth_type(id, int_type)
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for id in [value, sum, paren, negative] {
		for known in [false, true] {
			fallback := expected_scalar_output(mut g, id, expected_scalar_alias(int_type), int_type, known)
			assert expected_scalar_output(mut g, id, int_type, int_type, known) == fallback
		}
	}
	g.known_expr_type_id = int(right)
	g.known_expr_type = types.Type(types.u64_)
	assert g.expr_to_string_with_expected_type(left, int_type) == '3'
	assert g.known_expr_type_id == int(right)
	assert g.known_expr_type == types.Type(types.u64_)
}

fn test_expected_scalar_shortcuts_keep_f32_rounding_and_int128_widening() {
	mut a := flat.FlatAst.new()
	fraction := a.add_val(.float_literal, '1.0000000596046448')
	negative := expected_scalar_node(mut a, .prefix, .minus, [fraction])
	integer := a.add_val(.int_literal, '7')
	mut tc := types.TypeChecker.new(&a)
	tc.register_synth_type(fraction, types.Type(types.f64_))
	tc.register_synth_type(negative, types.Type(types.f64_))
	tc.register_synth_type(integer, types.Type(types.int_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert expected_scalar_output(mut g, fraction, types.Type(types.f32_), types.Type(types.f64_), true) == '(float)(1.0000000596046448)'
	assert expected_scalar_output(mut g, negative, types.Type(types.f32_), types.Type(types.f64_), true) == '-(float)(1.0000000596046448)'
	for portable in [false, true] {
		g.compile_defines = if portable { ['v3_no_native_int128'] } else { []string{} }
		assert expected_scalar_output(mut g, integer, types.Type(types.i128_), types.Type(types.int_), true) == '__v_i128_from_i64((i64)(7))'
		assert expected_scalar_output(mut g, integer, types.Type(types.u128_), types.Type(types.int_), true) == '__v_u128_from_i64((i64)(7))'
	}
}

fn test_expected_scalar_call_keeps_declared_pointer_return_and_mut_parameter_read() {
	mut a := flat.FlatAst.new()
	callee := a.add_val(.ident, 'read_pointer')
	call := expected_scalar_node(mut a, .call, .none, [callee])
	param := a.add_val(.ident, 'value')
	mut tc := types.TypeChecker.new(&a)
	int_type := types.Type(types.int_)
	pointer_type := types.Type(types.Pointer{ base_type: int_type })
	tc.fn_ret_types['read_pointer'] = pointer_type
	tc.register_synth_type(call, int_type)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.assert_expr_overrides[int(call)] = 'read_pointer()'
	assert expected_scalar_output(mut g, call, int_type, int_type, true) == '*read_pointer()'
	g.cur_param_types['value'] = types.new_type_reference(pointer_type)
	g.cur_mut_params['value'] = true
	g.cur_mut_param_owners['value'] = tc.cur_scope.insert_with_owner('value', pointer_type)
	assert expected_scalar_output(mut g, param, int_type, int_type, true) == '*value'
}

fn test_expected_scalar_translated_literals_keep_boolean_and_int_normalization() {
	mut a := flat.FlatAst.new()
	mut files := token.FileSet.new()
	a.source_files[1] = files.add_file('translated_scalar.v', 1)
	wide := a.add_node(flat.Node{ kind: .int_literal, value: '256', pos: token.new_pos(1, 0) })
	fraction := a.add_node(flat.Node{ kind: .float_literal, value: '0.5', pos: token.new_pos(1, 0) })
	mut tc := types.TypeChecker.new(&a)
	tc.translated_files['translated_scalar.v'] = true
	tc.register_synth_type(wide, types.Type(types.int_))
	tc.register_synth_type(fraction, types.Type(types.f64_))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert expected_scalar_output(mut g, wide, types.Type(types.bool_), types.Type(types.int_), true) == '((256) != 0)'
	assert expected_scalar_output(mut g, fraction, types.Type(types.bool_), types.Type(types.f64_), true) == '((0.5) != 0)'
	assert expected_scalar_output(mut g, wide, types.Type(types.int_), types.Type(types.int_), true) == '((i32)(256))'
}
