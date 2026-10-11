module c

import strings
import os
import v.flat
import v.parser
import v.pref
import v.types

fn test_fixed_array_numeric_literals_match_const_expression_emission() {
	for kind, texts in {
		flat.NodeKind.int_literal:   ['0', '127', '255', '32767', '65535', '2147483647', '4294967295',
			'9223372036854775807', '18446744073709551615', '18_446_744_073_709_551_615', '0xff_ff',
			'0o1777777777777777777777', '0b${'1'.repeat(64)}', '18446744073709551616',
			'0x1_0000_0000_0000_0000', '0o2000000000000000000000', '0b1${'0'.repeat(64)}',
			'340282366920938463463374607431768211455']
		flat.NodeKind.float_literal: ['0.0', '1_000.5', '1.0e+1_0', '3.402823466e+38',
			'1.7976931348623157e+308']
	} {
		for text in texts {
			for typ in [types.Type(types.u64_), types.Type(types.u128_), types.Type(types.i128_),
				types.Type(types.f32_), types.Type(types.f64_), types.Type(types.Alias{
					name:      'main.Number'
					base_type: types.Type(types.u128_)
				})] {
				mut a := flat.FlatAst.new()
				id := a.add_val(kind, text)
				mut tc := types.TypeChecker.new(&a)
				tc.register_synth_type(id, typ)
				mut g := FlatGen.new()
				g.a = &a
				g.tc = &tc
				for indent in [0, 1, 3] {
					g.indent = indent
					expected := g.expr_to_string(id)
					mut builder := strings.new_builder(64)
					g.write_fixed_array_elem_initializer(mut builder, id, typ)
					assert builder.str() == expected, text
					assert g.const_expr_to_string(id, []string{}) == expected, text
				}
			}
		}
	}
}

fn test_fixed_array_numeric_literals_preserve_expression_overrides() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.int_literal, '123')
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for callback in [false, true] {
		g.assert_expr_overrides.clear()
		g.callback_target_overrides.clear()
		if callback {
			g.callback_target_overrides[int(id)] = 'callback_literal'
		} else {
			g.assert_expr_overrides[int(id)] = 'assert_literal'
		}
		mut builder := strings.new_builder(64)
		g.write_fixed_array_elem_initializer(mut builder, id, types.Type(types.int_))
		assert builder.str() == if callback { 'callback_literal' } else { 'assert_literal' }
		assert g.const_expr_to_string(id, []string{}) == if callback {
			'callback_literal'
		} else {
			'assert_literal'
		}
	}
}

fn test_nested_fixed_array_literal_keeps_wide_alias_elements() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.int_literal, '0o7_7')
	second := a.add_val(.int_literal, '1_000')
	third := a.add_val(.int_literal, '18446744073709551616')
	start := a.children.len
	a.children << [first, second]
	inner := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: start
		children_count: 2
	})
	wide_start := a.children.len
	a.children << [third, first]
	wide_inner := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: wide_start
		children_count: 2
	})
	outer_start := a.children.len
	a.children << [inner, wide_inner]
	outer := a.add_node(flat.Node{
		kind:           .array_literal
		children_start: outer_start
		children_count: 2
	})
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	inner_type := types.ArrayFixed{
		elem_type: types.Type(types.Alias{ name: 'main.Number', base_type: types.Type(types.u128_) })
		len:       2
	}
	outer_type := types.ArrayFixed{ elem_type: types.Type(inner_type), len: 2 }
	assert g.fixed_array_initializer_string(outer, outer_type) == '{{077, 1000}, {__v_u128_make(1ULL, 0ULL), 077}}'
}

fn test_parsed_fixed_array_integer_casts_match_const_expression_emission() {
	path := os.join_path(os.vtmp_dir(), 'fixed_array_integer_casts_${os.getpid()}.v')
	os.write_file(path, 'type Wide = u128
const values = [u64(0xffff_ffff_ffff_ffff), u64(0o1777777777777777777777), u64(1_000)]!
const negatives = [i64(-1), i64(-9223372036854775807)]!
const wide = [u128(18446744073709551616), u128(340282366920938463463374607431768211455)]!
const aliases = [Wide(u128(18446744073709551616))]!
const platform = [int(123), isize(123), usize(123)]!
fn main() {}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut g := FlatGen.new()
	g.a = a
	g.tc = &tc
	mut fast_casts := 0
	mut fallback_casts := 0
	for i, node in a.nodes {
		if node.kind != .cast_expr {
			continue
		}
		id := flat.NodeId(i)
		typ := tc.resolve_type(id)
		mut direct := strings.new_builder(64)
		if g.write_fixed_array_integer_literal_cast(mut direct, &node) {
			fast_casts++
			assert direct.str() == g.const_expr_to_string(id, []string{})
		} else {
			fallback_casts++
		}
		for indent in [0, 1, 3] {
			g.indent = indent
			expected := g.const_expr_to_string(id, []string{})
			mut builder := strings.new_builder(64)
			g.write_fixed_array_elem_initializer(mut builder, id, typ)
			assert builder.str() == expected
		}
		g.indent = 0
	}
	assert fast_casts == 3
	assert fallback_casts >= 8
}

fn test_fixed_array_integer_cast_preserves_child_overrides_and_generic_context() {
	mut a := flat.FlatAst.new()
	literal := a.add_val(.int_literal, '123')
	start := a.children.len
	a.children << literal
	cast := a.add_node(flat.Node{
		kind:           .cast_expr
		value:          'u64'
		children_start: start
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for callback in [false, true] {
		g.assert_expr_overrides.clear()
		g.callback_target_overrides.clear()
		if callback {
			g.callback_target_overrides[int(literal)] = 'callback_literal'
		} else {
			g.assert_expr_overrides[int(literal)] = 'assert_literal'
		}
		expected := g.const_expr_to_string(cast, []string{})
		assert expected.contains(if callback { 'callback_literal' } else { 'assert_literal' })
		mut builder := strings.new_builder(64)
		g.write_fixed_array_elem_initializer(mut builder, cast, types.Type(types.u64_))
		assert builder.str() == expected
	}
	g.struct_default_generic_params = ['T']
	mut builder := strings.new_builder(64)
	assert !g.write_fixed_array_integer_literal_cast(mut builder, &a.nodes[int(cast)])
}
