module transform

import os
import v.flat
import v.gen.c as cgen
import v.parser
import v.pref
import v.types

fn numeric_infix_fixture_c(source string, shortcut bool) (string, int) {
	path := os.join_path(os.vtmp_dir(), 'numeric_infix_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = shortcut
	t.cur_module = 'main'
	t.cur_file = path
	t.transformed_fns = []bool{len: a.nodes.len}
	t.prepare()
	mut hits := 0
	mut used := map[string]bool{}
	for node in a.nodes {
		if node.kind == .infix && t.checked_numeric_infix_can_skip_handlers(node) {
			hits++
		}
		if node.kind == .fn_decl {
			used[node.value] = true
		}
	}
	t.transform_all()
	assert t.monomorph_errors.len == 0, t.monomorph_errors.str()
	mut gen := cgen.FlatGen.new()
	return gen.gen_with_used(a, used, &tc), hits
}

fn test_checked_numeric_infix_preserves_scalar_and_alias_emitted_c() {
	source := 'type Lane = u64
fn scalar(a u64, b u64, shift u32) u64 {
	mut value := (a ^ b) | (a & b)
	value += (a + b) * (a - b)
	value ^= (a / b) % b
	value += (a >> shift) + (a >>> shift) + (a << shift)
	if a < b || a == b || a >= b {
		value += b
	}
	return value
}
fn alias(a Lane, b Lane, values []Lane) Lane {
	return Lane((a ^ b) | (values[0] & values[1]))
}
fn floating(a f64, b f64) f64 {
	return (a + b) * (a - b) / b
}
fn main() {}
'
	actual, hits := numeric_infix_fixture_c(source, true)
	expected, _ := numeric_infix_fixture_c(source, false)
	assert hits >= 10
	assert actual == expected
}

fn test_checked_numeric_infix_preserves_alias_operator_and_index_lowering() {
	source := 'type Count = int
fn (a Count) + (b Count) Count { return Count(int(a) + int(b) + 7) }
fn (a Count) == (b Count) bool { return int(a) % 10 == int(b) % 10 }
fn (a Count) < (b Count) bool { return int(a) % 10 < int(b) % 10 }
fn overloaded(a Count, b Count, values []Count) Count {
	mut value := a + b
	value += values[0] + values[1]
	if a != b || values[0] >= values[1] || a <= b {
		value = b + a
	}
	return value
}
fn main() {}
'
	actual, _ := numeric_infix_fixture_c(source, true)
	expected, _ := numeric_infix_fixture_c(source, false)
	assert actual == expected
	assert actual.contains('Count__plus(')
	assert actual.contains('Count__eq(')
	assert actual.contains('Count__lt(')
}

fn test_checked_numeric_infix_preserves_value_branches_and_wide_operations() {
	source := 'fn value(a int) int { return a }
fn branching(a int, b int) int {
	return value(a) + (if b > 0 { value(b) } else { value(-b) })
}
fn wide(a u128, b u64, c i128) u128 {
	return a + u128(b) + u128(c)
}
fn main() {}
'
	actual, _ := numeric_infix_fixture_c(source, true)
	expected, _ := numeric_infix_fixture_c(source, false)
	assert actual == expected
	assert actual.contains('__v_u128_')
}

fn test_checked_numeric_infix_requires_ordinary_checked_operands() {
	mut a := flat.FlatAst.new()
	lhs := a.add_val(.ident, 'lhs')
	rhs := a.add_val(.ident, 'rhs')
	start := a.children.len
	a.children << [lhs, rhs]
	id := a.add_node(flat.Node{ kind: .infix, op: .plus, children_start: start, children_count: 2 })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	for typ in [types.Type(types.u128_), types.Type(types.i128_),
		types.Type(types.Pointer{ base_type: types.Type(types.int_) }),
		types.Type(types.Array{ elem_type: types.Type(types.int_) }),
		types.Type(types.OptionType{ base_type: types.Type(types.int_) })] {
		tc.register_synth_type(lhs, typ)
		tc.register_synth_type(rhs, typ)
		assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	}
	tc.register_synth_type(lhs, types.Type(types.int_))
	tc.register_synth_type(rhs, types.Type(types.int_))
	assert t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.skip_generics = false
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.skip_generics = true
	t.building_v = true
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.building_v = false
	t.validating_generic_spec = true
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.validating_generic_spec = false
	t.cur_fn_is_generic = true
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.cur_fn_is_generic = false
	t.active_generic_params = ['T']
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.active_generic_params = []string{}
	t.active_specialization_args = ['int']
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	t.active_specialization_args = []string{}
	t.smartcast_stack = [SmartcastContext{}]
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
}

fn test_checked_numeric_infix_recovers_index_alias_overrides_from_raw_element_type() {
	mut a := flat.FlatAst.new()
	base := a.add_val(.ident, 'values')
	index := a.add_val(.int_literal, '0')
	index_start := a.children.len
	a.children << [base, index]
	lhs := a.add_node(flat.Node{ kind: .index, children_start: index_start, children_count: 2 })
	rhs := a.add_val(.ident, 'other')
	start := a.children.len
	a.children << [lhs, rhs]
	id := a.add_node(flat.Node{ kind: .infix, op: .plus, children_start: start, children_count: 2 })
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Count'] = 'int'
	// A flattened index sidecar alone must not hide a declared alias operator.
	tc.register_synth_type(lhs, types.Type(types.int_))
	tc.register_synth_type(rhs, types.Type(types.int_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	t.cur_module = 'main'
	t.set_var_type_with_raw('values', '[]int', '[]Count')
	t.fn_ret_types['Count.+'] = 'Count'
	t.fn_ret_types['Count.=='] = 'bool'
	t.fn_ret_types['Count.<'] = 'bool'
	for op in [flat.Op.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		a.nodes[int(id)].op = op
		assert t.operator_alias_type_for_operand(lhs, op) or { '' } == 'Count'
		assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	}
	// The existing struct path dispatches on LHS, but the conservative shortcut
	// also keeps a RHS alias on that path instead of assuming scalar arithmetic.
	a.nodes[int(id)].op = .plus
	a.children[start] = rhs
	a.children[start + 1] = lhs
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
}

fn test_checked_numeric_infix_keeps_named_annotations_and_casts_on_operator_path() {
	mut a := flat.FlatAst.new()
	lhs := a.add_val(.ident, 'lhs')
	rhs := a.add_val(.ident, 'rhs')
	start := a.children.len
	a.children << [lhs, rhs]
	id := a.add_node(flat.Node{ kind: .infix, op: .plus, children_start: start, children_count: 2 })
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['Count'] = 'int'
	tc.register_synth_type(lhs, types.Type(types.int_))
	tc.register_synth_type(rhs, types.Type(types.int_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	t.cur_module = 'main'
	t.fn_ret_types['Count.+'] = 'Count'
	assert t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	// Both textual candidates used by the original alias lookup must survive
	// a primitive sidecar; only builtin annotations are free of nominal lookup.
	a.nodes[int(lhs)].typ = 'Count'
	assert t.operator_alias_type_for_operand(lhs, .plus) or { '' } == 'Count'
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	a.nodes[int(lhs)].typ = 'int'
	assert t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	a.nodes[int(lhs)].typ = ''
	a.nodes[int(lhs)].kind = .cast_expr
	a.nodes[int(lhs)].value = 'Count'
	assert t.operator_alias_type_for_operand(lhs, .plus) or { '' } == 'Count'
	assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	a.nodes[int(lhs)].value = 'int'
	assert t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
}

fn test_checked_numeric_infix_infers_only_existing_handler_operands() {
	mut a := flat.FlatAst.new()
	literal := a.add_val(.int_literal, '7')
	cast_start := a.children.len
	a.children << literal
	lhs := a.add_node(flat.Node{ kind: .cast_expr, value: 'u64', children_start: cast_start, children_count: 1 })
	rhs := a.add_val(.ident, 'unresolved_rhs')
	start := a.children.len
	a.children << [lhs, rhs]
	id := a.add_node(flat.Node{ kind: .infix, op: .minus, children_start: start, children_count: 2 })
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.skip_generics = true
	// These handlers inspect only LHS, so the shortcut must not resolve RHS.
	for op in [flat.Op.minus, .mul, .div, .mod, .amp, .pipe, .xor, .right_shift, .right_shift_unsigned] {
		a.nodes[int(id)].op = op
		assert t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
		assert tc.expr_type(rhs) == none
	}
	// Addition and comparison inspect both operands and retain the uncertain path.
	for op in [flat.Op.plus, .eq, .ne, .lt, .gt, .le, .ge] {
		a.nodes[int(id)].op = op
		assert !t.checked_numeric_infix_can_skip_handlers(a.nodes[int(id)])
	}
}

fn test_checked_numeric_infix_preserves_mixed_width_and_string_paths() {
	source := 'type Count = int
fn (a Count) - (b Count) Count { return Count(int(a) - int(b) + 13) }
fn mix(a u64, b u128, c i128, shift u32) u128 {
	return (u128(a) - b) ^ (u128(c) >> shift)
}
fn scalar_alias(a int, b Count) int {
	return a - int(b)
}
fn overloaded(a Count, b Count, values []Count) Count {
	return a - b - values[0]
}
fn strings(a string, b string) bool {
	return a + b == b + a || a < b
}
fn main() {}
'
	actual, _ := numeric_infix_fixture_c(source, true)
	expected, _ := numeric_infix_fixture_c(source, false)
	assert actual == expected
	assert actual.contains('Count__minus(')
	assert actual.contains('string__plus(')
	assert actual.contains('string__lt(')
	assert actual.contains('__v_u128_')
}
