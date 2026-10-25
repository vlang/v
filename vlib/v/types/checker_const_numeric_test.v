module types

import os
import v.flat
import v.parser
import v.pref

fn numeric_const_fixture(name string, source string) (&flat.FlatAst, &TypeChecker) {
	path := os.join_path(os.vtmp_dir(), 'numeric_const_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	return a, tc
}

fn numeric_const_type_name(tc &TypeChecker, name string) string {
	typ := tc.const_types[name] or { panic('missing ${name}') }
	return typ.name()
}

fn test_numeric_const_lookup_keeps_lexical_values_and_hides_main_suffixes() {
	_, mut tc := numeric_const_fixture('module_visibility', 'module main
const base = 100
const hidden = 42
const helper_value = 7
const helper_delta = base
fn main() {}
')
	tc.const_exprs['helper.base'] = tc.const_exprs['helper_value'] or { panic('missing value') }
	tc.const_exprs['helper.delta'] = tc.const_exprs['helper_delta'] or { panic('missing delta') }
	tc.const_modules['helper.base'] = 'helper'
	tc.const_modules['helper.delta'] = 'helper'
	// A suffix index must not expose a bare main constant to an imported helper.
	tc.const_suffixes['hidden'] = 'hidden'
	view := tc.fork_type_parse_view(tc.cur_file, 'helper')
	assert (view.const_int_value_in_module('delta', 'helper', []string{}) or { panic('missing value') }) == 7
	if value := view.const_int_value_in_module('hidden', 'helper', []string{}) {
		assert false, 'invisible main constant folded as ${value}'
	}
}

fn test_independent_numeric_const_initializer_uses_actual_literal_shapes() {
	a, tc := numeric_const_fixture('shapes', 'module main
type Count = u64
const nested = [[u64(1), 2]!, [u64(3), 4]!]!
const floats = [f32(1), 2.5, 3]
const negative = -i16(4)
const complement = ~u8(1)
const referenced = [nested[0]]
const aliased = [Count(1)]!
const wide = [u128(1)]!
const called = [make_number()]!
const expression = [u64(1) + 2]!
fn make_number() u64 { return 1 }
fn main() {}
')
	for name in ['nested', 'floats', 'negative', 'complement'] {
		id := tc.const_exprs[name] or { panic('missing ${name}') }
		assert independent_numeric_const_initializer(a, id), name
	}
	for name in ['referenced', 'aliased', 'wide', 'called', 'expression'] {
		id := tc.const_exprs[name] or { panic('missing ${name}') }
		assert !independent_numeric_const_initializer(a, id), name
	}
	assert numeric_const_type_name(tc, 'nested') == '[2][2]u64'
	assert numeric_const_type_name(tc, 'floats') == '[]f32'
}

fn test_numeric_const_resolution_keeps_forward_dependencies_and_type_dimensions() {
	_, tc := numeric_const_fixture('dependencies', 'module main
const dependent = [later]!
const later = [last]!
const last = f64(1)
const bound = 2
const sized = [bound]int{}
const pure = [[u64(4), 5]!, [u64(6), 7]!]!
fn main() {}
')
	assert numeric_const_type_name(tc, 'dependent') == '[1][1]f64'
	assert numeric_const_type_name(tc, 'later') == '[1]f64'
	assert numeric_const_type_name(tc, 'last') == 'f64'
	assert numeric_const_type_name(tc, 'sized') == '[bound]int'
	assert numeric_const_type_name(tc, 'pure') == '[2][2]u64'
	tc.resolve_const_types()
	assert numeric_const_type_name(tc, 'dependent') == '[1][1]f64'
	assert numeric_const_type_name(tc, 'pure') == '[2][2]u64'
}

fn test_numeric_const_resolution_retries_after_initializer_changes() {
	mut a, mut tc := numeric_const_fixture('rewrite', 'module main
const table = [u32(1), 2]!
fn main() {}
')
	assert numeric_const_type_name(tc, 'table') == '[2]u32'
	mut replaced := false
	for i, node in a.nodes {
		if node.kind == .cast_expr && node.source_value_id() == flat.source_type_u32 {
			a.nodes[i].set_source_value_id(flat.source_type_u64)
			replaced = true
		}
	}
	assert replaced
	tc.resolve_const_types()
	assert numeric_const_type_name(tc, 'table') == '[2]u64'
}

fn test_independent_numeric_const_initializer_rejects_type_annotation_dependencies() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '1', typ: flat.source_type_id('Number') })
	assert !independent_numeric_const_initializer(&a, literal)
	a.nodes[int(literal)].typ = flat.source_type_u64
	assert independent_numeric_const_initializer(&a, literal)
	children := a.begin_children()
	a.add_child(literal)
	array := a.add_node(flat.Node{
		kind:           .array_literal
		typ:            flat.source_type_id('[bound]u64')
		children_start: children
		children_count: 1
	})
	assert !independent_numeric_const_initializer(&a, array)
	assert !independent_numeric_const_initializer(&a, flat.empty_node)
}
