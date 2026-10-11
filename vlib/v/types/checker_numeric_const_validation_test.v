module types

import os
import v.flat
import v.parser
import v.pref

fn numeric_const_batch_checked(name string, source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'numeric_array_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	return tc
}

fn test_numeric_const_batch_checked_casts_preserve_fixed_array_and_leaf_types() {
	tc := numeric_const_batch_checked('casts', 'module main
const values = [u64(1), u64(0xffffffffffffffff), u64(2)]!
const grid = [[f32(1), 2.0]!, [f32(3), 4.0]!]!
fn main() {}
')
	assert tc.errors.len == 0, tc.errors.str()
	assert (tc.const_types['values'] or { panic('missing values') }).name() == '[3]u64'
	assert (tc.const_types['grid'] or { panic('missing grid') }).name() == '[2][2]f32'
	for idx, node in tc.a.nodes {
		if node.kind == .float_literal {
			assert tc.expr_type(flat.NodeId(idx)) == none
		}
	}
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_const_batch_literal_context_still_publishes_checked_types() {
	tc := numeric_const_batch_checked('context', 'module main
fn accept(values []f32) {}
fn main() {
 accept([1.25, 2, 3.5])
}
')
	assert tc.errors.len == 0, tc.errors.str()
	for idx, node in tc.a.nodes {
		if node.kind in [.int_literal, .float_literal] {
			assert tc.expr_type(flat.NodeId(idx)) == none
		}
	}
	for idx, node in tc.a.nodes {
		if node.kind == .array_literal {
			assert (tc.expr_type(flat.NodeId(idx)) or { panic('missing contextual array type') }).name() == '[]f32'
		}
	}
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_const_batch_literal_range_checks_are_not_skipped() {
	tc := numeric_const_batch_checked('overflow', 'module main
const values = [u8(1), u8(256)]!
const signed = [i16(1), i16(32768)]!
const wide = [u64(1), u64(18446744073709551616)]!
fn main() {}
')
	assert tc.errors.any(it.msg == 'value `256` overflows `u8`'), tc.errors.str()
	assert tc.errors.any(it.msg == 'value `18446744073709551616` overflows `u64`'), tc.errors.str()
	assert tc.notices.any(it.msg.contains('value `32768` overflows `i16`')), tc.notices.str()
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_const_batch_heterogeneous_values_keep_mismatch_diagnostics() {
	tc := numeric_const_batch_checked('mismatch', "module main
const values = [i64(1), 'bad']!
fn optional() ?int { return 1 }
fn accept(values []int) {}
fn main() { accept([int(1), optional()]) }
")
	assert tc.errors.any(it.msg.contains('invalid array element: expected `i64`, not `string`')), tc.errors.str()
	assert tc.errors.any(it.msg.contains('must be unwrapped first')), tc.errors.str()
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_const_batch_aliases_and_mixed_widths_use_existing_inference() {
	tc := numeric_const_batch_checked('aliases', 'module main
type Small = u8
const aliased = [Small(7), Small(8)]!
const values = [u64(1), u32(2)]!
fn main() {}
')
	assert tc.errors.len == 0, tc.errors.str()
	aliased := tc.const_types['aliased'] or { panic('missing alias array') }
	assert aliased is ArrayFixed
	assert aliased.elem_type is Alias
	assert (tc.const_types['values'] or { panic('missing values') }).name() == '[2]u64'
}

fn test_numeric_const_batch_rejects_generic_primitive_spelling_metadata() {
	mut tc := numeric_const_batch_checked('generic_metadata', 'module main
const values = [u64(1), u64(2)]!
fn main() {}
')
	for idx, node in tc.a.nodes {
		if node.kind == .const_field && node.value == 'values' {
			id := tc.a.child(node, 0)
			typ := tc.const_types['values'] or { panic('missing values') }
			assert tc.known_numeric_const_initializer(id, typ)
			tc.sum_generic_params['u64'] = ['T']
			assert !tc.known_numeric_const_initializer(id, typ)
			tc.sum_generic_params.delete('u64')
			tc.interface_generic_params['u64'] = ['T']
			assert !tc.known_numeric_const_initializer(id, typ)
			_ = idx
		}
	}
}

fn test_numeric_const_batch_rejects_ragged_and_nonliteral_values() {
	tc := numeric_const_batch_checked('fallback', 'module main
const other = u64(9)
const values = [u64(1), other]!
const negative = [i64(1), i64(-2)]!
const wide = [u128(1), u128(2)]!
fn main() {}
')
	assert tc.errors.len == 0, tc.errors.str()
	for node in tc.a.nodes {
		if node.kind == .const_field && node.value in ['values', 'negative', 'wide'] {
			id := tc.a.child(node, 0)
			typ := tc.const_types[node.value] or { panic('missing field') }
			assert !tc.known_numeric_const_initializer(id, typ)
		}
	}
}

fn test_numeric_const_batch_activates_before_array_sidecars_exist() {
	path := os.join_path(os.vtmp_dir(), 'numeric_batch_cold_${os.getpid()}.v')
	os.write_file(path, 'module main
const values = [u64(1), u64(2), 3]!
const rows = [[f32(1), 2.0]!, [f32(3), 4.0]!]!
fn main() {}
') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	mut accepted := 0
	for node in a.nodes {
		if node.kind == .const_field && node.value in ['values', 'rows'] {
			id := a.child(node, 0)
			array_id := a.child(a.node(id), 0)
			typ := tc.const_types[node.value] or { panic('missing const type') }
			assert tc.expr_type(array_id) == none
			assert tc.known_numeric_const_initializer(id, typ)
			assert tc.check_known_numeric_const_initializer(id, typ)
			assert semantic_types_equal(tc.expr_type(id) or { panic('missing fixed type') }, typ)
			assert tc.expr_type(array_id) != none
			accepted++
		}
	}
	assert accepted == 2
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_const_batch_preserves_synthetic_wrapped_sidecar_fallbacks() {
	mut tc := numeric_const_batch_checked('wrapped_metadata', 'module main
const values = [u64(1), u64(2)]!
fn main() {}
')
	for node in tc.a.nodes {
		if node.kind == .const_field && node.value == 'values' {
			id := tc.a.child(node, 0)
			array_id := tc.a.child(tc.a.node(id), 0)
			typ := tc.const_types['values'] or { panic('missing values') }
			array_type := tc.expr_type(array_id) or { panic('missing array') }
			assert tc.known_numeric_const_initializer(id, typ)
			tc.register_synth_type(array_id, Type(Alias{ name: 'Wrapped', base_type: array_type }))
			assert !tc.known_numeric_const_initializer(id, typ)
			tc.register_synth_type(array_id, Type(OptionType{ base_type: array_type }))
			assert !tc.known_numeric_const_initializer(id, typ)
			tc.register_synth_type(array_id, array_type)
			cast_id := tc.a.child(tc.a.node(array_id), 0)
			literal_id := tc.a.child(tc.a.node(cast_id), 0)
			tc.register_synth_type(literal_id, Type(OptionType{ base_type: builtin_int_type }))
			assert !tc.known_numeric_const_initializer(id, typ)
			assert (tc.expr_type(literal_id) or { panic('missing wrapper') }) is OptionType
		}
	}
}
