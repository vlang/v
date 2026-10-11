module types

import os
import v.flat
import v.parser
import v.pref

fn numeric_cast_checker(name string, source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'numeric_cast_${name}_${os.getpid()}.v')
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

fn test_numeric_literal_cast_checks_builtin_widths_and_float_types() {
	tc := numeric_cast_checker('widths', 'module main
const bytes = [u8(0xff), u8(0)]!
const words = [[u64(0xffffffffffffffff), u64(1)]!, [u64(2), u64(3)]!]!
fn main() {
 _ = int(2147483647)
 _ = i8(127)
 _ = i16(32767)
 _ = i32(2147483647)
 _ = i64(9223372036854775807)
 _ = u16(65535)
 _ = u32(4294967295)
 _ = f32(1.25)
 _ = f64(42)
 _ = int(1.25)
}
')
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.notices.len == 0, tc.notices.str()
	assert (tc.const_types['bytes'] or { panic('missing bytes') }).name() == '[2]u8'
	assert (tc.const_types['words'] or { panic('missing words') }).name() == '[2][2]u64'
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_literal_cast_preserves_overflow_errors_and_warnings() {
	tc := numeric_cast_checker('overflow', 'module main
fn main() {
 _ = u8(256)
 _ = u64(18446744073709551616)
 _ = i16(32768)
}
')
	assert tc.errors.any(it.msg == 'value `256` overflows `u8`'), tc.errors.str()
	assert tc.errors.any(it.msg == 'value `18446744073709551616` overflows `u64`'), tc.errors.str()
	assert tc.notices.any(it.msg == 'value `32768` overflows `i16`, this will be considered hard error soon' && it.severity == 'warning:'), tc.notices.str()
	assert tc.checking_nodes.all(!it)
}

fn test_numeric_literal_cast_keeps_alias_wrappers_prefixes_and_wide_checks() {
	tc := numeric_cast_checker('fallbacks', 'module main
type Small = u8
fn main() {
 _ = Small(256)
 _ = bool(1)
 _ = string(1)
 _ = i16(-32768)
 _ = u128(340282366920938463463374607431768211456)
}
')
	assert tc.errors.any(it.msg == 'value `256` overflows `Small`'), tc.errors.str()
	assert tc.errors.any(it.msg.starts_with('cannot cast to bool')), tc.errors.str()
	assert tc.errors.any(it.msg == 'cannot cast number to string, use `1.str()` instead.'), tc.errors.str()
	assert tc.errors.any(it.msg == 'value `340282366920938463463374607431768211456` overflows `u128`'), tc.errors.str()
	assert !tc.notices.any(it.msg.contains('`-32768` overflows `i16`')), tc.notices.str()
}

fn test_numeric_literal_cast_retains_generic_target_declaration_validation() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	children := a.begin_children()
	a.add_child(literal)
	cast := a.add_node(flat.Node{ kind: .cast_expr, value: 'Choice', children_start: children, children_count: 1 })
	mut tc := TypeChecker.new(&a)
	tc.diagnose_unknown_calls = true
	tc.sum_types['Choice'] = ['int']
	tc.sum_generic_params['Choice'] = ['T']
	tc.check_cast_expr(cast, *a.node(cast))
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'generic sumtype `Choice` must specify type parameter, e.g. Choice[int]'
}

fn test_numeric_literal_cast_keeps_builtin_spelling_metadata_behavior() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	children := a.begin_children()
	a.add_child(literal)
	cast := a.add_node(flat.Node{ kind: .cast_expr, value: 'u64', children_start: children, children_count: 1 })
	for sum_type in [true, false] {
		mut tc := TypeChecker.new(&a)
		tc.diagnose_unknown_calls = true
		tc.type_aliases['u64'] = 'i8'
		if sum_type {
			tc.sum_generic_params['u64'] = ['T']
		} else {
			tc.interface_generic_params['u64'] = ['T']
		}
		assert (tc.bare_generic_decl_type_name('u64') or { panic('missing generic declaration') }) == 'u64'
		assert tc.qualify_name('u64') == 'u64'
		assert tc.parse_type('u64').name() == 'u64'
		tc.check_cast_expr(cast, *a.node(cast))
		assert tc.errors.len == 1, tc.errors.str()
		expected := if sum_type {
			'generic sumtype `u64` must specify type parameter, e.g. u64[int]'
		} else {
			'could not infer generic type `T` in interface `u64`'
		}
		assert tc.errors[0].msg == expected, tc.errors.str()
	}
}

fn test_numeric_literal_cast_retains_checked_literal_wrapper_types() {
	mut a := flat.FlatAst.new()
	literal := a.add_node(flat.Node{ kind: .int_literal, value: '1' })
	children := a.begin_children()
	a.add_child(literal)
	cast := a.add_node(flat.Node{ kind: .cast_expr, value: 'u64', children_start: children, children_count: 1 })
	mut tc := TypeChecker.new(&a)
	tc.diagnose_unknown_calls = true
	tc.trust_checked_expr_types = true
	tc.register_synth_type(literal, Type(OptionType{ base_type: builtin_int_type }))
	tc.check_cast_expr(cast, *a.node(cast))
	assert tc.errors.any(it.msg == 'cannot type cast an Option'), tc.errors.str()
}
