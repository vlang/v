module types

import os
import v.flat
import v.parser
import v.pref

fn test_c_flag_initializers_require_known_bit_positions() {
	for i, backing in ['', ' as u64'] {
		path := os.join_path(os.vtmp_dir(), 'c_flag_initializer_${i}_${os.getpid()}.v')
		os.write_file(path, '@[flag]\nenum CFlags${backing} { first = C.enum_first_opt second }\nenum Position { third = 3 }\n@[flag]\nenum KnownFlags${backing} { first = int(Position.third) second }\nfn main() { _ = unsafe { CFlags(32) } _ = unsafe { KnownFlags(32) } }') or { panic(err) }
		defer { os.rm(path) or {} }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		_ = tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg == 'flag enum values must be known at compile time'), tc.errors.str()
		assert tc.errors.len == 1, tc.errors.str()
		cast_warnings := tc.notices.filter(it.msg.contains('does not represent a value of enum'))
		assert cast_warnings.len == 1, cast_warnings.str()
		assert cast_warnings[0].msg == '32 does not represent a value of enum KnownFlags'
	}
}

fn test_enum_selector_values_preserve_dependencies_and_reject_cycles() {
	for mod_name in ['main', 'models'] {
		path := os.join_path(os.vtmp_dir(), 'enum_selector_${mod_name}_${os.getpid()}.v')
		os.write_file(path, 'module ${mod_name}
enum Position { third = 3 }
enum Indirect { third = int(Position.third) }
enum Mixed { third = int(Later.third) }
enum Later { third = int(Position.third) fourth = int(Mixed.third) + 1 }
enum CycleA { item = int(CycleB.item) }
enum CycleB { item = int(CycleA.item) }
enum CSource { item = C.unknown_value }
enum IndirectC { item = int(CSource.item) }
') or { panic(err) }
		defer { os.rm(path) or {} }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		prefix := if mod_name == 'main' { '' } else { '${mod_name}.' }
		for name, expected in {
			'Indirect': i64(3)
			'Mixed':    3
			'Later':    4
		} {
			cases := tc.comptime_static_enum_decl_value_cases(flat.source_type_id('${prefix}${name}'))
			assert cases.len > 0, name
			assert !cases.last().value_from_c, name
			assert cases.last().value == expected, name
		}
		for name in ['CycleA', 'CycleB', 'IndirectC'] {
			cases := tc.comptime_static_enum_decl_value_cases(flat.source_type_id('${prefix}${name}'))
			assert cases.len == 1, name
			assert cases[0].value_from_c, name
			assert tc.comptime_enum_decl_field_values(flat.source_type_id('${prefix}${name}')).len == 0, name
			assert tc.const_int_enum_selector_value('${prefix}${name}.item') == none, name
			assert comptime_static_subst_value_cond('member.value == 0', 'member', cases[0]).contains('.value'), name
		}
	}
}

fn test_unknown_enum_successor_allows_explicit_zero_reset() {
	path := os.join_path(os.vtmp_dir(), 'enum_unknown_reset_${os.getpid()}.v')
	os.write_file(path, 'enum UnknownReset { first = C.unknown_value next zero = 0 after_zero }\nfn main() {}') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	cases := tc.comptime_static_enum_decl_value_cases(flat.source_type_id('UnknownReset'))
	assert cases.len == 4
	assert cases[0].value_from_c
	assert cases[1].value_from_c
	assert !cases[2].value_from_c
	assert cases[2].value == 0
	assert !cases[3].value_from_c
	assert cases[3].value == 1
}

fn test_enum_helper_folding_rejects_incomplete_bodies_before_evaluation() {
	path := os.join_path(@VMODROOT, 'vlib/v/checker/tests/enum_comptime_body_err.vv')
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	cases := tc.comptime_static_enum_decl_value_cases(flat.source_type_id('UnsupportedComptimeBody'))
	assert cases.len == 8
	assert cases.all(it.value_from_c), cases.str()
}

fn test_enum_helper_folding_requires_exact_argument_count() {
	path := os.join_path(os.vtmp_dir(), 'enum_helper_arity_${os.getpid()}.v')
	os.write_file(path, '@[comptime]\nfn enum_identity(value int) int { return value }\nenum Arity { missing = enum_identity() extra = enum_identity(1, 2) }') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	cases := tc.comptime_static_enum_decl_value_cases(flat.source_type_id('Arity'))
	assert cases.len == 2
	assert cases.all(it.value_from_c), cases.str()
}

fn test_enum_metadata_requires_exact_module_identity() {
	path := os.join_path(os.vtmp_dir(), 'enum_metadata_identity_${os.getpid()}.v')
	os.write_file(path, 'module consumer\nenum Values { first = 12 }') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	for node in a.nodes {
		if node.kind != .enum_decl {
			continue
		}
		mut resolving := map[string]bool{}
		assert tc.comptime_static_enum_decl_value_cases_for_node(flat.source_type_id('Values'),
			'consumer', node, '', mut resolving) == none
		cases := tc.comptime_static_enum_decl_value_cases_for_node(flat.source_type_id('consumer.Values'),
			'consumer', node, '', mut resolving) or { panic('missing enum') }
		assert cases.len == 1
		assert cases[0].value == 12
		return
	}
	assert false, 'missing enum declaration'
}
