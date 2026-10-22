module types

import os
import v.flat
import v.parser
import v.pref

fn test_c_flag_initializers_require_known_bit_positions() {
	for i, backing in ['', ' as u64'] {
		path := os.join_path(os.vtmp_dir(), 'c_flag_initializer_${i}_${os.getpid()}.v')
		os.write_file(path, '@[flag]\nenum CFlags${backing} { first = C.enum_first_opt second }\nenum Position { third = 3 }\n@[flag]\nenum KnownFlags${backing} { first = int(Position.third) second }\nfn main() {}') or { panic(err) }
		defer { os.rm(path) or {} }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		_ = tc.check_semantics_opt(false)
		assert tc.errors.any(it.msg == 'flag enum values must be known at compile time'), tc.errors.str()
		assert tc.errors.len == 1, tc.errors.str()
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
