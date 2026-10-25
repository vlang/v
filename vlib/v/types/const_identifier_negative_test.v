module types

import os
import v.flat
import v.parser
import v.pref

fn test_const_identifier_filter_preserves_text_and_module_resolution() {
	mut a := flat.FlatAst.new()
	known := a.add_val(.int_literal, '17')
	other := a.add_val(.int_literal, '29')
	mut tc := TypeChecker.new(a)
	for name in ['known', 'word_with_1', '_declared', 'helper.value', 'other.value'] {
		tc.const_exprs[name] = if name == 'other.value' { other } else { known }
		tc.const_types[name] = builtin_int_type
		tc.const_modules[name] = if name.contains('.') { name.all_before_last('.') } else { 'main' }
	}
	tc.build_const_suffixes()
	names := ['known', 'word_with_1', '_declared', 'value', 'missing', 'runtime_local', '_123',
		'123', '+17', '-17', 'known + 2', 'int(17)', '(known)', 'helper.value', 'other.value',
		'Color.red', 'a[0]', '', 'x + y', 'éclair']
	for module_name in ['main', 'builtin', 'helper', 'other'] {
		tc.cur_module = module_name
		for name in names {
			// Disable the snapshot by changing its counted table size before the slow query.
			tc.const_exprs['temporary_filter_key'] = known
			tc.prepare_check_name_filters()
			tc.const_exprs.delete('temporary_filter_key')
			slow := tc.const_int_value_in_module(name, module_name, []string{})
			tc.prepare_check_name_filters()
			fast := tc.const_int_value_in_module(name, module_name, []string{})
			assert (slow == none) == (fast == none), '${module_name}:${name}'
			assert (slow or { -9999 }) == (fast or { -9999 }), '${module_name}:${name}'
			println('CONST_VALUE:${module_name}:${name}:${fast or { -9999 }}')
		}
	}
	assert tc.const_int_value_in_module('_123', 'main', []string{})? == 123
	assert tc.const_int_value_in_module('known', 'main', ['known']) == none
}

fn test_const_identifier_filter_tracks_additions_and_builtin_shadowing() {
	mut a := flat.FlatAst.new()
	first := a.add_val(.int_literal, '11')
	second := a.add_val(.int_literal, '22')
	mut tc := TypeChecker.new(a)
	tc.const_exprs['value'] = first
	tc.const_types['value'] = builtin_int_type
	tc.const_modules['value'] = 'builtin'
	tc.prepare_check_name_filters()
	assert tc.const_int_value_in_module('missing', 'main', []string{}) == none
	tc.cur_module = 'main'
	key := tc.const_decl_key('value')
	tc.const_exprs[key] = second
	tc.const_types[key] = builtin_int_type
	tc.const_modules[key] = 'main'
	assert tc.const_int_value_in_module('value', 'main', []string{})? == 22
	assert tc.const_int_value_in_module('value', 'builtin', []string{})? == 11
	tc.prepare_check_name_filters()
	tc.const_exprs['added'] = first
	tc.const_types['added'] = builtin_int_type
	assert tc.const_int_value_in_module('added', 'main', []string{})? == 11
	tc.const_exprs.delete('added')
	tc.const_types.delete('added')
	tc.prepare_check_name_filters()
	tc.const_exprs['value'] = first
	assert tc.const_int_value_in_module('value', 'main', []string{})? == 11
	fork := tc.fork_for_parallel_codegen()
	assert fork.const_int_value_in_module('value', 'main', []string{})? == 11
	assert fork.const_int_value_in_module('missing', 'main', []string{}) == none
}

fn test_const_identifier_filter_preserves_checked_source_diagnostics() {
	cases := [
		'fn accept(value u8) {}\nfn main() { mut count := 1; count++; accept(count) }',
		'const known = 17\nfn accept(value u8) {}\nfn main() { accept(known); accept(known + 250) }',
		'fn main() { count := 3; _ = [count]int{} }',
		'const width = int(2 * (3 + 1))\nfn main() { _ = [width]int{} }',
		'enum Color { red = 3 blue = 9 }\nfn main() { _ = [Color.blue]int{} }',
		'const _declared = 7\nfn accept(value u8) {}\nfn main() { accept(_declared) }',
		'const cycle = cycle\nfn main() { _ = cycle }',
		'type Count = int\nfn accept(value u8) {}\nfn main() { value := Count(3); accept(value) }',
	]
	for index, source in cases {
		for scoped in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'const_identifier_negative_${index}_${os.getpid()}.v')
			os.write_file(path, 'module main\n' + source + '\n')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
			mut tc := TypeChecker.new(a)
			tc.scope_parallel_check_workers = scoped
			tc.collect(a)
			_ = tc.check_semantics_opt(false)
			os.rm(path) or {}
			println('CONST_CASE ${index} scoped=${scoped}: errors=${tc.errors.len} notices=${tc.notices.len}')
			for err in tc.errors {
				println('CONST_ERROR:${err.kind}:${int(err.node)}:${err.pos.offset}:${err.pos.end}:${err.msg}:${err.details.join('|')}')
			}
			for notice in tc.notices {
				println('CONST_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
			}
		}
	}
}
