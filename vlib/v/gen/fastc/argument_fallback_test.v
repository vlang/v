module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_argument_fallback_parser(selfhost bool, translated bool, mode int) Parser {
	prefs := pref.new_preferences()
	mut record_fields := flat.TypeTextBindings{}
	record_fields.set('next', 'Record*')
	record_fields.set('value', 'int')
	return Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		selfhost:                selfhost
		translated:              translated
		in_mono_drain:           mode == 1
		in_generic_placeholder:  mode == 2
		module_name:             'fixture'
		numeric_ops_are_builtin: true
		primitive_cmps_builtin:  true
		declared_kinds:          {
			'Record': .struct_
			'Mode':   .enum_
		}
		struct_fields:           {
			'Record': record_fields
		}
		locals:                  {
			'node':    FastcLocal{ typ: flat.source_type_id('Record*') }
			'pointer': FastcLocal{ typ: flat.source_type_id('int*') }
			'value':   FastcLocal{ typ: flat.source_type_int }
		}
	}
}

fn fastc_argument_fallback_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('argument_fallback.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn test_fastc_argument_fallback_reuses_only_pure_special_misses() {
	for mode in 0 .. 3 {
		for source in ['node', 'node.next', 'node.next.value', 'pointer', 'value + 3', 'value > 3',
			'value & 7', '.ready', 'unknown', '"literal"', 'node.value + value'] {
			mut slow := fastc_argument_fallback_parser(true, false, mode)
			mut fast := fastc_argument_fallback_parser(true, false, mode)
			tokens := fastc_argument_fallback_tokens(source)
			flags := fastc_special_token_flags(tokens)
			assert fast.can_reuse_special_expression_miss(flags), source
			expected := if source == '.ready' {
				flat.source_type_id('Mode')
			} else {
				fast.infer_expression_type(tokens) or { panic(source) }
			}
			raw := fast.render_raw_expression_tokens(tokens) or { panic(source) }
			slow_raw := slow.render_raw_expression_tokens(tokens) or { panic(source) }
			assert slow_raw == raw
			assert slow.render_special_expression(tokens, raw) == none, source
			assert fast.render_special_expression_with_flags(tokens, raw, flags) == none, source
			// The legacy call performs its own raw render and second special dispatch.
			legacy := slow.render_membership_candidate(tokens, expected) or { panic(source) }
			reused := fast.render_membership_candidate_with_raw(tokens, expected, raw, true) or { panic(source) }
			assert reused == legacy, source
			fast_type := fast.infer_expression_type(tokens) or { panic(source) }
			slow_type := slow.infer_expression_type(tokens) or { panic(source) }
			assert fast_type == slow_type
			assert fast.temp_id == slow.temp_id && fast.temp_id == 0
			assert fast.last_expression == slow.last_expression
			assert fast.last_expression_type == slow.last_expression_type
			assert fast.last_option_value_type == slow.last_option_value_type
			assert fast.last_multi_return_types.ids == slow.last_multi_return_types.ids
			assert fast.composite_types == slow.composite_types
			assert fast.fixed_array_types.ids == slow.fixed_array_types.ids
			assert fast.spawn_helpers == slow.spawn_helpers
			assert fast.mono_functions.len == slow.mono_functions.len
			assert fast.member_smartcasts.len == 0 && slow.member_smartcasts.len == 0
			assert fast.out.str() == slow.out.str()
			assert fast.protos.str() == slow.protos.str()
			assert fast.s.offset == slow.s.offset && fast.s.offset == 0
		}
	}
}

fn test_fastc_argument_fallback_preserves_complex_dispatch_and_narrowing() {
	mut g := fastc_argument_fallback_parser(true, false, 0)
	for source in ['call(value)', '(value)', 'node[0]', 'Record{}', 'value = 1', 'value as int',
		'value is int', 'typeof(value)', '!value', 'call()?', 'value && true', 'value in node',
		'value !in node'] {
		assert !g.can_reuse_special_expression_miss(fastc_special_token_flags(fastc_argument_fallback_tokens(source))), source
	}
	atom := [FastcExpressionToken{ tok: .name, source: '({ side_effect(); value; })', typ: flat.source_type_int }]
	assert !g.can_reuse_special_expression_miss(fastc_special_token_flags(atom))
	// Non-name synthetic tokens remain opaque, copied verbatim by both paths.
	string_atom := [FastcExpressionToken{ tok: .string, lit: '"ignored"', source: '_S("literal")', typ: flat.source_type_string }]
	string_raw := g.render_raw_expression_tokens(string_atom) or { panic('opaque string') }
	assert g.can_reuse_special_expression_miss(fastc_special_token_flags(string_atom))
	assert g.render_special_expression(string_atom, string_raw) == none
	legacy_string := g.render_membership_candidate(string_atom, flat.source_type_string) or { panic('legacy string') }
	reused_string := g.render_membership_candidate_with_raw(string_atom, flat.source_type_string, string_raw, true) or { panic('reused string') }
	assert legacy_string == reused_string && reused_string == string_raw
	plain := fastc_special_token_flags(fastc_argument_fallback_tokens('node.value'))
	g.member_smartcasts['other'] = FastcMemberSmartcast{ typ: flat.source_type_int, source: 'narrowed' }
	assert !g.can_reuse_special_expression_miss(plain)
	ordinary := fastc_argument_fallback_parser(false, false, 0)
	assert !ordinary.can_reuse_special_expression_miss(plain)
	translated := fastc_argument_fallback_parser(true, true, 0)
	assert !translated.can_reuse_special_expression_miss(plain)
}

fn test_fastc_argument_fallback_member_and_scalar_values_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_argument_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Record {
	next &Record = unsafe { nil }
	value int
}
fn read_int(value int) int { return value }
fn read_pointer(value &Record) int { return value.value }
fn read_bool(value bool) bool { return value }
fn read_string(value string) string { return value }
fn main() {
	tail := &Record{value: 9}
	head := &Record{next: tail, value: 5}
	assert read_pointer(head.next) == 9
	assert read_int(head.next.value) == 9
	assert read_int(head.next.value + head.value) == 14
	assert read_int(head.next.value & 7) == 1
	assert read_bool(head.value > 3)
	assert read_string("literal") == "literal"
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
