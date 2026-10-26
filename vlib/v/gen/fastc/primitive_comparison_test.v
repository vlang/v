module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_primitive_comparison_parser(allow bool) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:                  prefs
		s:                      scanner.new_scanner(prefs, .normal)
		selfhost:               true
		primitive_cmps_builtin: allow
		locals:                 {
			'a':          FastcLocal{ typ: flat.source_type_int }
			'b':          FastcLocal{ typ: flat.source_type_int, is_mut: true }
			'byte':       FastcLocal{ typ: flat.source_type_u8 }
			'tiny':       FastcLocal{ typ: flat.source_type_i8 }
			'wide':       FastcLocal{ typ: flat.source_type_u64 }
			'other_wide': FastcLocal{ typ: flat.source_type_u64 }
			'f':          FastcLocal{ typ: flat.source_type_f64 }
			'g':          FastcLocal{ typ: flat.source_type_f64 }
			'small_f':    FastcLocal{ typ: flat.source_type_f32 }
			'yes':        FastcLocal{ typ: flat.source_type_bool }
			'no':         FastcLocal{ typ: flat.source_type_bool }
			'character':  FastcLocal{ typ: flat.source_type_char }
			'rune_value': FastcLocal{ typ: flat.source_type_rune }
			'alias':      FastcLocal{ typ: flat.source_type_id('Alias') }
			'text':       FastcLocal{ typ: flat.source_type_string }
			'pointer':    FastcLocal{ typ: flat.source_type_id('int*') }
			'reference':  FastcLocal{ typ: flat.source_type_int, is_reference: true }
			'optional':   FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_int }
			'origin':     FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Choice') }
		}
	}
}

fn fastc_primitive_comparison_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('primitive_comparison.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn test_fastc_plain_primitive_comparisons_preserve_full_legacy_dispatch() {
	slow := fastc_primitive_comparison_parser(false)
	fast := fastc_primitive_comparison_parser(true)
	for source in ['a == b', 'a != 0', '0 < a', 'a <= 100', 'a > b', 'a >= b', 'wide == other_wide',
		'wide >= 0', 'byte < 255', 'f < g', 'f == 1.25', '1.25 >= f', 'small_f != 0.0', '1_000 == 1000',
		'0xffff == 65_535', 'yes == true', 'false != no', 'yes == no', 'true != false'] {
		tokens := fastc_primitive_comparison_tokens(source)
		assert fast.plain_primitive_comparison_expression(tokens), source
		assert !slow.plain_primitive_comparison_expression(tokens), source
		assert slow.render_overloaded_binary_expression(tokens) == none, source
		assert fast.render_overloaded_binary_expression(tokens) == none, source
		raw := slow.render_raw_expression_tokens(tokens) or { panic(source) }
		assert slow.render_special_expression(tokens, raw) == none, source
		assert fast.render_special_expression(tokens, raw) == none, source
		fast_type := fast.infer_expression_type(tokens) or { panic(source) }
		slow_type := slow.infer_expression_type(tokens) or { panic(source) }
		assert fast_type == flat.source_type_bool && fast_type == slow_type, source
		fast_argument := fast.render_call_argument_expression(tokens, flat.source_type_bool) or { panic(source) }
		slow_argument := slow.render_call_argument_expression(tokens, flat.source_type_bool) or { panic(source) }
		assert fast_argument == slow_argument, source
		assert fast.temp_id == slow.temp_id && fast.temp_id == 0
		assert fast.last_expression_type == 0 && slow.last_expression_type == 0
		assert fast.last_option_value_type == 0 && slow.last_option_value_type == 0
		assert fast.last_expression.len == 0 && slow.last_expression.len == 0
		assert fast.last_multi_return_types.ids.len == 0 && slow.last_multi_return_types.ids.len == 0
		assert fast.s.offset == 0 && slow.s.offset == 0
		assert fast.s.diagnostics.len == 0 && slow.s.diagnostics.len == 0
	}
}

fn test_fastc_plain_primitive_comparisons_preserve_mixed_sign_helpers_and_reject_complex_shapes() {
	fast := fastc_primitive_comparison_parser(true)
	slow := fastc_primitive_comparison_parser(false)
	for source in ['a == wide', 'wide != a', 'a < wide', 'wide <= a', 'a > wide', 'wide >= a'] {
		tokens := fastc_primitive_comparison_tokens(source)
		assert !fast.plain_primitive_comparison_expression(tokens), source
		raw := fast.render_raw_expression_tokens(tokens) or { panic(source) }
		fast_special := fast.render_special_expression(tokens, raw) or { panic(source) }
		slow_special := slow.render_special_expression(tokens, raw) or { panic(source) }
		assert fast_special.source == slow_special.source, source
		assert fast_special.source.contains('v_fastc_us_'), source
		assert fast_special.typ == slow_special.typ && fast_special.typ == flat.source_type_bool
	}
	for source in ['tiny == a', 'byte == wide', 'small_f == f', 'f == a'] {
		tokens := fastc_primitive_comparison_tokens(source)
		assert !fast.plain_primitive_comparison_expression(tokens), source
		raw := fast.render_raw_expression_tokens(tokens) or { panic(source) }
		assert fast.render_special_expression(tokens, raw) == none, source
		assert slow.render_special_expression(tokens, raw) == none, source
		fast_argument := fast.render_call_argument_expression(tokens, flat.source_type_bool) or { panic(source) }
		slow_argument := slow.render_call_argument_expression(tokens, flat.source_type_bool) or { panic(source) }
		assert fast_argument == slow_argument, source
	}
	for source in ['a', '(a == b)', 'a + 1 == b', 'a & b == 0', 'a == b && yes', 'a == call()',
		'a.field == 0', 'a[0] == b', 'a = b == 0', '-a < b', 'a < -1', 'pointer == pointer',
		'reference == a', 'optional == none', 'origin == a', 'alias == a', 'text == text',
		'character == a', 'rune_value == a', 'yes < no', 'yes == a', 'a == f', 'byte == wide',
		'tiny == a', 'small_f == f', 'unknown == a', 'a =='] {
		assert !fast.plain_primitive_comparison_expression(fastc_primitive_comparison_tokens(source)), source
	}
	for operator in [
		FastcExpressionToken{ tok: .eq, source: '__synthetic_operator' },
		FastcExpressionToken{ tok: .eq, is_mut_argument: true },
		FastcExpressionToken{ tok: .eq, typ: flat.source_type_bool },
	] {
		assert !fast.plain_primitive_comparison_expression([
			FastcExpressionToken{ tok: .name, lit: 'a' },
			operator,
			FastcExpressionToken{ tok: .name, lit: 'b' },
		])
	}
	for value in [
		FastcExpressionToken{ tok: .name, lit: 'a', source: '__lowered', typ: flat.source_type_int },
		FastcExpressionToken{ tok: .name, lit: 'a', typ: flat.source_type_u64 },
		FastcExpressionToken{ tok: .name, lit: 'a', is_mut_argument: true },
		FastcExpressionToken{ tok: .number, lit: '1', typ: flat.source_type_int },
	] {
		assert !fast.plain_primitive_comparison_expression([value, FastcExpressionToken{ tok: .eq },
			FastcExpressionToken{ tok: .name, lit: 'a' }])
	}
}

fn test_fastc_primitive_comparison_metadata_disables_custom_or_ambiguous_layouts() {
	assert fastc_primitive_comparisons_have_no_overloads(map[string]FastcFunctionSignature{},
		flat.TypeTextBindings{}, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
	for name in ['int.==', 'u64.<', 'f32.==', 'bool.<'] {
		assert !fastc_primitive_comparisons_have_no_overloads({
			name: FastcFunctionSignature{}
		},
			flat.TypeTextBindings{}, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{}), name
	}
	for name in ['bool', 'int', 'u64', 'f32'] {
		mut bindings := flat.TypeTextBindings{}
		bindings.set(name, 'Declared')
		assert !fastc_primitive_comparisons_have_no_overloads(map[string]FastcFunctionSignature{},
			bindings, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
		assert !fastc_primitive_comparisons_have_no_overloads(map[string]FastcFunctionSignature{},
			flat.TypeTextBindings{}, bindings, map[string]flat.TypeTextBindings{})
		assert !fastc_primitive_comparisons_have_no_overloads(map[string]FastcFunctionSignature{},
			flat.TypeTextBindings{}, flat.TypeTextBindings{}, {
				name: flat.TypeTextBindings{}
			})
	}
	base := fastc_primitive_comparison_parser(false)
	functions := {
		'int.==': FastcFunctionSignature{
			module_name:     'builtin'
			parameter_types: flat.TypeTextList{ ids: [flat.source_type_int, flat.source_type_int] }
			return_type:     flat.source_type_bool
		}
	}
	custom := Parser{
		...base
		functions:              functions
		primitive_cmps_builtin: fastc_primitive_comparisons_have_no_overloads(functions,
			flat.TypeTextBindings{}, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
	}
	custom_tokens := fastc_primitive_comparison_tokens('a == b')
	assert !custom.plain_primitive_comparison_expression(custom_tokens)
	custom_comparison := custom.render_overloaded_binary_expression(custom_tokens) or { panic('missing overload') }
	assert custom_comparison.source != '' && custom_comparison.typ == flat.source_type_bool
	mut g := fastc_primitive_comparison_parser(true)
	tokens := fastc_primitive_comparison_tokens('a == b')
	g.member_smartcasts['unrelated'] = FastcMemberSmartcast{}
	assert !g.plain_primitive_comparison_expression(tokens)
	g.member_smartcasts.clear()
	generic := Parser{
		...g
		generic_method_names: {
			'a': true
		}
	}
	assert !generic.plain_primitive_comparison_expression(tokens)
	translated := Parser{ ...g, translated: true }
	assert !translated.plain_primitive_comparison_expression(tokens)
	g.in_mono_drain = true
	assert !g.plain_primitive_comparison_expression(tokens)
	g.in_mono_drain = false
	g.in_generic_placeholder = true
	assert !g.plain_primitive_comparison_expression(tokens)
	g.in_generic_placeholder = false
	ordinary := Parser{ ...g, selfhost: false }
	assert !ordinary.plain_primitive_comparison_expression(tokens)
}

fn test_fastc_plain_primitive_comparisons_runtime_keep_unsigned_and_boolean_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_primitive_comparisons_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

fn signed_less(a int, b int) bool { return a < b }
fn unsigned_less(a u64, b u64) bool { return a < b }
fn same_bool(a bool, b bool) bool { return a == b }
fn floating_less(a f64, b f64) bool { return a < b }
fn mixed_greater(a u64, b int) bool { return a > b }
fn main() {
	assert signed_less(-2, 3)
	assert unsigned_less(3, u64(0xffffffffffffffff))
	assert same_bool(true, true)
	assert !same_bool(true, false)
	assert floating_less(1.25, 1.5)
	assert mixed_greater(u64(0xffffffffffffffff), -1)
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
