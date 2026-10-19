module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_grouped_propagation_preserves_payload_type() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:       prefs
		s:           scanner.new_scanner(prefs, .normal)
		selfhost:    true
		return_type: fastc_type_option
		functions:   {
			'value': FastcFunctionSignature{
				return_type: fastc_type_option
				option_type: flat.source_type_u32
			}
		}
	}
	for marker in [token.Token.question, .not] {
		tokens := [FastcExpressionToken{ tok: .lpar }, FastcExpressionToken{ tok: .lpar },
			FastcExpressionToken{ tok: .name, lit: 'value' }, FastcExpressionToken{ tok: .lpar },
			FastcExpressionToken{ tok: .rpar }, FastcExpressionToken{ tok: marker },
			FastcExpressionToken{ tok: .rpar }, FastcExpressionToken{ tok: .rpar }]
		raw := g.render_raw_expression_tokens(tokens) or { panic('missing raw grouping') }
		value := g.render_special_expression(tokens, raw) or { panic('missing grouped propagation') }
		assert value.typ == flat.source_type_u32
		assert value.source.contains('if (__vf_op.state) { return __vf_op; }')
		assert value.source.contains('*((u32 *)')
	}
	// A fixed-array suffix shares the `!` token but does not propagate an Option.
	fixed_tokens := [FastcExpressionToken{ tok: .lpar }, FastcExpressionToken{ tok: .lsbr },
		FastcExpressionToken{ tok: .number, lit: '4' }, FastcExpressionToken{ tok: .rsbr },
		FastcExpressionToken{ tok: .not }, FastcExpressionToken{ tok: .rpar }]
	assert g.render_special_expression(fixed_tokens, '([4]!)') == none
}

fn test_fastc_grouped_option_and_result_propagation_preserves_evaluation() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_grouped_propagation_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Payload {
	prefix string
	value u32
	suffix string
}

fn optional(mut calls int, ok bool) ?u32 {
	calls++
	if !ok { return none }
	return u32(40)
}

fn result(mut calls int, ok bool) !u32 {
	calls++
	if !ok { return error("missing") }
	return u32(40)
}

fn grouped_option(mut calls int, ok bool) ?u32 {
	value := (((optional(mut calls, ok)?)))
	calls += 100
	return value + 2
}

fn grouped_result(mut calls int, ok bool) !u32 {
	value := (((result(mut calls, ok)!)))
	calls += 100
	return value + 2
}

fn option_field(mut calls int, ok bool) ?Payload {
	return Payload{
		prefix: "before"
		value: (((optional(mut calls, ok) or { return none })))
		suffix: "after"
	}
}

fn result_field(mut calls int, ok bool) !Payload {
	return Payload{
		prefix: "before"
		value: (((result(mut calls, ok) or { return err })))
		suffix: "after"
	}
}

fn add(mut calls int, value u32) u32 {
	calls += 10
	return value + 2
}

fn option_argument(mut calls int, ok bool) ?u32 {
	return add(mut calls, ((optional(mut calls, ok)?)))
}

fn result_argument(mut calls int, ok bool) !u32 {
	return add(mut calls, ((result(mut calls, ok)!)))
}

fn text(ok bool) ?string {
	if !ok { return none }
	return "a"
}

fn grouped_concatenation(ok bool) ?string {
	return ((text(ok)? + text(true)?))
}

fn main() {
	mut calls := 0
	assert (grouped_option(mut calls, true) or { 0 }) == u32(42)
	assert calls == 101
	calls = 0
	assert (grouped_option(mut calls, false) or { 9 }) == u32(9)
	assert calls == 1
	calls = 0
	assert (grouped_result(mut calls, true) or { 0 }) == u32(42)
	assert calls == 101
	calls = 0
	assert (grouped_result(mut calls, false) or { 9 }) == u32(9)
	assert calls == 1
	calls = 0
	assert (option_argument(mut calls, true) or { 0 }) == u32(42)
	assert calls == 11
	calls = 0
	assert (option_argument(mut calls, false) or { 9 }) == u32(9)
	assert calls == 1
	calls = 0
	assert (result_argument(mut calls, true) or { 0 }) == u32(42)
	assert calls == 11
	calls = 0
	assert (result_argument(mut calls, false) or { 9 }) == u32(9)
	assert calls == 1
	calls = 0
	option_payload := option_field(mut calls, true) or { panic("missing option payload") }
	assert option_payload.prefix == "before"
	assert option_payload.value == u32(40)
	assert option_payload.suffix == "after"
	assert calls == 1
	calls = 0
	missing_option := option_field(mut calls, false) or { Payload{value: 9} }
	assert missing_option.value == u32(9)
	assert calls == 1
	calls = 0
	result_payload := result_field(mut calls, true) or { panic(err) }
	assert result_payload.prefix == "before"
	assert result_payload.value == u32(40)
	assert result_payload.suffix == "after"
	assert calls == 1
	calls = 0
	missing_result := result_field(mut calls, false) or { Payload{value: 9} }
	assert missing_result.value == u32(9)
	assert calls == 1
	assert (grouped_concatenation(true) or { "missing" }) == "aa"
	assert (grouped_concatenation(false) or { "missing" }) == "missing"
	fixed := [4, 5, 6]!
	assert fixed.len == 3
	assert fixed[1] == 5
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
