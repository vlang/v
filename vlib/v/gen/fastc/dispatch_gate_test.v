module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn dispatch_gate_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('dispatch_gate.v', source.len), source)
	mut result := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		result << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return result
}

fn test_expression_dispatch_gate_rejected_shapes_leave_renderers_empty() {
	prefs := pref.new_preferences()
	g := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal), selfhost: true }
	mut rejected := [0, 0, 0, 0, 0, 0]
	for source in ['a', 'a + b', 'a + b * c', '!a', 'a && b', 'a || b', '(a + b)', 'a.field',
		'a.field + b.other', 'a.field == b.other', 'a == b', 'a + b < c + d', 'a == b && c == d',
		'(a < b) || (c > d)', 'f()', 'f(a)', 'f(a, b + c)', 'a.method()', 'a.method(b)',
		'a.method(b + c)', 'a.method(b) == c', 'a.method(b) > c.field', 'a[0]', 'a[b + c]',
		'a[0] == b[1]', '[a, b]', 'Record{value: a}', 'f()!', 'f()?', '(f()!)', 'f()!.value',
		'(f()?).value', 'a is Record', 'a !is Record', '(a is Record) && b', 'f() is Record',
		'f(a).field', 'f()[0]', 'a[0](b)', 'f().value == 3', 'f()? == 3', '3 < f()!', 'a.method(a[0])',
		'(a[0]).method(b)', '[1, 2]!', 'a.field == 1 && !b'] {
		tokens := dispatch_gate_tokens(source)
		flags := fastc_special_token_flags(tokens)
		if !(flags.has_lpar && (flags.has_dot || flags.has_lsbr)) {
			assert g.render_call_result_member_expression(tokens) == none, source
			rejected[0]++
		}
		if flags.has_lpar && !flags.has_lsbr && tokens.last().tok == .rpar {
			assert g.render_element_function_call(tokens) == none, source
			rejected[1]++
		}
		if flags.has_comparison && !(flags.has_not || flags.has_question || (flags.has_lpar && flags.has_dot)) {
			assert g.render_propagation_in_comparison(tokens) == none, source
			rejected[2]++
		}
		if !flags.has_type_test {
			assert g.render_boolean_is_expression(tokens) == none, source
			rejected[3]++
		}
		if !(flags.has_dot && (flags.has_not || flags.has_question)) {
			assert g.render_propagation_before_member(tokens) == none, source
			rejected[4]++
		}
		if !flags.has_lsbr {
			assert g.render_nested_array_access_expression(tokens, source) == none, source
			rejected[5]++
		}
		assert g.temp_id == 0, source
		assert g.composite_types.len == 0, source
		assert g.mono_functions.len == 0, source
		assert g.locals.len == 0, source
		assert g.functions.len == 0, source
	}
	for count in rejected {
		assert count > 0
	}
}

fn test_expression_dispatch_gate_synthetic_typed_receiver_without_call_tokens_is_rendered() {
	prefs := pref.new_preferences()
	fields := {
		'Payload': [FastcStructField{ name: 'value', typ: flat.source_type_int }]
	}
	mut field_types := flat.TypeTextBindings{}
	field_types.set('value', 'int')
	g := Parser{
		prefs:               prefs
		s:                   scanner.new_scanner(prefs, .normal)
		selfhost:            true
		struct_fields:       {
			'Payload': field_types
		}
		struct_field_info:   fields
		struct_field_lookup: fastc_build_struct_field_lookup(fields)
		declared_kinds:      {
			'Payload': .struct_
		}
	}
	tokens := [
		FastcExpressionToken{ tok: .name, source: fastc_token_source('typed_payload'), typ: flat.source_type_id('Payload') },
		FastcExpressionToken{ tok: .dot },
		FastcExpressionToken{ tok: .name, lit: 'value' },
	]
	raw := g.render_raw_expression_tokens(tokens) or { panic('missing raw synthetic source') }
	result := g.render_special_expression(tokens, raw) or { panic('missing synthetic member') }
	assert result.typ == flat.source_type_int
	assert result.source == '(typed_payload).value' || result.source == 'typed_payload.value', result.source
}

fn test_expression_dispatch_gate_call_index_comparison_and_propagation_execute_once() {
	root := os.join_path(os.vtmp_dir(), 'fastc_dispatch_gate_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Payload { value int }
struct Other { value int }
type Choice = Payload | Other
struct Worker {}
fn make(mut calls int, value int) Payload { calls++; return Payload{value: value} }
fn optional(mut calls int, ok bool) ?Payload {
 calls++
 if !ok { return none }
 return Payload{value: 7}
}
fn choice(value int) Choice { return Payload{value: value} }
fn twice(value int) int { return value * 2 }
fn (worker Worker) echo(value int) int { return value }
fn through_option(mut calls int, ok bool) ?int {
 return optional(mut calls, ok)?.value
}
fn through_group(mut calls int, ok bool) ?int {
 return (optional(mut calls, ok)?).value
}
fn main() {
 mut calls := 0
 assert make(mut calls, 7).value == 7
 assert calls == 1
 assert make(mut calls, 8).value > 7
 assert calls == 2
 values := [1, 2, 3]
 worker := Worker{}
 assert worker.echo(7) == 7
 assert worker.echo(values[1]) == 2
 handlers := [twice]
 assert handlers[0](21) == 42
 assert choice(4) is Payload
 assert (choice(4) is Payload) && !(choice(3) is Other)
 assert through_option(mut calls, true) or { 0 } == 7
 assert calls == 3
 assert through_option(mut calls, false) or { 9 } == 9
 assert calls == 4
 assert through_group(mut calls, true) or { 0 } == 7
 assert calls == 5
 assert through_group(mut calls, false) or { 9 } == 9
 assert calls == 6
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
