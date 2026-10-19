module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_atomic_aggregate_parser(source string) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('atomic_aggregate.v', source.len), source)
	first := scan.scan()
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Alias', 'Record')
	return Parser{
		prefs:                    prefs
		selfhost:                 true
		s:                        scan
		tok:                      first
		lit:                      scan.lit
		unsafe_depth:             2
		expected_expression_type: flat.source_type_id('Iface')
		last_expression_type:     flat.source_type_void
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		last_option_value_type:   flat.source_type_string
		declared_kinds:           {
			'Record':           .struct_
			'Alias':            .alias_
			'Iface':            .interface_
			'Choice':           .alias_
			'Option':           .struct_
			'__v_result':       .struct_
			'FixedArray_2_int': .struct_
		}
		alias_base_types:         aliases
		sum_types:                {
			'Choice': true
		}
		generic_method_names:     {
			'generic': true
		}
		locals:                   {
			'value':     FastcLocal{ typ: flat.source_type_id('Record') }
			'renamed':   FastcLocal{ typ: flat.source_type_id('Record'), c_name: '__narrowed_copy' }
			'mutable':   FastcLocal{ typ: flat.source_type_id('Record'), is_mut: true }
			'items':     FastcLocal{ typ: flat.source_type_id('Array_Record') }
			'values':    FastcLocal{ typ: flat.source_type_id('Map_string_Record') }
			'reference': FastcLocal{ typ: flat.source_type_id('Record*'), is_reference: true }
			'pointer':   FastcLocal{ typ: flat.source_type_id('Record*') }
			'alias':     FastcLocal{ typ: flat.source_type_id('Alias') }
			'boxed':     FastcLocal{ typ: flat.source_type_id('Choice') }
			'iface':     FastcLocal{ typ: flat.source_type_id('Iface') }
			'optional':  FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_id('Record') }
			'wrapper':   FastcLocal{ typ: fastc_type_option }
			'result':    FastcLocal{ typ: flat.source_type_id('__v_result') }
			'fixed':     FastcLocal{ typ: flat.source_type_id('FixedArray_2_int') }
			'origin':    FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_type: flat.source_type_id('Choice') }
			'generic':   FastcLocal{ typ: flat.source_type_id('Record') }
			'C':         FastcLocal{ typ: flat.source_type_id('Record') }
		}
	}
}

fn test_fastc_atomic_aggregate_values_match_streaming_output_and_state() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value;', 'value,', 'value)', 'value]', 'value}', 'value:', 'value {', 'value\n',
		'renamed;', 'mutable;', 'items;', 'values;', 'value /* comment */;', 'value\n*pointer = 2;',
		'value\n/* comment */ .count;'] {
		for expected in [u32(0), flat.source_type_id('Record'), flat.source_type_id('Iface'),
			flat.source_type_id('Choice'), fastc_type_option, flat.source_type_id('__v_result'),
			flat.source_type_voidptr] {
			mut fast := fastc_atomic_aggregate_parser(source)
			mut slow := fastc_atomic_aggregate_parser(source)
			fast.expected_expression_type = expected
			slow.expected_expression_type = expected
			atom := fast.try_read_atomic_expression(stops, false, true)
			assert atom.found, source
			rendered := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
			assert atom.source == rendered, source
			assert fast.last_expression == slow.last_expression, source
			assert fast.last_expression_type == slow.last_expression_type, source
			assert fast.expected_expression_type == slow.expected_expression_type, source
			assert fast.last_multi_return_types == slow.last_multi_return_types, source
			assert fast.last_option_value_type == slow.last_option_value_type, source
			assert fast.tok == slow.tok && fast.lit == slow.lit, source
			assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
			assert fast.s.lit == slow.s.lit && fast.s.diagnostics == slow.s.diagnostics, source
			assert fast.unsafe_depth == slow.unsafe_depth && fast.temp_id == slow.temp_id, source
			assert fast.composite_types == slow.composite_types, source
			assert fast.fixed_array_types.ids == slow.fixed_array_types.ids, source
		}
	}
}

fn test_fastc_atomic_aggregate_rejections_leave_the_scanner_and_metadata_unchanged() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value + other;', 'value\n + other;', 'value\n .count;', 'value\n// comment\n + other;',
		'reference;', 'pointer;', 'alias;', 'boxed;', 'iface;', 'optional;', 'wrapper;', 'result;',
		'fixed;', 'origin;', 'generic;', 'C;', 'value();'] {
		mut g := fastc_atomic_aggregate_parser(source)
		before := g.s
		before_tok := g.tok
		before_lit := g.lit
		previous_expression := g.last_expression.clone()
		assert !g.try_read_atomic_expression(stops, false, true).found, source
		assert g.s.offset == before.offset && g.s.pos == before.pos, source
		assert g.s.lit == before.lit && g.s.diagnostics == before.diagnostics, source
		assert g.tok == before_tok && g.lit == before_lit, source
		assert g.last_expression == previous_expression, source
		assert g.last_expression_type == flat.source_type_void, source
		assert g.last_option_value_type == flat.source_type_string, source
		assert g.expected_expression_type == flat.source_type_id('Iface'), source
	}
	mut narrowed := fastc_atomic_aggregate_parser('value;')
	narrowed.member_smartcasts['value'] = FastcMemberSmartcast{ typ: flat.source_type_id('Record'), source: '__narrowed' }
	assert !narrowed.try_read_atomic_expression(stops, false, true).found
	mut stopped := fastc_atomic_aggregate_parser('value;')
	assert !stopped.try_read_atomic_expression([token.Token.name], false, true).found
}

fn test_fastc_atomic_aggregate_returns_declarations_and_destination_coercions() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_atomic_aggregates_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main

struct Record { count int }
interface Scored { score() int }
type Choice = Record | int
fn (value Record) score() int { return value.count }
fn return_record(value Record) Record { return value }
fn return_array(values []int) []int { return values }
fn return_map(values map[string]int) map[string]int { return values }
fn return_interface(value Record) Scored { return value }
fn return_sum(value Record) Choice { return value }
fn return_option(value Record) ?Record { return value }
fn return_result(value Record) !Record { return value }
fn main() {
	original := Record{count: 12}
	declared := original
	returned := return_record(declared)
	assert returned.count == 12
	items := [3, 4]
	declared_items := items
	returned_items := return_array(declared_items)
	assert returned_items[0] == 3 && returned_items[1] == 4
	values := {"entry": 7}
	declared_values := values
	returned_values := return_map(declared_values)
	assert returned_values["entry"] == 7
	boxed := return_interface(original)
	assert boxed.score() == 12
	choice := return_sum(original)
	if choice is Record {
		assert choice.count == 12
	} else {
		assert false
	}
	optional := return_option(original) or { panic("missing option") }
	assert optional.count == 12
	result := return_result(original) or { panic(err) }
	assert result.count == 12
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
