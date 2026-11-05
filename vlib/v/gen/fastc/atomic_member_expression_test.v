module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_atomic_member_parser(source string) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('atomic_member.v', source.len), source)
	first := scan.scan()
	mut record_types := flat.TypeTextBindings{}
	for name in ['count', 'short'] { record_types.set(name, 'int') }
	record_types.set('items', 'Array_int')
	record_types.set('next', 'Record*')
	mut array_types := flat.TypeTextBindings{}
	array_types.set('len', 'int')
	array_types.set('cap', 'int')
	mut string_types := flat.TypeTextBindings{}
	string_types.set('len', 'int')
	fields := {
		'Record': [
			FastcStructField{ name: 'count', typ: flat.source_type_int },
			FastcStructField{ name: 'short', typ: flat.source_type_int },
			FastcStructField{ name: 'items', typ: flat.source_type_id('Array_int') },
			FastcStructField{ name: 'next', typ: flat.source_type_id('Record*') },
			FastcStructField{ name: 'promoted', typ: flat.source_type_int, storage_path: ['__embedded_0'] },
			FastcStructField{ name: 'shared', typ: flat.source_type_int, is_shared_pointer: true },
			FastcStructField{ name: 'callback', typ: flat.source_type_voidptr, is_function: true },
			FastcStructField{ name: 'optional', typ: fastc_type_option, option_value_type: flat.source_type_int },
			FastcStructField{ name: 'generic', typ: flat.source_type_int, generic_argument_type: flat.source_type_int },
			FastcStructField{ name: 'channel', typ: flat.source_type_id('chan'), chan_element_type: flat.source_type_int },
			FastcStructField{ name: 'fixed', typ: flat.source_type_id('FixedArray_2_int') },
			FastcStructField{ name: 'alias', typ: flat.source_type_id('Alias') },
		]
		'array':  [FastcStructField{ name: 'len', typ: flat.source_type_int },
			FastcStructField{ name: 'cap', typ: flat.source_type_int }]
		'string': [FastcStructField{ name: 'len', typ: flat.source_type_int }]
	}
	mut aliases := flat.TypeTextBindings{}
	aliases.set('Alias', 'Record')
	aliases.set('Shade', 'Mode')
	aliases.set('PointerShade', 'Mode')
	return Parser{
		prefs:                    prefs
		s:                        scan
		tok:                      first
		lit:                      scan.lit
		selfhost:                 true
		module_name:              'fixture'
		unsafe_depth:             2
		expected_expression_type: flat.source_type_id('Iface')
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_expression_type:     flat.source_type_void
		last_option_value_type:   flat.source_type_string
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		declared_kinds:           {
			flat.source_type_id('Record'):       .struct_
			flat.source_type_id('string'):       .struct_
			flat.source_type_id('Alias'):        .alias_
			flat.source_type_id('Iface'):        .interface_
			flat.source_type_id('Mode'):         .enum_
			flat.source_type_id('Shade'):        .alias_
			flat.source_type_id('PointerShade'): .alias_
		}
		declared_types:           {
			flat.source_type_id('Mode'):         true
			flat.source_type_id('Shade'):        true
			flat.source_type_id('PointerShade'): true
		}
		alias_base_types:         aliases
		sum_types:                {
			'Choice': true
		}
		struct_fields:            {
			'Record': record_types
			'array':  array_types
			'string': string_types
		}
		struct_field_info:        fields
		struct_field_lookup:      fastc_build_struct_field_lookup(fields)
		generic_method_names:     {
			'generic_field': true
		}
		locals:                   {
			'value':        FastcLocal{ typ: flat.source_type_id('Record') }
			'Mode':         FastcLocal{ typ: flat.source_type_id('Record') }
			'Shade':        FastcLocal{ typ: flat.source_type_id('Record'), c_name: 'Shade__short' }
			'PointerShade': FastcLocal{ typ: flat.source_type_id('Record*'), c_name: 'PointerShade__short' }
			'count':        FastcLocal{ typ: flat.source_type_id('Record*') }
			'mutable':      FastcLocal{ typ: flat.source_type_id('Record'), is_mut: true }
			'pointer':      FastcLocal{ typ: flat.source_type_id('Record*') }
			'reference':    FastcLocal{ typ: flat.source_type_id('Record*'), is_reference: true, is_mut: true }
			'renamed':      FastcLocal{ typ: flat.source_type_id('Record*'), c_name: '__record_value' }
			'double':       FastcLocal{ typ: flat.source_type_id('Record**') }
			'items':        FastcLocal{ typ: flat.source_type_id('Array_int') }
			'item_ref':     FastcLocal{ typ: flat.source_type_id('Array_int*'), is_reference: true }
			'text':         FastcLocal{ typ: flat.source_type_string }
			'map':          FastcLocal{ typ: flat.source_type_id('Map_string_int') }
			'fixed':        FastcLocal{ typ: flat.source_type_id('FixedArray_2_int') }
			'alias':        FastcLocal{ typ: flat.source_type_id('Alias') }
			'boxed':        FastcLocal{ typ: flat.source_type_id('Choice') }
			'iface':        FastcLocal{ typ: flat.source_type_id('Iface') }
			'option':       FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_id('Record') }
			'origin':       FastcLocal{ typ: flat.source_type_id('Record'), smartcast_origin_type: flat.source_type_id('Choice') }
			'C':            FastcLocal{ typ: flat.source_type_id('Record') }
		}
	}
}

fn test_fastc_atomic_direct_members_match_the_complete_streamed_reader() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for value in ['value.count', 'mutable.count', 'pointer.count', 'reference.count', 'renamed.short',
		'value.items', 'pointer.next', 'items.len', 'items.cap', 'item_ref.len', 'text.len',
		'pointer . /* comment */ count'] {
		for ending in [';', ',', ')', ']', '}', ':', ' {', '\n', '', ' /* comment */;', '\n*pointer = 2;'] {
			for expected in [u32(0), flat.source_type_int, flat.source_type_id('Iface'),
				fastc_type_option, flat.source_type_voidptr] {
				source := value + ending
				mut fast := fastc_atomic_member_parser(source)
				mut slow := fastc_atomic_member_parser(source)
				fast.expected_expression_type = expected
				slow.expected_expression_type = expected
				atom := fast.try_read_atomic_expression(stops, false, true)
				assert atom.found, source
				rendered := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
				assert atom.source == rendered, source
				assert fast.last_expression == slow.last_expression, source
				assert fast.last_expression_type == slow.last_expression_type, source
				assert fast.expected_expression_type == slow.expected_expression_type, source
				assert fast.last_option_value_type == slow.last_option_value_type, source
				assert fast.last_multi_return_types == slow.last_multi_return_types, source
				assert fast.tok == slow.tok && fast.lit == slow.lit, source
				assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
				assert fast.s.lit == slow.s.lit && fast.s.diagnostics == slow.s.diagnostics, source
				assert fast.s.in_str_inter == slow.s.in_str_inter, source
				assert fast.s.in_str_incomplete == slow.s.in_str_incomplete, source
				assert fast.unsafe_depth == slow.unsafe_depth && fast.temp_id == slow.temp_id, source
				assert fast.composite_types == slow.composite_types, source
				assert fast.fixed_array_types.ids == slow.fixed_array_types.ids, source
				assert fast.spawn_helpers == slow.spawn_helpers, source
				assert fast.protos.str() == slow.protos.str(), source
				assert fast.out.str() == slow.out.str(), source
				mut fast_follow := fast.s
				mut slow_follow := slow.s
				for _ in 0 .. 3 {
					assert fast_follow.scan() == slow_follow.scan(), source
					assert fast_follow.lit == slow_follow.lit, source
					assert fast_follow.offset == slow_follow.offset && fast_follow.pos == slow_follow.pos, source
				}
			}
		}
	}
}

fn test_fastc_atomic_direct_members_preserve_enum_name_inference_through_fallback() {
	stops := [token.Token.semicolon]
	for source in ['Mode.short;', 'Shade.short;', 'PointerShade.short;'] {
		mut fast := fastc_atomic_member_parser(source)
		mut slow := fastc_atomic_member_parser(source)
		assert !fast.try_read_atomic_expression(stops, false, true).found, source
		result := fast.read_expression_with_continuation_mode('', stops, false, false, true) or { panic(err) }
		// Match the old top-level wrapper entry: this fixture has empty memos
		// and no inherited boolean casts, so only option reset and depth apply.
		slow.last_option_value_type = 0
		slow.expression_depth = 1
		legacy := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
		slow.expression_depth = 0
		assert fast.expression_depth == slow.expression_depth, source
		assert result == legacy, source
		assert fast.last_expression == slow.last_expression, source
		assert fast.last_expression_type == slow.last_expression_type, source
		assert fast.last_expression_type == flat.source_type_id('Mode'), source
		assert fast.last_option_value_type == slow.last_option_value_type, source
		assert fast.last_multi_return_types == slow.last_multi_return_types, source
		assert fast.tok == slow.tok && fast.lit == slow.lit, source
		assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
	}
}

fn test_fastc_atomic_direct_member_rejections_leave_parser_semantics_unchanged() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value.count + 1;', 'value.count\n + 1;', 'value.count\n .field;', 'value.count()',
		'value.items[0];', 'value.next.count;', 'value.count = 2;', 'value.count++;', 'value.count?',
		'value.\ncount;', 'value /* comment */ .count;', 'double.count;', 'map.len;', 'fixed.len;',
		'alias.count;', 'boxed.count;', 'iface.count;', 'option.count;', 'origin.count;', 'C.count;',
		'value.unknown;', 'value.promoted;', 'value.shared;', 'value.callback;', 'value.optional;',
		'value.generic;', 'value.channel;', 'value.fixed;', 'value.alias;', 'value.generic_field;',
		'items.data;', 'unknown.count;', 'Mode.short;', 'Shade.short;', 'PointerShade.short;'] {
		mut g := fastc_atomic_member_parser(source)
		previous := g.s
		previous_tokens := g.last_expression.clone()
		assert !g.try_read_atomic_expression(stops, false, true).found, source
		assert g.s.offset == previous.offset && g.s.pos == previous.pos, source
		assert g.s.lit == previous.lit && g.s.diagnostics == previous.diagnostics, source
		assert g.last_expression == previous_tokens, source
		assert g.last_expression_type == flat.source_type_void, source
		assert g.last_option_value_type == flat.source_type_string, source
		assert g.expected_expression_type == flat.source_type_id('Iface'), source
		assert g.temp_id == 0 && g.protos.len == 0 && g.spawn_helpers.len == 0, source
	}
	mut narrowed := fastc_atomic_member_parser('value.count;')
	narrowed.member_smartcasts['unrelated'] = FastcMemberSmartcast{ typ: flat.source_type_int, source: 'saved' }
	assert !narrowed.try_read_atomic_expression(stops, false, true).found
	mut mono := fastc_atomic_member_parser('value.count;')
	mono.in_mono_drain = true
	assert !mono.try_read_atomic_expression(stops, false, true).found
	mut generic := fastc_atomic_member_parser('value.count;')
	generic.in_generic_placeholder = true
	assert !generic.try_read_atomic_expression(stops, false, true).found
	base := fastc_atomic_member_parser('value.count;')
	mut ordinary := Parser{ ...base, selfhost: false }
	assert !ordinary.try_read_atomic_expression(stops, false, true).found
	mut translated := Parser{ ...base, translated: true }
	assert !translated.try_read_atomic_expression(stops, false, true).found
	mut stopped := fastc_atomic_member_parser('value.count;')
	assert !stopped.try_read_atomic_expression([token.Token.dot, .semicolon], false, true).found
}

fn test_fastc_atomic_direct_member_returns_and_destination_coercions_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_atomic_members_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Payload { count int }
struct Holder {
	item Payload
	items []int
}
interface Scored { score() int }
fn (p Payload) score() int { return p.count }
fn read_holder(h &Holder) Payload { return h.item }
fn read_items(h &Holder) []int { return h.items }
fn read_mut(mut h Holder) int { return h.item.count }
fn read_score(h &Holder) Scored { return h.item }
fn read_optional(h &Holder) ?Payload { return h.item }
fn array_len(values []int) int { return values.len }
fn string_len(value string) int { return value.len }
fn main() {
	mut holder := Holder{item: Payload{count: 12}, items: [3, 4]}
	payload := read_holder(&holder)
	assert payload.count == 12
	assert read_mut(mut holder) == 12
	returned_items := read_items(&holder)
	assert returned_items[1] == 4
	score := read_score(&holder)
	assert score.score() == 12
	optional := read_optional(&holder) or { panic("missing") }
	assert optional.count == 12
	assert array_len(holder.items) == 2
	assert string_len("plain") == 5
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
