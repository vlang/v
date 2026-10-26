module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_member_comparison_parser(source string, allow bool) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('primitive_member_comparison.v', source.len), source)
	first := scan.scan()
	mut record := flat.TypeTextBindings{}
	mut fields := []FastcStructField{}
	for name, type_name in {
		'offset':     'int'
		'short':      'i8'
		'wide':       'u64'
		'ratio':      'f64'
		'flag':       'bool'
		'character':  'char'
		'rune_value': 'rune'
		'pointer':    'int*'
		'text':       'string'
	} {
		record.set(name, type_name)
		fields << FastcStructField{ name: name, typ: flat.source_type_id(type_name) }
	}
	record.set('shared', 'int')
	fields << FastcStructField{ name: 'shared', typ: flat.source_type_int, is_shared_pointer: true }
	mut array := flat.TypeTextBindings{}
	array.set('len', 'int')
	array.set('cap', 'int')
	info := {
		'Record': fields
		'array':  [FastcStructField{ name: 'len', typ: flat.source_type_int },
			FastcStructField{ name: 'cap', typ: flat.source_type_int }]
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
		primitive_cmps_builtin:   allow
		numeric_ops_are_builtin:  true
		unsafe_depth:             2
		expected_expression_type: flat.source_type_id('Iface')
		last_expression_type:     flat.source_type_void
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		last_option_value_type:   flat.source_type_string
		declared_types:           {
			'Mode':         true
			'PointerMode':  true
			'Shade':        true
			'PointerShade': true
		}
		declared_kinds:           {
			'Record':       .struct_
			'Alias':        .alias_
			'Iface':        .interface_
			'Mode':         .enum_
			'PointerMode':  .enum_
			'Shade':        .alias_
			'PointerShade': .alias_
		}
		alias_base_types:         aliases
		struct_fields:            {
			'Record': record
			'array':  array
		}
		struct_field_info:        info
		struct_field_lookup:      fastc_build_struct_field_lookup(info)
		locals:                   {
			'g':            FastcLocal{ typ: flat.source_type_id('Record*'), is_reference: true, is_mut: true }
			'value':        FastcLocal{ typ: flat.source_type_id('Record') }
			'pointer':      FastcLocal{ typ: flat.source_type_id('Record*'), c_name: '__record' }
			'Mode':         FastcLocal{ typ: flat.source_type_id('Record') }
			'PointerMode':  FastcLocal{ typ: flat.source_type_id('Record*') }
			'Shade':        FastcLocal{ typ: flat.source_type_id('Record'), c_name: 'Shade__offset' }
			'PointerShade': FastcLocal{ typ: flat.source_type_id('Record*'), c_name: 'PointerShade__offset' }
			'items':        FastcLocal{ typ: flat.source_type_id('Array_int') }
			'item_ref':     FastcLocal{ typ: flat.source_type_id('Array_int*'), is_reference: true }
			'alias':        FastcLocal{ typ: flat.source_type_id('Alias') }
			'double':       FastcLocal{ typ: flat.source_type_id('Record**') }
			'bad_ref':      FastcLocal{ typ: flat.source_type_id('Record'), is_reference: true }
			'end':          FastcLocal{ typ: flat.source_type_int }
			'wide':         FastcLocal{ typ: flat.source_type_u64 }
			'short':        FastcLocal{ typ: flat.source_type_i8 }
			'ratio':        FastcLocal{ typ: flat.source_type_f64 }
			'flag':         FastcLocal{ typ: flat.source_type_bool }
			'reference':    FastcLocal{ typ: flat.source_type_id('int*'), is_reference: true }
		}
	}
}

fn fastc_member_comparison_tokens(source string) []FastcExpressionToken {
	g := fastc_member_comparison_parser(source, true)
	mut scan := g.s
	mut kind := g.tok
	mut lit := g.lit
	mut result := []FastcExpressionToken{}
	for kind !in [.eof, .semicolon] {
		result << FastcExpressionToken{ tok: kind, lit: lit, unsafe_depth: g.unsafe_depth }
		kind = scan.scan()
		lit = scan.lit
	}
	return result
}

fn test_fastc_primitive_member_comparisons_match_full_streaming_finish() {
	for source in ['g.offset < end', 'end <= g.offset', 'g.offset == value.offset', 'value.offset != 0',
		'pointer.offset >= 17', 'items.len == 0', 'item_ref.cap > end', 'g.wide > 0xffffffffffffffff',
		'g.wide == wide', 'g.short < short', 'g.ratio >= 1.25', 'g.ratio != ratio', 'g.flag == true',
		'false != g.flag', 'g.flag == value.flag'] {
		for ending in [';', ' {', ',', ')', ']', '}', '\n', ''] {
			text := source + ending
			mut fast := fastc_member_comparison_parser(text, true)
			mut slow := fastc_member_comparison_parser(text, false)
			tokens := fastc_member_comparison_tokens(source)
			assert fast.plain_primitive_member_comparison_expression(tokens), source
			assert !slow.plain_primitive_member_comparison_expression(tokens), source
			stops := [token.Token.semicolon, .lcbr, .comma, .rpar, .rsbr, .rcbr]
			rendered := fast.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
			legacy := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
			assert rendered == legacy, '${source}: ${rendered} != ${legacy}'
			assert fast.last_expression == slow.last_expression, source
			assert fast.last_expression_type == slow.last_expression_type, source
			assert fast.last_expression_type == flat.source_type_bool, source
			assert fast.expected_expression_type == slow.expected_expression_type, source
			assert fast.last_option_value_type == slow.last_option_value_type, source
			assert fast.last_multi_return_types == slow.last_multi_return_types, source
			assert fast.tok == slow.tok && fast.lit == slow.lit, source
			assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
			assert fast.s.diagnostics == slow.s.diagnostics, source
			assert fast.temp_id == slow.temp_id && fast.temp_id == 0, source
			assert fast.unsafe_depth == slow.unsafe_depth, source
			assert fast.protos.str() == slow.protos.str(), source
			assert fast.spawn_helpers == slow.spawn_helpers, source
			assert fast.member_smartcasts == slow.member_smartcasts, source
			assert fast.composite_types == slow.composite_types, source
			assert fast.fixed_array_types.ids == slow.fixed_array_types.ids, source
		}
	}
}

fn test_fastc_primitive_member_comparison_guard_keeps_other_types_and_shapes() {
	g := fastc_member_comparison_parser('', true)
	for source in ['g.offset < wide', 'g.wide > end', 'g.short == end', 'g.ratio > end',
		'g.offset == g.ratio', 'g.flag < true', 'g.character == 0', 'g.rune_value == 0',
		'g.pointer == reference', 'g.text == "text"', 'alias.offset < end', 'double.offset < end',
		'bad_ref.offset < end', 'g.shared < end', 'g.absent == 0', 'g.offset() == 0',
		'g.offset < 1 + end', 'g.offset < (end)', 'g.next.offset < 0', 'g.offset == reference',
		'g.offset is int', 'g.offset in items', 'g.offset < end && flag', 'g.offset = 2'] {
		assert !g.plain_primitive_member_comparison_expression(fastc_member_comparison_tokens(source)), source
	}
	for index in 0 .. 5 {
		mut tokens := fastc_member_comparison_tokens('g.offset < end')
		tokens[index] = FastcExpressionToken{ ...tokens[index], source: fastc_token_source('synthetic') }
		assert !g.plain_primitive_member_comparison_expression(tokens)
	}
	mut tokens := fastc_member_comparison_tokens('g.offset < end')
	tokens[3] = FastcExpressionToken{ ...tokens[3], typ: flat.source_type_int }
	assert !g.plain_primitive_member_comparison_expression(tokens)
	mut narrowed := fastc_member_comparison_parser('', true)
	narrowed.member_smartcasts['other.field'] = FastcMemberSmartcast{}
	assert !narrowed.plain_primitive_member_comparison_expression(fastc_member_comparison_tokens('g.offset < end'))
	mut generic := fastc_member_comparison_parser('', true)
	generic.in_mono_drain = true
	assert !generic.plain_primitive_member_comparison_expression(fastc_member_comparison_tokens('g.offset < end'))
	base := fastc_member_comparison_parser('', true)
	translated := Parser{ ...base, translated: true }
	assert !translated.plain_primitive_member_comparison_expression(fastc_member_comparison_tokens('g.offset < end'))
}

fn test_fastc_primitive_member_comparisons_preserve_enum_name_shadow_inference() {
	for name in ['Mode', 'PointerMode', 'Shade', 'PointerShade'] {
		source := '${name}.offset < end;'
		mut fast := fastc_member_comparison_parser(source, true)
		mut slow := fastc_member_comparison_parser(source, false)
		tokens := fastc_member_comparison_tokens(source)
		assert fast.is_enum_type_name(name), name
		// The enum spelling and direct local field intentionally disagree.
		assert fast.struct_direct_member_type(fast.locals[name].typ, 'offset') == flat.source_type_int
		legacy_operand := fast.infer_expression_type(tokens[..3]) or { panic(name) }
		assert legacy_operand != flat.source_type_int, name
		assert !fast.plain_primitive_member_comparison_expression(tokens), name
		stops := [token.Token.semicolon]
		rendered := fast.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
		legacy := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
		assert rendered == legacy, name
		assert fast.last_expression == slow.last_expression, name
		assert fast.last_expression_type == slow.last_expression_type, name
		assert fast.expected_expression_type == slow.expected_expression_type, name
		assert fast.last_option_value_type == slow.last_option_value_type, name
		assert fast.last_multi_return_types == slow.last_multi_return_types, name
		assert fast.tok == slow.tok && fast.lit == slow.lit, name
		assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, name
		assert fast.temp_id == slow.temp_id && fast.spawn_helpers == slow.spawn_helpers, name
	}
}
