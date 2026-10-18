module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_simple_call_test_parser(source string, selfhost bool, translated bool) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('simple_call.v', source.len), source)
	first := scan.scan()
	return Parser{
		prefs:                    prefs
		selfhost:                 selfhost
		translated:               translated
		module_name:              'fixture'
		s:                        scan
		tok:                      first
		lit:                      scan.lit
		unsafe_depth:             2
		expected_expression_type: flat.source_type_id('Iface')
		last_expression_type:     flat.source_type_void
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		last_option_value_type:   flat.source_type_string
		functions:                {
			'fixture.value':        FastcFunctionSignature{
				return_type: flat.source_type_int
				module_name: 'fixture'
			}
			'fixture.scalar':       FastcFunctionSignature{
				parameter_types: flat.TypeTextList.from_names(['int', 'string', 'bool', 'f64'])
				return_type:     flat.source_type_string
				module_name:     'fixture'
			}
			'fixture.integer':      FastcFunctionSignature{
				parameter_types: flat.TypeTextList.from_names(['int'])
				return_type:     flat.source_type_int
				module_name:     'fixture'
			}
			'fixture.noreturn':     FastcFunctionSignature{
				parameter_types: flat.TypeTextList.from_names(['bool'])
				return_type:     flat.source_type_void
				module_name:     'fixture'
			}
			'builtin.builtin_only': FastcFunctionSignature{
				parameter_types: flat.TypeTextList.from_names(['int'])
				return_type:     flat.source_type_int
				module_name:     'builtin'
			}
			'builtin.value':        FastcFunctionSignature{
				parameter_types: flat.TypeTextList.from_names(['int'])
				return_type:     flat.source_type_bool
				module_name:     'builtin'
			}
		}
		function_c_names:         {
			'fixture.scalar': 'F__scalar'
		}
		generic_method_names:     {
			'generic': true
		}
		locals:                   {
			'count':     FastcLocal{ typ: flat.source_type_int }
			'text':      FastcLocal{ typ: flat.source_type_string, c_name: '__renamed' }
			'flag':      FastcLocal{ typ: flat.source_type_bool }
			'fraction':  FastcLocal{ typ: flat.source_type_f64 }
			'reference': FastcLocal{ typ: flat.source_type_id('int*'), is_reference: true }
			'optional':  FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_int }
			'narrowed':  FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Choice') }
			'mutable':   FastcLocal{ typ: flat.source_type_int, is_mut: true }
			'wide':      FastcLocal{ typ: flat.source_type_u64 }
			'generic':   FastcLocal{ typ: flat.source_type_int }
			'chan':      FastcLocal{ typ: flat.source_type_int }
		}
	}
}

fn fastc_simple_call_assert_state_equal(fast &Parser, slow &Parser, source string) {
	assert fast.last_expression == slow.last_expression, source
	assert fast.last_expression_type == slow.last_expression_type, source
	assert fast.last_multi_return_types == slow.last_multi_return_types, source
	assert fast.last_option_value_type == slow.last_option_value_type, source
	assert fast.expected_expression_type == slow.expected_expression_type, source
	assert fast.unsafe_depth == slow.unsafe_depth && fast.expression_depth == slow.expression_depth, source
	assert fast.tok == slow.tok && fast.lit == slow.lit, source
	assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
	assert fast.s.lit == slow.s.lit && fast.s.diagnostics == slow.s.diagnostics, source
	assert fast.s.in_str_incomplete == slow.s.in_str_incomplete, source
	assert fast.s.in_str_inter == slow.s.in_str_inter, source
	assert fast.s.in_str_inter_format == slow.s.in_str_inter_format, source
	assert fast.s.str_parent_quotes == slow.s.str_parent_quotes, source
	assert fast.s.str_parent_depths == slow.s.str_parent_depths, source
	assert fast.s.line_directive_offsets == slow.s.line_directive_offsets, source
	// Subsequent tokens also check scanner-private insert_semi/after_dot state.
	mut fast_scan := fast.s
	mut slow_scan := slow.s
	for _ in 0 .. 3 {
		assert fast_scan.scan() == slow_scan.scan(), source
		assert fast_scan.lit == slow_scan.lit, source
		assert fast_scan.offset == slow_scan.offset && fast_scan.pos == slow_scan.pos, source
	}
}

fn test_fastc_simple_calls_match_the_complete_streaming_reader() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value();', 'value()', 'value(),', 'value())', 'value()]', 'value()}', 'value():',
		'value() {', 'value()\n', 'builtin_only(count);', 'integer(00017);', 'integer(0xffff);',
		'scalar(count,text,flag,fraction);', 'scalar(1,"text",true,1.25);',
		'scalar(1 /* comment */, text, false, 3.5);', 'noreturn(flag);', 'value() /* comment */;',
		'value()\n*ptr = 2;', 'value()\n/* comment */ .field;'] {
		for mutation in [true, false] {
			for newline_values in [true, false] {
				mut fast := fastc_simple_call_test_parser(source, true, false)
				mut slow := fastc_simple_call_test_parser(source, true, false)
				call := fast.try_read_simple_call_expression(stops, mutation, newline_values)
				assert call.found, source
				rendered := slow.read_expression_with_prefix_mode_impl('', stops, mutation, false, newline_values) or { panic(err) }
				assert call.source == rendered, '${source}: ${call.source} != ${rendered}'
				fastc_simple_call_assert_state_equal(&fast, &slow, source)
			}
		}
	}
}

fn test_fastc_simple_call_rejections_restore_the_reader_checkpoint() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['absent();', 'count();', 'generic();', 'int(1);', 'value(1);', 'integer();',
		'integer(1,2);', 'integer(mut count);', 'integer(reference);', 'integer(optional);',
		'integer(narrowed);', 'integer(mutable);', 'integer(wide);', 'integer(generic);',
		'integer(chan);', 'integer(count + 1);', 'integer((count));', 'integer(.enum_value);',
		'integer(value());', 'integer(-1);', 'integer(nil);', 'integer(1)!;', 'integer(1)?;',
		'integer(1).field;', 'integer(1)[0];', 'integer(1) + 2;', 'value()\n + 2;', 'value()\n .field;',
		'value()\n && flag;', 'integer(\ncount);', 'integer(count\n);', 'integer(/*\n*/count);',
		r'scalar(1,"${count}",true,1.0);', r'scalar(1,"a\n",true,1.0);', r'scalar(1,"a\u0000",true,1.0);',
		'integer(12wrong);', 'print(count);', 'println(count);', 'typeof(count);', 'C.integer(1);'] {
		mut g := fastc_simple_call_test_parser(source, true, false)
		before := fastc_simple_call_test_parser(source, true, false)
		assert !g.try_read_simple_call_expression(stops, false, true).found, source
		fastc_simple_call_assert_state_equal(&g, &before, source)
		assert g.unqualified_key_memo.len == 0 && g.declared_type_key_memo.len == 0, source
	}
}

fn test_fastc_simple_call_matches_streaming_in_generic_modes_and_unrelated_smartcasts() {
	stops := [token.Token.semicolon]
	for mono in [false, true] {
		for placeholder in [false, true] {
			for unrelated_smartcast in [false, true] {
				mut fast := fastc_simple_call_test_parser('scalar(count,text,flag,fraction);', true, false)
				mut slow := fastc_simple_call_test_parser('scalar(count,text,flag,fraction);', true, false)
				fast.in_mono_drain = mono
				slow.in_mono_drain = mono
				fast.in_generic_placeholder = placeholder
				slow.in_generic_placeholder = placeholder
				if unrelated_smartcast {
					fast.member_smartcasts['other.field'] = FastcMemberSmartcast{}
					slow.member_smartcasts['other.field'] = FastcMemberSmartcast{}
				}
				call := fast.try_read_simple_call_expression(stops, false, true)
				assert call.found
				rendered := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
				assert call.source == rendered
				fastc_simple_call_assert_state_equal(&fast, &slow, 'generic mode')
				assert fast.member_smartcasts == slow.member_smartcasts
			}
		}
	}
}

fn test_fastc_simple_call_signature_and_parser_guards() {
	stops := [token.Token.semicolon]
	for selfhost in [true, false] {
		for translated in [true, false] {
			mut g := fastc_simple_call_test_parser('value();', selfhost, translated)
			assert g.try_read_simple_call_expression(stops, false, true).found == (selfhost && !translated)
		}
	}
	for signature in [
		FastcFunctionSignature{ return_type: flat.source_type_int, is_variadic: true },
		FastcFunctionSignature{ return_type: flat.source_type_int, last_parameter_is_params: true },
		FastcFunctionSignature{ return_type: flat.source_type_int, is_disabled: true },
		FastcFunctionSignature{ return_type: flat.source_type_int, is_c_extern: true },
		FastcFunctionSignature{ return_type: flat.source_type_int, is_global: true },
		FastcFunctionSignature{ return_type: flat.source_type_int, callable_index_depth: 1 },
		FastcFunctionSignature{ return_type: fastc_type_option, option_type: flat.source_type_int },
		FastcFunctionSignature{
			return_type:  flat.source_type_id('MultiReturn')
			return_types: flat.TypeTextList.from_names([
				'int',
				'bool',
			])
		},
		FastcFunctionSignature{ return_type: flat.source_type_int, returned_fn_return_type: flat.source_type_int },
		FastcFunctionSignature{ return_type: flat.source_type_id('Record') },
		FastcFunctionSignature{ return_type: flat.source_type_id('FixedArray_2_int') },
		FastcFunctionSignature{ return_type: flat.source_type_voidptr },
	] {
		mut g := fastc_simple_call_test_parser('value();', true, false)
		g.functions['fixture.value'] = signature
		assert !g.try_read_simple_call_expression(stops, false, true).found
	}
	mut mutable_parameter := fastc_simple_call_test_parser('integer(count);', true, false)
	mutable_parameter.functions['fixture.integer'] = FastcFunctionSignature{
		parameter_types:      flat.TypeTextList.from_names(['int'])
		parameter_mutability: [true]
		return_type:          flat.source_type_int
	}
	assert !mutable_parameter.try_read_simple_call_expression(stops, false, true).found
	mut generic_parameter := fastc_simple_call_test_parser('integer(count);', true, false)
	generic_parameter.functions['fixture.integer'] = FastcFunctionSignature{
		parameter_types:          flat.TypeTextList.from_names(['int'])
		parameter_generic_values: [true]
		return_type:              flat.source_type_int
	}
	assert !generic_parameter.try_read_simple_call_expression(stops, false, true).found
	mut narrowed := fastc_simple_call_test_parser('integer(count);', true, false)
	narrowed.member_smartcasts['count'] = FastcMemberSmartcast{}
	assert !narrowed.try_read_simple_call_expression(stops, false, true).found
	type_base := fastc_simple_call_test_parser('value();', true, false)
	mut type_call := Parser{
		...type_base
		declared_types: {
			'fixture.value': true
		}
	}
	assert !type_call.try_read_simple_call_expression(stops, false, true).found
	mut line_directives := fastc_simple_call_test_parser('value();', true, false)
	line_directives.s.line_directives = true
	assert !line_directives.try_read_simple_call_expression(stops, false, true).found
	mut mono_shadow := fastc_simple_call_test_parser('builtin_only(count);', true, false)
	mono_shadow.mono_functions['fixture.builtin_only'] = FastcFunctionSignature{}
	assert !mono_shadow.try_read_simple_call_expression(stops, false, true).found
	mut stop_name := fastc_simple_call_test_parser('value();', true, false)
	assert !stop_name.try_read_simple_call_expression([token.Token.name], false, true).found
}
