module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_atomic_test_parser(source string) Parser {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('atomic_expression.v', source.len), source)
	first := scan.scan()
	return Parser{
		prefs:                    prefs
		selfhost:                 true
		s:                        scan
		tok:                      first
		lit:                      scan.lit
		unsafe_depth:             2
		expected_expression_type: flat.source_type_f64
		last_expression_type:     flat.source_type_void
		last_expression:          [FastcExpressionToken{ tok: .number, lit: '99' }]
		last_multi_return_types:  flat.TypeTextList.from_names(['int', 'string'])
		generic_method_names:     {
			'generic': true
		}
		locals:                   {
			'value':     FastcLocal{ typ: flat.source_type_int }
			'flag':      FastcLocal{ typ: flat.source_type_bool }
			'renamed':   FastcLocal{ typ: flat.source_type_string, c_name: '__renamed' }
			'reference': FastcLocal{ typ: flat.source_type_id('int*'), is_reference: true }
			'optional':  FastcLocal{ typ: flat.source_type_id('Option'), option_value_type: flat.source_type_int }
			'narrowed':  FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Scalar') }
			'object':    FastcLocal{ typ: flat.source_type_id('Record') }
			'generic':   FastcLocal{ typ: flat.source_type_int }
		}
	}
}

fn test_fastc_atomic_values_match_streaming_render_and_parser_state() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value\n', 'value;', 'value)', 'renamed,', 'flag {', '0xffff,', '0012;', '1_024]',
		'1.25}', 'true {', 'false:', '"text"}', '`λ`;', "c'abc';", 'value /* comment */;',
		'value\n*ptr = 2;'] {
		mut fast := fastc_atomic_test_parser(source)
		mut slow := fastc_atomic_test_parser(source)
		atom := fast.try_read_atomic_expression(stops, false, true)
		assert atom.found, source
		rendered := slow.read_expression_with_prefix_mode_impl('', stops, false, false, true) or { panic(err) }
		assert atom.source == rendered, '${source}: ${atom.source} != ${rendered}'
		assert fast.last_expression == slow.last_expression, source
		assert fast.last_expression_type == slow.last_expression_type, source
		assert fast.expected_expression_type == slow.expected_expression_type, source
		assert fast.last_multi_return_types == slow.last_multi_return_types, source
		assert fast.tok == slow.tok && fast.lit == slow.lit, source
		assert fast.s.offset == slow.s.offset && fast.s.pos == slow.s.pos, source
		assert fast.s.lit == slow.s.lit && fast.s.diagnostics == slow.s.diagnostics, source
		assert fast.s.in_str_incomplete == slow.s.in_str_incomplete, source
		assert fast.s.in_str_inter == slow.s.in_str_inter, source
	}
}

fn test_fastc_atomic_rejections_preserve_current_expression_and_scanner() {
	stops := [token.Token.semicolon, .comma, .rpar, .rsbr, .rcbr, .colon, .lcbr]
	for source in ['value + 1;', 'value\n + 1;', 'value\n .field;', 'value\n/* comment */ .field;',
		'value\n// comment\n + 1;', 'reference;', 'optional;', 'narrowed;', 'object;', 'generic;',
		'value();', r'"${value}";'] {
		mut g := fastc_atomic_test_parser(source)
		before := g.s
		before_tok := g.tok
		before_lit := g.lit
		previous_expression := g.last_expression.clone()
		assert !g.try_read_atomic_expression(stops, false, true).found, source
		assert g.s.offset == before.offset && g.s.pos == before.pos, source
		assert g.s.lit == before.lit && g.s.diagnostics == before.diagnostics, source
		assert g.s.in_str_incomplete == before.in_str_incomplete, source
		assert g.s.in_str_inter == before.in_str_inter, source
		assert g.tok == before_tok && g.lit == before_lit, source
		assert g.last_expression == previous_expression, source
		assert g.last_expression_type == flat.source_type_void, source
		assert g.expected_expression_type == flat.source_type_f64, source
	}
}
