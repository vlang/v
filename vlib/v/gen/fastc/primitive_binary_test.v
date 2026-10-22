module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_plain_numeric_binary_test_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('primitive_binary.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] {
			break
		}
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn fastc_plain_numeric_binary_test_parser(allow bool) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		selfhost:                true
		numeric_ops_are_builtin: allow
		locals:                  {
			'a':         FastcLocal{ typ: flat.source_type_int }
			'b':         FastcLocal{ typ: flat.source_type_int }
			'large':     FastcLocal{ typ: flat.source_type_u64 }
			'small':     FastcLocal{ typ: flat.source_type_u8 }
			'floating':  FastcLocal{ typ: flat.source_type_f64 }
			'character': FastcLocal{ typ: flat.source_type_char }
			'letter':    FastcLocal{ typ: flat.source_type_rune }
			'reference': FastcLocal{ typ: flat.source_type_id('int*'), is_reference: true }
			'optional':  FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_int }
			'origin':    FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Scalar') }
			'alias':     FastcLocal{ typ: flat.source_type_id('Alias') }
			'text':      FastcLocal{ typ: flat.source_type_string }
		}
	}
}

fn test_fastc_plain_numeric_binary_guard_preserves_original_dispatch_result() {
	slow := fastc_plain_numeric_binary_test_parser(false)
	fast := fastc_plain_numeric_binary_test_parser(true)
	for source in ['a + b', 'a - b * (a + 4)', 'a / 4 % 3', '(a + (b * 2))', '-a + +b', 'a + -(b / 2)',
		'large + small', 'small * a', 'floating + 1.25 * floating', '0xffffffffffffffff + 1',
		'1.25 + 2.5 * 3.0'] {
		tokens := fastc_plain_numeric_binary_test_tokens(source)
		assert fast.plain_numeric_arithmetic_expression(tokens), source
		assert slow.render_overloaded_binary_expression(tokens) == none, source
		assert fast.render_overloaded_binary_expression(tokens) == none, source
		assert fast.render_raw_expression_tokens(tokens) == slow.render_raw_expression_tokens(tokens), source
	}
}

fn test_fastc_plain_numeric_binary_guard_keeps_non_arithmetic_and_represented_values() {
	g := fastc_plain_numeric_binary_test_parser(true)
	for source in ['a', '(a)', 'a == b', 'a & b', 'a << b', 'a > b', 'a = b + 1', 'call(a) + b',
		'a.field + b', 'a[0] + b', 'reference + a', 'optional + a', 'origin + a', 'alias + a',
		'text + text', 'character + a', 'letter + a', 'unknown + a', '*reference', 'a +', 'a b',
		'(a + b', 'a + b)'] {
		assert !g.plain_numeric_arithmetic_expression(fastc_plain_numeric_binary_test_tokens(source)), source
	}
	for item in [
		FastcExpressionToken{ tok: .name, lit: 'a', source: fastc_token_source('lowered_a'), typ: flat.source_type_int },
		FastcExpressionToken{ tok: .name, lit: 'a', typ: flat.source_type_u64 },
		FastcExpressionToken{ tok: .name, lit: 'a', is_mut_argument: true },
		FastcExpressionToken{ tok: .number, lit: '1', typ: flat.source_type_id('Alias') },
	] {
		assert !g.plain_numeric_arithmetic_expression([item, FastcExpressionToken{ tok: .plus },
			FastcExpressionToken{ tok: .number, lit: '1' }])
	}
}

fn test_fastc_plain_numeric_binary_guard_respects_program_metadata_and_translated_files() {
	assert fastc_numeric_arithmetic_has_no_overloads(map[string]FastcFunctionSignature{}, flat.TypeTextBindings{}, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
	for name in ['int.+', 'u64.-', 'f32.*', 'usize./', 'u8.%'] {
		assert !fastc_numeric_arithmetic_has_no_overloads({
			name: FastcFunctionSignature{}
		}, flat.TypeTextBindings{}, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
	}
	for type_name in ['int', 'f32', 'u64'] {
		mut bindings := flat.TypeTextBindings{}
		bindings.set(type_name, 'Declared')
		assert !fastc_numeric_arithmetic_has_no_overloads(map[string]FastcFunctionSignature{}, bindings, flat.TypeTextBindings{}, map[string]flat.TypeTextBindings{})
		assert !fastc_numeric_arithmetic_has_no_overloads(map[string]FastcFunctionSignature{}, flat.TypeTextBindings{}, bindings, map[string]flat.TypeTextBindings{})
		assert !fastc_numeric_arithmetic_has_no_overloads(map[string]FastcFunctionSignature{}, flat.TypeTextBindings{}, flat.TypeTextBindings{}, {
			type_name: flat.TypeTextBindings{}
		})
	}
	prefs := pref.new_preferences()
	translated := Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		selfhost:                true
		translated:              true
		numeric_ops_are_builtin: true
	}
	assert !translated.plain_numeric_arithmetic_expression(fastc_plain_numeric_binary_test_tokens('1 + 2'))
	default_parser := fastc_plain_numeric_binary_test_parser(false)
	assert !default_parser.plain_numeric_arithmetic_expression(fastc_plain_numeric_binary_test_tokens('a + b'))
}

fn test_fastc_plain_numeric_binary_guard_rejects_active_smartcasts_and_nonselfhost_mode() {
	prefs := pref.new_preferences()
	tokens := fastc_plain_numeric_binary_test_tokens('a + 1')
	narrowed := Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		selfhost:                true
		numeric_ops_are_builtin: true
		locals:                  {
			'a': FastcLocal{ typ: flat.source_type_int }
		}
		member_smartcasts:       {
			'a': FastcMemberSmartcast{}
		}
	}
	assert !narrowed.plain_numeric_arithmetic_expression(tokens)
	nonselfhost := Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		numeric_ops_are_builtin: true
		locals:                  {
			'a': FastcLocal{ typ: flat.source_type_int }
		}
	}
	assert !nonselfhost.plain_numeric_arithmetic_expression(tokens)
}
