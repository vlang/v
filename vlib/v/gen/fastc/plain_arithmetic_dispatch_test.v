module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_arithmetic_dispatch_parser(allow bool) Parser {
	prefs := pref.new_preferences()
	return Parser{
		prefs:                   prefs
		s:                       scanner.new_scanner(prefs, .normal)
		selfhost:                true
		numeric_ops_are_builtin: allow
		locals:                  {
			'a':          FastcLocal{ typ: flat.source_type_int }
			'b':          FastcLocal{ typ: flat.source_type_int }
			'wide':       FastcLocal{ typ: flat.source_type_u64 }
			'byte':       FastcLocal{ typ: flat.source_type_u8 }
			'f':          FastcLocal{ typ: flat.source_type_f64 }
			'character':  FastcLocal{ typ: flat.source_type_char }
			'rune_value': FastcLocal{ typ: flat.source_type_rune }
			'text':       FastcLocal{ typ: flat.source_type_string }
			'pointer':    FastcLocal{ typ: flat.source_type_id('int*') }
			'reference':  FastcLocal{ typ: flat.source_type_int, is_reference: true }
			'optional':   FastcLocal{ typ: fastc_type_option, option_value_type: flat.source_type_int }
			'origin':     FastcLocal{ typ: flat.source_type_int, smartcast_origin_type: flat.source_type_id('Choice') }
		}
	}
}

fn fastc_arithmetic_dispatch_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('plain_arithmetic.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		kind := scan.scan()
		if kind in [.eof, .semicolon] { break }
		tokens << FastcExpressionToken{ tok: kind, lit: scan.lit }
	}
	return tokens
}

fn test_fastc_flat_numeric_arithmetic_skips_special_dispatch_with_identical_results() {
	slow := fastc_arithmetic_dispatch_parser(false)
	fast := fastc_arithmetic_dispatch_parser(true)
	for source in ['a + b', 'a - b * 3', 'a / b % 3', '-a + +b', 'a * -b', 'a + -b / 2', 'wide + byte',
		'byte * a', 'f + 1.25 * f', '1_000 + 2', '0xffffffffffffffff + 1', '1.25 + 2.5 * 3.0'] {
		tokens := fastc_arithmetic_dispatch_tokens(source)
		assert fast.plain_numeric_arithmetic_expression(tokens), source
		assert !slow.plain_numeric_arithmetic_expression(tokens), source
		flags := fastc_special_token_flags(tokens)
		assert flags.has_binary && !flags.has_lpar && !flags.has_comparison, source
		raw := fast.render_raw_expression_tokens(tokens) or { panic(source) }
		assert fast.render_special_expression(tokens, raw) == none, source
		assert slow.render_special_expression(tokens, raw) == none, source
		assert fast.render_overloaded_binary_expression(tokens) == none, source
		assert slow.render_overloaded_binary_expression(tokens) == none, source
		fast_type := fast.infer_expression_type(tokens) or { panic(source) }
		slow_type := slow.infer_expression_type(tokens) or { panic(source) }
		assert fast_type == slow_type, source
		fast_argument := fast.render_call_argument_expression(tokens, fast_type) or { panic(source) }
		slow_argument := slow.render_call_argument_expression(tokens, slow_type) or { panic(source) }
		assert fast_argument == slow_argument, source
		assert fast.temp_id == 0 && slow.temp_id == 0
		assert fast.last_expression_type == 0 && slow.last_expression_type == 0
		assert fast.last_option_value_type == 0 && slow.last_option_value_type == 0
		assert fast.last_multi_return_types.ids.len == 0 && slow.last_multi_return_types.ids.len == 0
		assert fast.last_expression.len == 0 && slow.last_expression.len == 0
		assert fast.s.offset == 0 && slow.s.offset == 0
		assert fast.s.diagnostics.len == 0 && slow.s.diagnostics.len == 0
	}
}

fn test_fastc_flat_numeric_arithmetic_keeps_grouping_cast_and_special_value_paths() {
	g := fastc_arithmetic_dispatch_parser(true)
	for source in ['a & b', 'a << b', 'a > b', 'a = b + 1', 'call(a) + b', 'a.field + b', 'a[0] + b',
		'reference + a', 'optional + a', 'origin + a', 'text + text', 'character + a', 'rune_value + a',
		'pointer + a', 'int(a + b)', 'string(a) + string(b)', 'unknown + a', '*pointer', 'a +',
		'a b'] {
		assert !g.plain_numeric_arithmetic_expression(fastc_arithmetic_dispatch_tokens(source)), source
	}
	for source in ['(a + b)', 'a * (b + 2)', '-a + -(b / 2)'] {
		tokens := fastc_arithmetic_dispatch_tokens(source)
		assert g.plain_numeric_arithmetic_expression(tokens), source
		assert fastc_special_token_flags(tokens).has_lpar, source
		// The new full-dispatch gate deliberately excludes all parentheses.
		raw := g.render_raw_expression_tokens(tokens) or { panic(source) }
		assert g.render_special_expression(tokens, raw) == none, source
	}
}

fn test_fastc_flat_numeric_arithmetic_keeps_translated_checked_division() {
	base := fastc_arithmetic_dispatch_parser(true)
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	translated := Parser{ ...base, prefs: prefs, translated: true }
	assert translated.checked_target_division_enabled()
	tokens := fastc_arithmetic_dispatch_tokens('a / b')
	assert !translated.plain_numeric_arithmetic_expression(tokens)
	raw := translated.render_raw_expression_tokens(tokens) or { panic('raw division') }
	division := translated.render_special_expression(tokens, raw) or { panic('checked division') }
	assert division.source.contains('FastC checked division requires integer operands')
	assert division.typ == flat.source_type_int
}
