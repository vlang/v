module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn fastc_render_borrow_test_tokens(source string) []FastcExpressionToken {
	prefs := pref.new_preferences()
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(token.File.unindexed('render_token_borrow.v', source.len), source)
	mut tokens := []FastcExpressionToken{}
	for {
		tok := scan.scan()
		if tok == .eof {
			break
		}
		if tok != .semicolon {
			tokens << FastcExpressionToken{
				tok: tok
				lit: scan.lit
			}
		}
	}
	return tokens
}

fn test_fastc_numeric_narrowing_keeps_reference_and_origin_requirements() {
	prefs := pref.new_preferences()
	for is_reference in [false, true] {
		for has_origin in [false, true] {
			g := Parser{
				prefs:  prefs
				s:      scanner.new_scanner(prefs, .normal)
				locals: {
					'value': FastcLocal{
						typ:                   flat.source_type_id('u64*')
						is_reference:          is_reference
						smartcast_origin_type: if has_origin { flat.source_type_int } else { 0 }
					}
				}
			}
			for source in ['value', '((value))'] {
				tokens := fastc_render_borrow_test_tokens(source)
				result := g.narrowed_numeric_operand_type(tokens, flat.source_type_int)
				assert result == if is_reference && has_origin {
					flat.source_type_u64
				} else {
					flat.source_type_int
				}
			}
		}
	}
}

fn test_fastc_map_option_rendering_keeps_borrowed_tokens_unchanged() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:  prefs
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'items': FastcLocal{ typ: flat.source_type_id('Map_string_int') }
			'key':   FastcLocal{ typ: flat.source_type_string }
		}
	}
	for source in ['items[key]', '(items[key]', '&items[key]', '(&items[key]', '(*(&items))[key]'] {
		tokens := fastc_render_borrow_test_tokens(source)
		before := tokens.clone()
		rendered := g.render_map_lookup_option_expression(tokens) or {
			panic('missing map lookup ${source}')
		}
		assert rendered.source.contains('builtin__map_get_check'), rendered.source
		assert rendered.typ == flat.source_type_id(if source.contains('&items[') {
			'int*'
		} else {
			'int'
		})
		assert tokens == before, source
	}
	for source in ['items', 'items[key', '(key)', 'items[0](key)'] {
		tokens := fastc_render_borrow_test_tokens(source)
		before := tokens.clone()
		assert g.render_map_lookup_option_expression(tokens) == none, source
		assert tokens == before, source
	}
}

fn test_fastc_generic_normalization_preserves_surrounding_indexes_and_token_sources() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs:  prefs
		s:      scanner.new_scanner(prefs, .normal)
		locals: {
			'values': FastcLocal{ typ: flat.source_type_id('Array_int') }
		}
	}
	mut tokens := fastc_render_borrow_test_tokens('values[0] + first_mono_int[int](1) + second_mono_int[int](2) + values[3]')
	for index, item in tokens {
		if item.tok == .number && item.lit in ['1', '2'] {
			tokens[index] = FastcExpressionToken{
				...item
				source: if item.lit == '1' { 'first_value' } else { 'second_value' }
				typ:    flat.source_type_u64
			}
		}
	}
	before := tokens.clone()
	rendered := g.render_explicit_generic_call_expression(tokens) or {
		panic('missing normalized generic calls')
	}
	assert rendered.source.contains('first_mono_int(first_value)'), rendered.source
	assert rendered.source.contains('second_mono_int(second_value)'), rendered.source
	first_index := rendered.source.index('first_mono_int') or { panic('missing first call') }
	second_index := rendered.source.index('second_mono_int') or { panic('missing second call') }
	assert first_index < second_index
	assert !rendered.source.contains('[int]'), rendered.source
	assert rendered.source.contains('values'), rendered.source
	assert tokens == before
	for source in ['values[0]', 'first_mono_int[int]', 'first_mono_int[value](1)', 'ordinary[int](1)'] {
		unchanged := fastc_render_borrow_test_tokens(source)
		copy := unchanged.clone()
		assert g.render_explicit_generic_call_expression(unchanged) == none, source
		assert unchanged == copy, source
	}
}
