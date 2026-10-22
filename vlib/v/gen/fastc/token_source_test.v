module fastc

import v.flat
import v.pref
import v.scanner
import v.token

fn test_token_source45_actual_layout_default_and_empty_sources() {
	if sizeof(voidptr) == 8 {
		assert sizeof(string) == 24
		assert sizeof(FastcTokenSource) == 24
		assert sizeof(FastcExpressionToken) == 64
	}
	empty := FastcExpressionToken{}
	assert isnil(empty.source)
	assert !empty.has_source()
	assert empty.source_text() == ''
	assert isnil(fastc_token_source(''))
	assert empty == FastcExpressionToken{ source: fastc_token_source('') }
	assert [empty] == [FastcExpressionToken{ source: fastc_token_source('') }]
}

fn token_source45_returned(text string) FastcExpressionToken {
	return FastcExpressionToken{ tok: .name, lit: 'logical_name', source: fastc_token_source(text), typ: flat.source_type_int }
}

fn test_token_source45_preserves_headers_contents_and_shared_copy_lifetimes() {
	text := 'first\x00last'.clone()
	original := token_source45_returned(text)
	assert original.has_source()
	assert original.source_text() == text
	assert original.source_text().len == text.len
	assert original.source_text().str == text.str
	mut values := [original]
	mut copies := values.clone()
	assert voidptr(values[0].source) == voidptr(copies[0].source)
	values[0].typ = flat.source_type_string
	values[0].lit = 'different_name'
	assert copies[0].typ == flat.source_type_int
	assert copies[0].lit == 'logical_name'
	assert copies[0].source_text() == text
	// Releasing one token array must not release its shared immutable payload.
	unsafe { values.free() }
	for i in 0 .. 256 {
		_ := token_source45_returned('later_${i}')
	}
	assert copies[0].source_text() == text
	copies[0].typ = flat.source_type_bool
	assert original.typ == flat.source_type_int
}

fn test_token_source45_content_equality_ignores_payload_identity_and_compares_every_logical_field() {
	left := FastcExpressionToken{ tok: .name, lit: 'value', source: fastc_token_source('rendered'), typ: flat.source_type_int }
	right := FastcExpressionToken{ tok: .name, lit: 'value', source: fastc_token_source('rendered'.clone()), typ: flat.source_type_int }
	assert voidptr(left.source) != voidptr(right.source)
	assert left == right
	assert [left] == [right]
	assert [left].clone() == [right]
	for changed in [
		FastcExpressionToken{ ...right, source: fastc_token_source('different') },
		FastcExpressionToken{ ...right, unsafe_depth: 1 },
		FastcExpressionToken{ ...right, tok: .number },
		FastcExpressionToken{ ...right, chan_element_type: flat.source_type_int },
		FastcExpressionToken{ ...right, option_value_type: flat.source_type_int },
		FastcExpressionToken{ ...right, is_mut_argument: true },
		FastcExpressionToken{ ...right, is_statement: true },
		FastcExpressionToken{ ...right, is_capturing_closure: true },
		FastcExpressionToken{ ...right, is_storage: true },
		FastcExpressionToken{ ...right, typ: flat.source_type_string },
		FastcExpressionToken{ ...right, lit: 'different' },
	] {
		assert left != changed
		assert [left] != [changed]
	}
}

fn test_token_source45_renderer_and_debug_keep_synthetic_text_and_original_spelling() {
	prefs := pref.new_preferences()
	g := Parser{ prefs: prefs, s: scanner.new_scanner(prefs, .normal), selfhost: true }
	for kind in [token.Token.name, .string] {
		atom := FastcExpressionToken{ tok: kind, lit: 'original', source: fastc_token_source('synthetic_value'), typ: flat.source_type_int }
		assert g.render_raw_expression_tokens([atom]) or { panic('missing source') } == 'synthetic_value'
		assert g.render_call_argument_expression([atom], flat.source_type_int) or { panic('missing argument source') } == 'synthetic_value'
		assert fastc_expression_tokens_debug([atom]) == '${kind.str()}:original'
	}
	items := [FastcExpressionToken{ tok: .name, lit: 'first' },
		FastcExpressionToken{ tok: .name, lit: 'second' }]
	assert fastc_comparison_memo_range_key(items, 1, 2, 0) == fastc_comparison_memo_key(items[1..2], 0)
}

type TokenSourceAlias53 = FastcExpressionToken

type TokenSourcePointerAlias53 = &FastcExpressionToken

type TokenSourceNestedPointerAlias53 = TokenSourcePointerAlias53

struct TokenSourceAliases53 {
	value   TokenSourceAlias53
	pointer TokenSourceNestedPointerAlias53
}

fn test_token_source_aliases_preserve_token_content_and_pointer_identity() {
	left := FastcExpressionToken{ tok: .name, lit: 'value', source: fastc_token_source('rendered'), typ: flat.source_type_int }
	right := FastcExpressionToken{ tok: .name, lit: 'value', source: fastc_token_source('rendered'.clone()), typ: flat.source_type_int }
	assert voidptr(left.source) != voidptr(right.source)
	alias_left := TokenSourceAlias53(left)
	alias_right := TokenSourceAlias53(right)
	assert [alias_left] == [alias_right]
	assert [[alias_left]] == [[alias_right]]
	fixed_left := [alias_left]!
	fixed_right := [alias_right]!
	assert [fixed_left] == [fixed_right]
	pointer_left := TokenSourceNestedPointerAlias53(&left)
	pointer_same := TokenSourceNestedPointerAlias53(&left)
	pointer_right := TokenSourceNestedPointerAlias53(&right)
	assert [pointer_left] == [pointer_same]
	assert [pointer_left] != [pointer_right]
	assert [[pointer_left]] != [[pointer_right]]
	assert [TokenSourceAliases53{ value: alias_left, pointer: pointer_left }] == [TokenSourceAliases53{ value: alias_right, pointer: pointer_same }]
	assert [TokenSourceAliases53{ value: alias_left, pointer: pointer_left }] != [TokenSourceAliases53{ value: alias_right, pointer: pointer_right }]
}
