import json2
import math

struct Item {
	name  string
	qs    []string
	n     int
	deep  map[string][]int
	inner Item2
}

struct Item2 {
	flag bool
}

struct OptionalSettings {
	present ?int
	absent  ?int
	label   ?string
	item    ?Item2
	color   ?Color
}

fn test_encode_options_preserves_present_payloads_and_none() {
	settings := OptionalSettings{
		present: 42
		label:   'value'
		item:    Item2{ flag: true }
		color:   Color.green
	}
	text := json2.encode(settings, json5: true)
	decoded := json2.decode[OptionalSettings](text, json5: true)!
	assert decoded.present or { -1 } == 42
	assert decoded.absent == none
	assert decoded.label or { '' } == 'value'
	assert (decoded.item or { Item2{} }).flag
	assert decoded.color or { Color.red } == Color.green
	zero := ?int(0)
	missing := ?int(none)
	assert json2.encode(zero, json5: true) == '0'
	assert json2.encode(missing, json5: true) == 'null'
}

enum Color {
	red
	green
}

struct WithEnum {
	color Color
	list  []Color
	opt   ?Color
}

struct Skip {
	hidden string @[skip]
	shown  string
}

struct Renamed {
	first string @[json: 'First Name']
}

struct Embedded {
	common string
}

struct Holder {
	Embedded
	own string
}

struct Upper {
mut:
	v string
}

fn (u Upper) to_json() string {
	return json2.encode(u.v, json2.EncoderOptions{
		json5:         true
		single_quotes: true
	})
}

struct WithCustom {
	label Upper
}

fn test_encode_compact_by_default() {
	got := json2.encode(json2.parse('{ a: 1, b: "x" }', json5: true) or { panic(err) }, json5: true)
	assert got == '{"a":1,"b":"x"}'
}

fn test_encode_scalars() {
	assert json2.encode(json2.parse('null', json5: true) or { panic(err) }, json5: true) == 'null'
	assert json2.encode(json2.parse('true', json5: true) or { panic(err) }, json5: true) == 'true'
	assert json2.encode(json2.parse('1.5', json5: true) or { panic(err) }, json5: true) == '1.5'
	assert json2.encode(json2.parse('"s"', json5: true) or { panic(err) }, json5: true) == '"s"'
	assert json2.encode(json2.parse('false', json5: true) or { panic(err) }, json5: true) == 'false'
}

fn test_encode_empty_containers() {
	assert json2.encode(json2.parse('{}', json5: true) or { panic(err) }, json5: true) == '{}'
	assert json2.encode(json2.parse('[]', json5: true) or { panic(err) }, json5: true) == '[]'
}

fn test_encode_normalizes_number_text() {
	got := json2.encode(json2.parse('[0x1F, +1, .5, 5., -Infinity, NaN]', json5: true) or { panic(err) },
		json5: true
	)
	assert got == '[31,1,0.5,5,-Infinity,NaN]'
}

fn test_encode_indent() {
	got := json2.encode(json2.parse('{ a: 1, b: [2] }', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:         true
			prettify:      true
			indent_string: '  '
		})
	assert got == '{
  "a": 1,
  "b": [
    2
  ]
}'
}

fn test_encode_unquoted_keys() {
	got := json2.encode(json2.parse('{ a: 1, "b c": 2 }', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:         true
			unquoted_keys: true
		})
	assert got == '{a:1,"b c":2}'
}

fn test_encode_and_decode_unicode_identifier_categories() {
	// Nl may start an identifier; Mn, Mc, Nd and Pc may continue it.
	for key in ['\u2165', '\U00010341', 'a\u1ab0', 'a\u0903', 'a\u0660', 'a\u203f', 'a\U0001d165',
		'a\U0001d7ce', 'a\u200c', 'a\u200d'] {
		value := {
			key: 1
		}
		text := json2.encode(value, json5: true, unquoted_keys: true)
		assert text == '{' + key + ':1}'
		assert json2.decode[map[string]int](text, json5: true)! == value
	}
	// No and Me are excluded; marks, digits and other connectors cannot start keys.
	for key in ['a\u00b2', 'a\u0488', 'a\U0001f100', '\u0903a', '\u203fa', '\u0660a', 'a-'] {
		value := {
			key: 1
		}
		text := json2.encode(value, json5: true, unquoted_keys: true)
		assert text == '{"' + key + '":1}'
		assert json2.decode[map[string]int](text, json5: true)! == value
		json2.decode[map[string]int]('{' + key + ':1}', json5: true) or { continue }
		assert false, 'invalid bare key was accepted: ${key}'
	}
}

fn test_encode_single_quotes() {
	got := json2.encode(json2.parse('{ "it\'s": 1 }', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:         true
			single_quotes: true
		})
	assert got == "{'it\\'s':1}"
}

fn test_encode_trailing_commas() {
	got := json2.encode(json2.parse('{ a: 1, b: [2] }', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:           true
			prettify:        true
			indent_string:   '  '
			trailing_commas: true
		})
	assert got == '{
  "a": 1,
  "b": [
    2,
  ],
}'
}

fn test_encode_escapes() {
	got := json2.encode(json2.parse(r'"a\nb"', json5: true) or { panic(err) },
		json5: true
	)
	assert got == r'"a\nb"'
}

fn test_encode_escapes_only_the_chosen_quote() {
	// With single quotes chosen, `'` is escaped and `"` stays literal.
	got := json2.encode(json2.parse('"a\'b"', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:         true
			single_quotes: true
		})
	assert got == "'a\\'b'"
}

fn test_encode_line_separators_with_each_quote_style() {
	value := 'V\'s "quoted" 🎼\u2028next\u2029line'
	for single_quotes in [false, true] {
		for escape_unicode in [false, true] {
			text := json2.encode(value,
				json5:          true
				single_quotes:  single_quotes
				escape_unicode: escape_unicode
			)
			assert text.contains(r'\u2028')
			assert text.contains(r'\u2029')
			assert !text.contains('\u2028')
			assert !text.contains('\u2029')
			assert text[0] == if single_quotes { `'` } else { `"` }
			assert json2.decode[string](text, json5: true)! == value
		}
	}
	assert json2.encode('a\u2028b\u2029c', json5: true) == r'"a\u2028b\u2029c"'
	// Ordinary JSON permits these code points literally.
	assert json2.encode('a\u2028b\u2029c') == '"a\u2028b\u2029c"'
	keys := {
		'key\u2028\u2029': 'value\u2028\u2029'
	}
	text := json2.encode(keys, json5: true, unquoted_keys: true)
	assert text == r'{"key\u2028\u2029":"value\u2028\u2029"}'
	assert json2.decode[map[string]string](text, json5: true)! == keys
	mut appended := 'prefix:'.bytes()
	json2.encode_append('a\u2028b', mut appended, json5: true)
	assert appended.bytestr() == r'prefix:"a\u2028b"'
}

fn test_encode_control_character_escape() {
	got := json2.encode(json2.parse(r'"\u0001"', json5: true) or { panic(err) },
		json5: true
	)
	assert got == r'"\u0001"'
}

fn test_encode_struct() {
	got := json2.encode(Item{
		name: 'x'
		qs:   ['a']
		n:    2
	}, json5: true)
	assert got.contains('"name":"x"')
	assert got.contains('"qs":["a"]')
	assert got.contains('"n":2')
}

fn test_encode_struct_respects_skip() {
	got := json2.encode(Skip{
		hidden: 'h'
		shown:  's'
	}, json5: true)
	assert !got.contains('hidden')
	assert got.contains('"shown":"s"')
}

fn test_encode_struct_uses_renamed_key() {
	got := json2.encode(Renamed{
		first: 'f'
	}, json5: true)
	assert got == '{"First Name":"f"}'
}

fn test_encode_enum_as_name() {
	got := json2.encode(WithEnum{
		color: .green
		list:  [.red, .green]
		opt:   .red
	}, json5: true)
	assert got.contains('"color":"green"')
	assert got.contains('"list":["red","green"]')
}

fn test_encode_map_and_nested_array() {
	got := json2.encode(Item{
		deep: {
			'k': [1, 2]
		}
	}, json5: true)
	assert got.contains('"deep":{"k":[1,2]}')
}

fn test_encode_embedded_struct_is_flat() {
	got := json2.encode(Holder{
		common: 'c'
		own:    'o'
	}, json5: true)
	assert got == '{"common":"c","own":"o"}'
}

fn test_encode_custom_to_json() {
	got := json2.encode(WithCustom{
		label: Upper{
			v: 'hi'
		}
	}, json5: true)
	assert got.contains("'hi'")
}

fn test_encode_infinity_and_nan_from_float() {
	inf := unsafe { math.f64_from_bits(u64(0x7FF0000000000000)) }
	got := json2.encode(inf, json5: true)
	assert got == 'Infinity'
	nan := unsafe { math.f64_from_bits(u64(0x7FF8000000000000)) }
	assert json2.encode(nan, json5: true) == 'NaN'
}

fn test_reindent() {
	got := json2.encode(json2.parse('{a:1,b:[1,2],}', json5: true)!,
		prettify:      true
		indent_string: '  '
	)
	assert got.contains('\n  "a": 1')
}

fn test_quote_string() {
	assert json2.encode('a"b') == '"a\\"b"'
}

fn test_roundtrip() {
	source := '{
		// a comment
		name: "V",
		list: [1, 2, 3,],
		nested: { deep: true },
	}'
	first := json2.parse(source, json5: true) or { panic(err) }
	text := json2.encode(first, json5: true)
	second := json2.parse(text, json5: true) or { panic(err) }
	assert json2.encode(second, json5: true) == text
}

fn test_roundtrip_normalizes_number_spelling() {
	first := json2.parse('{ a: 0x10, b: .5 }', json5: true) or { panic(err) }
	assert json2.encode(first, json5: true) == '{"a":16,"b":0.5}'
}

fn test_output_is_valid_json() {
	// Finite values without JSON5 formatting still produce valid JSON.
	text := json2.encode(json2.parse('{ a: 1, b: ["x"], c: null }', json5: true) or { panic(err) },
		json5: true
	)
	back := json2.decode[map[string]json2.Any](text, strict: true) or { panic(err) }
	assert (back['a'] or { json2.Any(0) }).int() == 1
}

fn test_unicode_keys_are_not_unquoted() {
	// A non-identifier key stays quoted even with `unquoted_keys`.
	got := json2.encode(json2.parse('{ "a b": 1 }', json5: true) or { panic(err) },
		json2.EncoderOptions{
			json5:         true
			unquoted_keys: true
		})
	assert got == '{"a b":1}'
}
