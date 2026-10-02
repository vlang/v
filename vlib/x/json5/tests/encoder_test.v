import json2
import math
import x.json5

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
	text := json5.encode(settings)
	decoded := json5.decode[OptionalSettings](text)!
	assert decoded.present or { -1 } == 42
	assert decoded.absent == none
	assert decoded.label or { '' } == 'value'
	assert (decoded.item or { Item2{} }).flag
	assert decoded.color or { Color.red } == Color.green
	zero := ?int(0)
	missing := ?int(none)
	assert json5.encode(zero) == '0'
	assert json5.encode(missing) == 'null'
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
	first string @[json5: 'First Name']
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

fn (mut u Upper) to_json5() string {
	return json5.encode_with_opts(u.v, json5.EncodeOpts{
		single_quotes: true
	})
}

struct WithCustom {
	label Upper
}

fn test_encode_compact_by_default() {
	got := json5.encode(json5.parse('{ a: 1, b: "x" }') or { panic(err) })
	assert got == '{"a":1,"b":"x"}'
}

fn test_encode_scalars() {
	assert json5.encode(json5.parse('null') or { panic(err) }) == 'null'
	assert json5.encode(json5.parse('true') or { panic(err) }) == 'true'
	assert json5.encode(json5.parse('1.5') or { panic(err) }) == '1.5'
	assert json5.encode(json5.parse('"s"') or { panic(err) }) == '"s"'
	assert json5.encode(json5.parse('false') or { panic(err) }) == 'false'
}

fn test_encode_empty_containers() {
	assert json5.encode(json5.parse('{}') or { panic(err) }) == '{}'
	assert json5.encode(json5.parse('[]') or { panic(err) }) == '[]'
}

fn test_encode_preserves_number_text() {
	got := json5.encode(json5.parse('[0x1F, +1, .5, 5., -Infinity, NaN]') or { panic(err) })
	assert got == '[0x1F,+1,.5,5.,-Infinity,NaN]'
}

fn test_encode_indent() {
	got := json5.encode_with_opts(json5.parse('{ a: 1, b: [2] }') or { panic(err) },
		json5.EncodeOpts{
			indent: '  '
		})
	assert got == '{
  "a": 1,
  "b": [
    2
  ]
}'
}

fn test_encode_unquoted_keys() {
	got := json5.encode_with_opts(json5.parse('{ a: 1, "b c": 2 }') or { panic(err) },
		json5.EncodeOpts{
			unquoted_keys: true
		})
	assert got == '{a:1,"b c":2}'
}

fn test_encode_single_quotes() {
	got := json5.encode_with_opts(json5.parse('{ "it\'s": 1 }') or { panic(err) },
		json5.EncodeOpts{
			single_quotes: true
		})
	assert got == "{'it\\'s':1}"
}

fn test_encode_trailing_commas() {
	got := json5.encode_with_opts(json5.parse('{ a: 1, b: [2] }') or { panic(err) },
		json5.EncodeOpts{
			indent:          '  '
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
	got := json5.encode_with_opts(json5.parse(r'"a\nb"') or { panic(err) },
		json5.default_encode_opts)
	assert got == r'"a\nb"'
}

fn test_encode_escapes_only_the_chosen_quote() {
	// With single quotes chosen, `'` is escaped and `"` stays literal.
	got := json5.encode_with_opts(json5.parse('"a\'b"') or { panic(err) },
		json5.EncodeOpts{
			single_quotes: true
		})
	assert got == "'a\\'b'"
}

fn test_encode_control_character_escape() {
	got := json5.encode_with_opts(json5.parse(r'"\u0001"') or { panic(err) },
		json5.default_encode_opts)
	assert got == r'"\u0001"'
}

fn test_encode_struct() {
	got := json5.encode(Item{
		name: 'x'
		qs:   ['a']
		n:    2
	})
	assert got.contains('"name":"x"')
	assert got.contains('"qs":["a"]')
	assert got.contains('"n":2')
}

fn test_encode_struct_respects_skip() {
	got := json5.encode(Skip{
		hidden: 'h'
		shown:  's'
	})
	assert !got.contains('hidden')
	assert got.contains('"shown":"s"')
}

fn test_encode_struct_uses_renamed_key() {
	got := json5.encode(Renamed{
		first: 'f'
	})
	assert got == '{"First Name":"f"}'
}

fn test_encode_enum_as_name() {
	got := json5.encode(WithEnum{
		color: .green
		list:  [.red, .green]
		opt:   .red
	})
	assert got.contains('"color":"green"')
	assert got.contains('"list":["red","green"]')
}

fn test_encode_map_and_nested_array() {
	got := json5.encode(Item{
		deep: {
			'k': [1, 2]
		}
	})
	assert got.contains('"deep":{"k":[1,2]}')
}

fn test_encode_embedded_struct_is_flat() {
	got := json5.encode(Holder{
		common: 'c'
		own:    'o'
	})
	assert got == '{"common":"c","own":"o"}'
}

fn test_encode_custom_to_json5() {
	got := json5.encode(WithCustom{
		label: Upper{
			v: 'hi'
		}
	})
	assert got.contains("'hi'")
}

fn test_encode_infinity_and_nan_from_float() {
	inf := unsafe { math.f64_from_bits(u64(0x7FF0000000000000)) }
	got := json5.encode(inf)
	assert got == 'Infinity'
	nan := unsafe { math.f64_from_bits(u64(0x7FF8000000000000)) }
	assert json5.encode(nan) == 'NaN'
}

fn test_reindent() {
	got := json5.reindent('{a:1,b:[1,2],}') or { panic(err) }
	assert got.contains('\n  "a": 1')
}

fn test_quote_string() {
	assert json5.quote_string('a"b') == '"a\\"b"'
}

fn test_roundtrip() {
	source := '{
		// a comment
		name: "V",
		list: [1, 2, 3,],
		nested: { deep: true },
	}'
	first := json5.parse(source) or { panic(err) }
	text := json5.encode(first)
	second := json5.parse(text) or { panic(err) }
	assert json5.encode(second) == text
}

fn test_roundtrip_preserves_number_spelling() {
	first := json5.parse('{ a: 0x10, b: .5 }') or { panic(err) }
	assert json5.encode(first) == '{"a":0x10,"b":.5}'
}

fn test_output_is_valid_json() {
	// The default encoder emits JSON, so the strict `json2` module can read it.
	text := json5.encode(json5.parse('{ a: 1, b: ["x"], c: null }') or { panic(err) })
	back := json2.decode[map[string]json2.Any](text) or { panic(err) }
	assert (back['a'] or { json2.Any(0) }).int() == 1
}

fn test_unicode_keys_are_not_unquoted() {
	// A non-identifier key stays quoted even with `unquoted_keys`.
	got := json5.encode_with_opts(json5.parse('{ "a b": 1 }') or { panic(err) },
		json5.EncodeOpts{
			unquoted_keys: true
		})
	assert got == '{"a b":1}'
}
