import os
import json2
import math

struct Inner {
	label string
	count int
}

struct Simple {
	name  string
	count int
	ratio f64
	flag  bool
}

struct Skipped {
	a int
	b int @[skip]
}

struct Renamed {
	name string @[json: 'real_name']
}

struct Embed {
	label string
}

struct WithEmbed {
	Embed
	name string
}

enum Mode {
	slow
	fast  @[json: 'fast']
}

struct Tagged {
	mode  Mode
	other string
}

struct IntWidths {
	u u8
	i i8
	b u8
	h u16
	w i64
	l u64
	x i64
}

struct WithAny {
	raw  json2.Any
	list []json2.Any
}

struct WithOption {
	present ?Inner
	absent  ?Inner
}

struct UpperString {
mut:
	value string
}

fn (mut u UpperString) from_json_string(raw string) ! {
	if raw == '' {
		return error('an empty string is not a name')
	}
	u.value = raw.to_upper()
}

struct HexNumber {
mut:
	value int
}

fn (mut h HexNumber) from_json_number(raw string) ! {
	digits := raw.trim_left('+-')
	if !digits.starts_with('0x') {
		return error('expected a hexadecimal literal, found `${raw}`')
	}
	mut value := 0
	for ch in digits[2..].runes() {
		value = value * 16 + int(ch - `0`)
	}
	h.value = value
}

struct CustomString {
	name UpperString
}

struct CustomNumber {
	n HexNumber
}

struct CustomWhole {
mut:
	value string
}

fn (mut c CustomWhole) from_json(v json2.Any) {
	c.value = v.as_map()['whatever'] or { json2.null }.str()
}

fn test_decode_simple_struct() {
	got := json2.decode[Simple]('{ name: "V", count: 3, ratio: 1.5, flag: true }', json5: true)!
	assert got == Simple{
		name:  'V'
		count: 3
		ratio: 1.5
		flag:  true
	}
}

fn test_decode_missing_keys_keep_defaults() {
	got := json2.decode[Simple]('{ name: "V" }', json5: true)!
	assert got.name == 'V'
	assert got.count == 0
	assert got.ratio == 0.0
	assert got.flag == false
}

fn test_decode_null_leaves_default() {
	got := json2.decode[Simple]('{ name: null }', json5: true)!
	assert got.name == ''
}

fn test_decode_unquoted_and_quoted_keys_mix() {
	got := json2.decode[Simple]('{ "name": \'a\', \'count\': 2, /* c */ ratio: -0.25, }',
		json5: true
	)!
	assert got.name == 'a'
	assert got.count == 2
	assert got.ratio == -0.25
}

fn test_decode_nested_struct() {
	got := json2.decode[Inner]('{ label: "x", count: 1 }', json5: true)!
	assert got.label == 'x'
	assert got.count == 1
}

fn test_decode_struct_error_on_non_object() {
	json2.decode[Inner]('[1]', json5: true) or {
		assert err.msg() != ''
		return
	}
	assert false, 'expected a type error'
}

fn test_decode_map() {
	got := json2.decode[map[string]string]('{ a: "1", b: "2", }', json5: true)!
	assert got == {
		'a': '1'
		'b': '2'
	}
}

fn test_decode_map_error_on_non_object() {
	json2.decode[map[string]string]('[]', json5: true) or {
		assert err.msg() != ''
		return
	}
	assert false, 'expected a type error'
}

fn test_decode_dynamic_array() {
	got := json2.decode[[]int]('[1, 2, 3]', json5: true)!
	assert got == [1, 2, 3]
}

fn test_decode_empty_array() {
	got := json2.decode[[]string]('[]', json5: true)!
	assert got.len == 0
}

fn test_decode_fixed_array_keeps_length() {
	got := json2.decode[[2]int]('[1, 2, 3]', json5: true)!
	assert got == [1, 2]
	got2 := json2.decode[[3]int]('[1]', json5: true)!
	assert got2 == [1, 0, 0]
}

fn test_decode_array_of_structs() {
	got := json2.decode[[]Inner]('[{ label: "a", count: 1 }, { label: "b", count: 2 }]',
		json5: true
	)!
	assert got.len == 2
	assert got[0].label == 'a'
	assert got[1].count == 2
}

fn test_decode_nested_array() {
	got := json2.decode[[][]int]('[[1, 2], [], [3]]', json5: true)!
	assert got.len == 3
	assert got[0] == [1, 2]
	assert got[1].len == 0
	assert got[2] == [3]
}

fn test_decode_skip_attribute() {
	got := json2.decode[Skipped]('{ a: 1, b: 2 }', json5: true)!
	assert got.a == 1
	assert got.b == 0
}

fn test_decode_json5_attribute() {
	got := json2.decode[Renamed]('{ real_name: "v" }', json5: true)!
	assert got.name == 'v'
}

fn test_decode_embedded_struct_is_flattened() {
	got := json2.decode[WithEmbed]('{ label: "l", name: "n" }', json5: true)!
	assert got.label == 'l'
	assert got.name == 'n'
}

fn test_decode_json5_attribute_on_enum() {
	got := json2.decode[Tagged]('{ mode: "fast", other: "keep" }', json5: true)!
	assert got.mode == .fast
	assert got.other == 'keep'
}

fn test_decode_enum_by_name_and_number() {
	assert json2.decode[Tagged]('{ mode: "fast" }', json5: true)!.mode == .fast
	assert json2.decode[Tagged]('{ mode: 1 }', json5: true)!.mode == .fast
	assert json2.decode[Tagged]('{ mode: 0 }', json5: true)!.mode == .slow
}

fn test_decode_enum_error() {
	json2.decode[Tagged]('{ mode: "nope" }', json5: true) or {
		assert err.msg().contains('enum')
		return
	}
	assert false, 'expected an enum error'
}

fn test_decode_all_integer_widths() {
	got := json2.decode[IntWidths]('{ u: 1, i: -1, b: 2, h: 3, w: 4, l: 5, x: 6 }', json5: true)!
	assert got.u == u8(1)
	assert got.i == i8(-1)
	assert got.b == u8(2)
	assert got.h == u16(3)
	assert got.w == i64(4)
	assert got.l == u64(5)
	assert got.x == i64(6)
}

fn test_decode_narrow_int_rejects_overflow() {
	json2.decode[IntWidths]('{ u: 300 }', json5: true) or {
		assert err.msg() != ''
		return
	}
	assert false, 'expected a range error'
}

fn test_decode_negative_to_unsigned_rejected() {
	json2.decode[IntWidths]('{ b: -1 }', json5: true) or {
		assert err.msg() != ''
		return
	}
	assert false, 'expected a range error'
}

fn test_decode_hex_and_floats() {
	got := json2.decode[Simple]('{ count: 0x10, ratio: .5 }', json5: true)!
	assert got.count == 16
	assert got.ratio == 0.5
}

fn test_decode_large_hexadecimal_floats() {
	// Check exact bits at rounding ties, including nonzero discarded digits.
	cases := {
		'0x10000000000000000':                   u64(0x43f0000000000000)
		'0x100000000000007ff':                   u64(0x43f0000000000000)
		'0x10000000000000800':                   u64(0x43f0000000000000)
		'0x10000000000000801':                   u64(0x43f0000000000001)
		'0x10000000000001800':                   u64(0x43f0000000000002)
		'0x20000000000001001':                   u64(0x4400000000000001)
		'0x40000000000002001':                   u64(0x4410000000000001)
		'0x80000000000004001':                   u64(0x4420000000000001)
		'0xabcdefabcdefabcdef':                  u64(0x446579bdf579bdf5)
		'+0X10000000000000000':                  u64(0x43f0000000000000)
		'-0x10000000000000801':                  u64(0xc3f0000000000001)
		'0x00000000000000000010000000000000000': u64(0x43f0000000000000)
		'0xffffffffffffffff':                    u64(0x43f0000000000000)
		'0x0':                                   u64(0)
		'-0x000':                                u64(0x8000000000000000)
	}
	for text, expected in cases {
		assert math.f64_bits(json2.decode[f64](text, json5: true)!) == expected, text
		assert math.f64_bits(json2.parse(text, json5: true)!.f64()) == expected, text
	}
	assert json2.decode[f32]('0x10000000000000000', json5: true)! == f32(18446744073709551616.0)
	got := json2.decode[Simple]('{ratio: 0x10000000000000000}', json5: true)!
	assert got.ratio == 18446744073709551616.0
	doc := json2.parse_text('{values: [0x10000000000000000]}', json5: true)!
	assert doc.value('values[0]').f64() == 18446744073709551616.0
}

fn test_decode_hexadecimal_float_overflow() {
	// At 2^1024 - 2^970, nearest-even rounding overflows to infinity.
	below := '0x' + 'f'.repeat(13) + 'b' + 'f'.repeat(242)
	tie := '0x' + 'f'.repeat(13) + 'c' + '0'.repeat(242)
	assert math.f64_bits(json2.decode[f64](below, json5: true)!) == u64(0x7fefffffffffffff)
	for text in [tie, '0x1' + '0'.repeat(256), '0x' + 'f'.repeat(1024)] {
		assert math.is_inf(json2.decode[f64](text, json5: true)!, 1)
		assert math.is_inf(json2.decode[f64]('-' + text, json5: true)!, -1)
		assert math.is_inf(json2.parse(text, json5: true)!.f64(), 1)
	}
	assert json2.decode[f64]('0x' + '0'.repeat(1024), json5: true)! == 0.0
}

fn test_decode_hexadecimal_integer_range_checks() {
	assert json2.decode[u64]('0xffffffffffffffff', json5: true)! == u64(0xffffffffffffffff)
	assert json2.decode[i64]('-0x8000000000000000', json5: true)! == i64(-9223372036854775807 - 1)
	for text in ['0x10000000000000000', '-0x10000000000000000'] {
		json2.decode[u64](text, json5: true) or { continue }
		assert false, 'expected unsigned integer overflow: ${text}'
	}
	for text in ['0x8000000000000000', '-0x8000000000000001', '0x10000000000000000'] {
		json2.decode[i64](text, json5: true) or { continue }
		assert false, 'expected signed integer overflow: ${text}'
	}
}

fn test_decode_infinity_to_float() {
	got := json2.decode[Simple]('{ ratio: Infinity }', json5: true)!
	assert got.ratio > 0.0
}

fn test_decode_any_field() {
	got := json2.decode[WithAny]('{ raw: { a: 1 }, list: [1, 2] }', json5: true)!
	assert got.raw.as_map()['a'] or { json2.null }.int() == 1
	assert got.list.len == 2
}

fn test_decode_to_any() {
	got := json2.decode[json2.Any]('{ a: 1 }', json5: true)!
	assert got.as_map()['a'] or { json2.null }.int() == 1
}

fn test_decode_top_level_scalars() {
	assert json2.decode[string]('"s"', json5: true)! == 's'
	assert json2.decode[int]('42', json5: true)! == 42
	assert json2.decode[bool]('true', json5: true)!
	assert json2.decode[f64]('1.5', json5: true)! == 1.5
}

fn test_decode_option_field() {
	got := json2.decode[WithOption]('{ present: { label: "x" }, absent: null }', json5: true)!
	assert got.present != none
	assert (got.present or { Inner{} }).label == 'x'
	assert got.absent == none
}

fn test_decode_custom_string_hook() {
	got := json2.decode[CustomString]('{ name: "ab" }', json5: true)!
	assert got.name.value == 'AB'
}

fn test_decode_custom_string_hook_error() {
	json2.decode[CustomString]('{ name: "" }', json5: true) or { return }
	assert false, 'expected the custom hook to fail'
}

fn test_decode_custom_number_hook() {
	// The hook receives the literal source text of the number, so a hexadecimal
	// literal reaches it intact.
	got := json2.decode[CustomNumber]('{ n: 0x10 }', json5: true)!
	assert got.n.value == 16
}

fn test_decode_custom_whole_value_hook() {
	got := json2.decode[CustomWhole]('{ whatever: "raw" }', json5: true)!
	assert got.value == 'raw'
}

fn test_decode_doc_method() {
	doc := json2.parse_text('{ name: "V" }', json5: true)!
	assert doc.decode[Simple]()!.name == 'V'
}

fn test_decode_file() {
	path := os.temp_dir() + '/json5_decode_test_' + int(os.getpid()).str() + '.json5'
	os.write_file(path, '{ name: "from file" }') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	assert json2.decode_file[Simple](path, json5: true)!.name == 'from file'
}

fn test_reflect_leaves_unmatched_fields() {
	doc := json2.parse_text('{ name: "V", count: "not a number" }', json5: true)!
	got := doc.reflect[Simple]()
	assert got.name == 'V'
	assert got.count == 0
}

fn test_reflect_embedded() {
	doc := json2.parse_text('{ label: "l", name: "n" }', json5: true)!
	got := doc.reflect[WithEmbed]()
	assert got.label == 'l'
	assert got.name == 'n'
}
