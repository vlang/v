import os
import x.json5

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
	name string @[json5: 'real_name']
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
	fast  @[json5: 'fast']
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
	raw  json5.Any
	list []json5.Any
}

struct WithOption {
	present ?Inner
	absent  ?Inner
}

struct UpperString {
mut:
	value string
}

fn (mut u UpperString) from_json5_string(raw string) ! {
	if raw == '' {
		return error('an empty string is not a name')
	}
	u.value = raw.to_upper()
}

struct HexNumber {
mut:
	value int
}

fn (mut h HexNumber) from_json5_number(raw string) ! {
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

fn (mut c CustomWhole) from_json5(v json5.Any) {
	c.value = v.as_map()['whatever'] or { json5.null }.string()
}

fn test_decode_simple_struct() {
	got := json5.decode[Simple]('{ name: "V", count: 3, ratio: 1.5, flag: true }')!
	assert got == Simple{
		name:  'V'
		count: 3
		ratio: 1.5
		flag:  true
	}
}

fn test_decode_missing_keys_keep_defaults() {
	got := json5.decode[Simple]('{ name: "V" }')!
	assert got.name == 'V'
	assert got.count == 0
	assert got.ratio == 0.0
	assert got.flag == false
}

fn test_decode_null_leaves_default() {
	got := json5.decode[Simple]('{ name: null }')!
	assert got.name == ''
}

fn test_decode_unquoted_and_quoted_keys_mix() {
	got := json5.decode[Simple]('{ "name": \'a\', \'count\': 2, /* c */ ratio: -0.25, }')!
	assert got.name == 'a'
	assert got.count == 2
	assert got.ratio == -0.25
}

fn test_decode_nested_struct() {
	got := json5.decode[Inner]('{ label: "x", count: 1 }')!
	assert got.label == 'x'
	assert got.count == 1
}

fn test_decode_struct_error_on_non_object() {
	json5.decode[Inner]('[1]') or {
		assert err is json5.TypeError
		return
	}
	assert false, 'expected a type error'
}

fn test_decode_map() {
	got := json5.decode[map[string]string]('{ a: "1", b: "2", }')!
	assert got == {
		'a': '1'
		'b': '2'
	}
}

fn test_decode_map_error_on_non_object() {
	json5.decode[map[string]string]('[]') or {
		assert err is json5.TypeError
		return
	}
	assert false, 'expected a type error'
}

fn test_decode_dynamic_array() {
	got := json5.decode[[]int]('[1, 2, 3]')!
	assert got == [1, 2, 3]
}

fn test_decode_empty_array() {
	got := json5.decode[[]string]('[]')!
	assert got.len == 0
}

fn test_decode_fixed_array_keeps_length() {
	got := json5.decode[[2]int]('[1, 2, 3]')!
	assert got == [1, 2]
	got2 := json5.decode[[3]int]('[1]')!
	assert got2 == [1, 0, 0]
}

fn test_decode_array_of_structs() {
	got := json5.decode[[]Inner]('[{ label: "a", count: 1 }, { label: "b", count: 2 }]')!
	assert got.len == 2
	assert got[0].label == 'a'
	assert got[1].count == 2
}

fn test_decode_nested_array() {
	got := json5.decode[[][]int]('[[1, 2], [], [3]]')!
	assert got.len == 3
	assert got[0] == [1, 2]
	assert got[1].len == 0
	assert got[2] == [3]
}

fn test_decode_skip_attribute() {
	got := json5.decode[Skipped]('{ a: 1, b: 2 }')!
	assert got.a == 1
	assert got.b == 0
}

fn test_decode_json5_attribute() {
	got := json5.decode[Renamed]('{ real_name: "v" }')!
	assert got.name == 'v'
}

fn test_decode_embedded_struct_is_flattened() {
	got := json5.decode[WithEmbed]('{ label: "l", name: "n" }')!
	assert got.label == 'l'
	assert got.name == 'n'
}

fn test_decode_json5_attribute_on_enum() {
	got := json5.decode[Tagged]('{ mode: "fast", other: "keep" }')!
	assert got.mode == .fast
	assert got.other == 'keep'
}

fn test_decode_enum_by_name_and_number() {
	assert json5.decode[Tagged]('{ mode: "fast" }')!.mode == .fast
	assert json5.decode[Tagged]('{ mode: 1 }')!.mode == .fast
	assert json5.decode[Tagged]('{ mode: 0 }')!.mode == .slow
}

fn test_decode_enum_error() {
	json5.decode[Tagged]('{ mode: "nope" }') or {
		assert err is json5.EnumError
		return
	}
	assert false, 'expected an enum error'
}

fn test_decode_all_integer_widths() {
	got := json5.decode[IntWidths]('{ u: 1, i: -1, b: 2, h: 3, w: 4, l: 5, x: 6 }')!
	assert got.u == u8(1)
	assert got.i == i8(-1)
	assert got.b == u8(2)
	assert got.h == u16(3)
	assert got.w == i64(4)
	assert got.l == u64(5)
	assert got.x == i64(6)
}

fn test_decode_narrow_int_rejects_overflow() {
	json5.decode[IntWidths]('{ u: 300 }') or {
		assert err is json5.TypeError
		return
	}
	assert false, 'expected a range error'
}

fn test_decode_negative_to_unsigned_rejected() {
	json5.decode[IntWidths]('{ b: -1 }') or {
		assert err is json5.TypeError
		return
	}
	assert false, 'expected a range error'
}

fn test_decode_hex_and_floats() {
	got := json5.decode[Simple]('{ count: 0x10, ratio: .5 }')!
	assert got.count == 16
	assert got.ratio == 0.5
}

fn test_decode_infinity_to_float() {
	got := json5.decode[Simple]('{ ratio: Infinity }')!
	assert got.ratio > 0.0
}

fn test_decode_any_field() {
	got := json5.decode[WithAny]('{ raw: { a: 1 }, list: [1, 2] }')!
	assert got.raw.as_map()['a'] or { json5.null }.int() == 1
	assert got.list.len == 2
}

fn test_decode_to_any() {
	got := json5.decode[json5.Any]('{ a: 1 }')!
	assert got.as_map()['a'] or { json5.null }.int() == 1
}

fn test_decode_top_level_scalars() {
	assert json5.decode[string]('"s"')! == 's'
	assert json5.decode[int]('42')! == 42
	assert json5.decode[bool]('true')!
	assert json5.decode[f64]('1.5')! == 1.5
}

fn test_decode_option_field() {
	got := json5.decode[WithOption]('{ present: { label: "x" }, absent: null }')!
	assert got.present != none
	assert (got.present or { Inner{} }).label == 'x'
	assert got.absent == none
}

fn test_decode_custom_string_hook() {
	got := json5.decode[CustomString]('{ name: "ab" }')!
	assert got.name.value == 'AB'
}

fn test_decode_custom_string_hook_error() {
	json5.decode[CustomString]('{ name: "" }') or { return }
	assert false, 'expected the custom hook to fail'
}

fn test_decode_custom_number_hook() {
	// The hook receives the literal source text of the number, so a hexadecimal
	// literal reaches it intact.
	got := json5.decode[CustomNumber]('{ n: 0x10 }')!
	assert got.n.value == 16
}

fn test_decode_custom_whole_value_hook() {
	got := json5.decode[CustomWhole]('{ whatever: "raw" }')!
	assert got.value == 'raw'
}

fn test_decode_doc_method() {
	doc := json5.parse_text('{ name: "V" }')!
	assert doc.decode[Simple]()!.name == 'V'
}

fn test_decode_file() {
	path := os.temp_dir() + '/json5_decode_test_' + int(os.getpid()).str() + '.json5'
	os.write_file(path, '{ name: "from file" }') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	assert json5.decode_file[Simple](path)!.name == 'from file'
}

fn test_reflect_leaves_unmatched_fields() {
	doc := json5.parse_text('{ name: "V", count: "not a number" }')!
	got := doc.reflect[Simple]()
	assert got.name == 'V'
	assert got.count == 0
}

fn test_reflect_embedded() {
	doc := json5.parse_text('{ label: "l", name: "n" }')!
	got := doc.reflect[WithEmbed]()
	assert got.label == 'l'
	assert got.name == 'n'
}
