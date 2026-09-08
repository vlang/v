// vtest vflags: -w
@[has_globals]
module main

import json
import time

struct DecodeV3Required {
	name string @[required]
	age  int
}

struct DecodeV3Node {
	value int
	next  &DecodeV3Node
}

struct DecodeV3Box[T] {
	values [2]T
	hidden string @[skip]
}

struct DecodeV3Plain {
	name string
	age  int
}

struct DecodeV3SkippedDefault {
	visible string
	hidden  int = 7 @[skip]
}

struct DecodeV3DefaultElem {
	value int = 7
}

struct DecodeV3FixedDefaults {
mut:
	maps  [2]map[string]int
	elems [2]DecodeV3DefaultElem
}

struct DecodeV3Event {
	at time.Time
}

struct DecodeV3ThrowawayFirst {
	value int = decode_v3_sum_default_value()
}

struct DecodeV3SelectedVariant {
	name string
}

type DecodeV3NoThrowawaySum = DecodeV3ThrowawayFirst | DecodeV3SelectedVariant

__global decode_v3_sum_default_calls int

type DecodeV3DeepI64 = [][][][][][][][][][][][][]i64

fn decode_v3_generic[T](source string) !T {
	return json.decode(T, source)
}

fn decode_v3_sum_default_value() int {
	decode_v3_sum_default_calls++
	return 7
}

fn test_json_decode_v3_required_and_type_errors() {
	mut missing_failed := false
	_ := json.decode(DecodeV3Required, '{"age":1}') or {
		missing_failed = true
		assert err.msg() == "expected field 'name' is missing"
		DecodeV3Required{
			name: ''
		}
	}
	assert missing_failed
	mut type_failed := false
	_ := json.decode(DecodeV3Required, '{"name":1}') or {
		type_failed = true
		assert err.msg() == "type mismatch for field 'name', expecting `string` type, got: 1"
		DecodeV3Required{
			name: ''
		}
	}
	assert type_failed
}

fn test_json_decode_v3_struct_rejects_non_object_roots() {
	for source in ['[]', 'null'] {
		mut failed := false
		_ := json.decode(DecodeV3Plain, source) or {
			failed = true
			assert err.msg().starts_with('Json element is not an object:')
			DecodeV3Plain{}
		}
		assert failed
	}
}

fn test_json_decode_v3_skipped_fields_keep_struct_defaults() {
	decoded := json.decode(DecodeV3SkippedDefault, '{"visible":"ok","hidden":99}')!
	assert decoded.visible == 'ok'
	assert decoded.hidden == 7
}

fn test_json_decode_v3_fixed_arrays_keep_recursive_defaults() {
	mut omitted := json.decode(DecodeV3FixedDefaults, '{}')!
	omitted.maps[0]['first'] = 1
	omitted.maps[1]['second'] = 2
	assert omitted.maps[0]['first'] == 1
	assert omitted.maps[1]['second'] == 2
	assert omitted.elems[0].value == 7
	assert omitted.elems[1].value == 7

	mut partial := json.decode(DecodeV3FixedDefaults,
		'{"maps":[{"decoded":3}],"elems":[{"value":4}]}')!
	partial.maps[1]['defaulted'] = 5
	assert partial.maps[0]['decoded'] == 3
	assert partial.maps[1]['defaulted'] == 5
	assert partial.elems[0].value == 4
	assert partial.elems[1].value == 7

	mut nulls := json.decode(DecodeV3FixedDefaults, '{"maps":null,"elems":null}')!
	nulls.maps[0]['ready'] = 6
	assert nulls.maps[0]['ready'] == 6
	assert nulls.elems[0].value == 7
}

fn test_json_decode_v3_propagates_invalid_time_errors() {
	mut failed := false
	mut message := ''
	_ := json.decode(DecodeV3Event, '{"at":"not-a-time"}') or {
		failed = true
		message = err.msg()
		DecodeV3Event{}
	}
	assert failed
	assert message.contains('Expected iso8601/rfc3339/unix time')

	valid := json.decode(DecodeV3Event, '{"at":"2001-01-01"}')!
	assert valid.at.str() == '2001-01-01 00:00:00'
}

fn test_json_decode_v3_root_time_roundtrip() {
	original := time.new(year: 2020, month: 12, day: 22, hour: 7, minute: 23)
	encoded := json.encode(original)
	decoded := json.decode(time.Time, encoded)!
	assert decoded.unix() == original.unix()

	mut message := ''
	_ := json.decode(time.Time, '"not-a-time"') or {
		message = err.msg()
		time.Time{}
	}
	assert message.contains('Expected iso8601/rfc3339/unix time')
}

fn test_json_decode_v3_sum_constructs_only_selected_variant() {
	decode_v3_sum_default_calls = 0
	decoded := json.decode(DecodeV3NoThrowawaySum,
		'{"name":"selected","_type":"DecodeV3SelectedVariant"}')!
	assert (decoded as DecodeV3SelectedVariant).name == 'selected'
	// Result unwrapping may construct one default sum value. The decoder must not
	// construct another first-variant payload before selecting the second variant.
	assert decode_v3_sum_default_calls <= 1
}

fn test_json_decode_v3_recursive_pointer_and_generic_fixed_array() {
	node := json.decode(DecodeV3Node, '{"value":1,"next":{"value":2,"next":{"value":3}}}')!
	assert node.next.next.value == 3
	assert node.next.next.next == unsafe { nil }

	box := decode_v3_generic[DecodeV3Box[int]]('{"values":[4,5],"hidden":"ignored"}')!
	assert box.values == [4, 5]!
	assert box.hidden == ''
}

fn test_json_decode_v3_recursive_pointer_has_no_static_depth_cap() {
	mut source := 'null'
	for i := 47; i >= 0; i-- {
		source = '{"value":${i},"next":${source}}'
	}
	node := decode_v3_generic[DecodeV3Node](source)!
	assert node.value == 0
	mut expected := 1
	mut cursor := node.next
	for cursor != unsafe { nil } {
		assert cursor.value == expected
		expected++
		cursor = cursor.next
	}
	assert expected == 48
}

fn test_json_decode_v3_preserves_deep_exact_integer() {
	mut source := '9007199254740993'
	for _ in 0 .. 13 {
		source = '[${source}]'
	}
	decoded := decode_v3_generic[DecodeV3DeepI64](source)!
	assert decoded[0][0][0][0][0][0][0][0][0][0][0][0][0] == i64(9007199254740993)
}
