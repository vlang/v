// vtest vflags: -w
import json

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

type DecodeV3DeepI64 = [][][][][][][][][][][][][]i64

fn decode_v3_generic[T](source string) !T {
	return json.decode(T, source)
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
