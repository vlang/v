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

fn decode_v3_generic[T](source string) !T {
	return json.decode(T, source)
}

fn test_json_decode_v3_required_and_type_errors() {
	mut missing_failed := false
	_ := json.decode(DecodeV3Required, '{"age":1}') or {
		missing_failed = true
		assert err.msg() == "expected field 'name' is missing"
		DecodeV3Required{}
	}
	assert missing_failed
	mut type_failed := false
	_ := json.decode(DecodeV3Required, '{"name":1}') or {
		type_failed = true
		assert err.msg() == "type mismatch for field 'name', expecting `string` type, got: 1"
		DecodeV3Required{}
	}
	assert type_failed
}

fn test_json_decode_v3_recursive_pointer_and_generic_fixed_array() {
	node := json.decode(DecodeV3Node, '{"value":1,"next":{"value":2,"next":{"value":3}}}')!
	assert node.next.next.value == 3
	assert node.next.next.next == unsafe { nil }

	box := decode_v3_generic[DecodeV3Box[int]]('{"values":[4,5],"hidden":"ignored"}')!
	assert box.values == [4, 5]!
	assert box.hidden == ''
}
