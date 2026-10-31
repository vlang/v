// Casting `[]T` to a sum type that has a `[]Sum` variant, where `T` is a variant
// of `Sum`, converts each element, as `json2.Any([]u8)` does.
import x.json2

type Value = []Value | int | string | u8

fn test_cast_array_of_variants_to_recursive_sum() {
	bytes := [u8(1), 2, 3]
	value := Value(bytes.clone())
	assert value is []Value
	if value is []Value {
		assert value.len == 3
		assert value[0] is u8
		assert value[2] as u8 == 3
	}
	words := ['a', 'b']
	wrapped := Value(words)
	if wrapped is []Value {
		assert wrapped[1] as string == 'b'
	} else {
		assert false
	}
}

fn test_cast_array_literal_keeps_its_element_type() {
	value := Value([u8(1), 2])
	if value is []Value {
		assert value[0] is u8
		assert value[1] is u8
	} else {
		assert false
	}
}

fn test_cast_byte_array_to_json2_any() {
	shared_key := ?[]u8([u8(1), 2, 3])
	mut object := map[string]json2.Any{}
	if key := shared_key {
		object['sharedKey'] = json2.Any(key.clone())
	}
	assert json2.encode(object) == '{"sharedKey":[1,2,3]}'
	any := object['sharedKey'] or { json2.Any('') }
	if any is []json2.Any {
		assert any.len == 3
		assert any[1] as u8 == 2
	} else {
		assert false
	}
}
