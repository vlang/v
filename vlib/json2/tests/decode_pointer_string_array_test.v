import json2

struct StringArrayInner {
	inner  string
	deeper []string
}

struct StringArrayDefaultInner {
	deeper []string
	count  int = 7
}

struct StringArrayDefaultOuter {
	inner &StringArrayDefaultInner = unsafe { nil }
}

struct StringArrayPointerOuter {
	inner &StringArrayInner = unsafe { nil }
}

struct StringArrayValueOuter {
	inner StringArrayInner
}

fn test_decode_string_array_through_struct_pointer() {
	document := '{"inner":{"inner":"x","deeper":["a","b","escaped\\nvalue",""]}}'
	pointer := json2.decode[StringArrayPointerOuter](document)!
	value := json2.decode[StringArrayValueOuter](document)!
	assert pointer.inner.inner == 'x'
	assert pointer.inner.deeper == ['a', 'b', 'escaped\nvalue', '']
	assert pointer.inner.deeper == value.inner.deeper
	assert json2.encode(pointer) == document
}

fn test_decode_string_array_through_top_level_and_array_pointers() {
	inner := json2.decode[&StringArrayInner]('{"deeper":["a","b"]}')!
	assert inner.deeper == ['a', 'b']
	values := json2.decode[[]&StringArrayInner]('[{"deeper":["x"]},{"deeper":["y","z"]}]')!
	assert values[0].deeper == ['x']
	assert values[1].deeper == ['y', 'z']
	array := json2.decode[&[]string]('["a","b"]')!
	assert *array == ['a', 'b']
}

fn test_decode_pointer_struct_retains_field_defaults() {
	value := json2.decode[StringArrayDefaultOuter]('{"inner":{"deeper":["a"]}}')!
	assert value.inner.deeper == ['a']
	assert value.inner.count == 7
}
