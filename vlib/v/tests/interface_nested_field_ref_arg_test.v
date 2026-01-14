module main

import arrays

interface Value {}

struct Inner {
	data string
}

struct Outer {
	inner Inner
}

struct WithArray {
	items []string
}

fn nested_struct_field(p Outer) []Value {
	mut params := []Value{}
	if p.inner.data != '' {
		params = arrays.concat(params, p.inner.data)
	}
	return params
}

fn array_index_field(p WithArray) []Value {
	mut params := []Value{}
	if p.items.len > 0 {
		params = arrays.concat(params, p.items[0])
	}
	return params
}

fn test_interface_nested_field_ref_arg() {
	outer := Outer{
		inner: Inner{
			data: 'nested data'
		}
	}
	params := nested_struct_field(outer)
	assert params == [Value('nested data')]
}

fn test_interface_array_index_ref_arg() {
	wa := WithArray{
		items: ['first', 'second']
	}
	params := array_index_field(wa)
	assert params == [Value('first')]
}
