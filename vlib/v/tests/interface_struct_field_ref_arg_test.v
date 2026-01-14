module main

import arrays

interface Value {}

struct SomeStruct {
	s string
}

fn generate_params(p SomeStruct) (string, []Value) {
	mut conditions := []string{}
	mut params := []Value{}

	if p.s != '' {
		conditions = arrays.concat(conditions, 'email = ?')
		params = arrays.concat(params, p.s)
	}

	return conditions.join('AND '), params
}

fn test_interface_struct_field_ref_arg() {
	str, params := generate_params(SomeStruct{
		s: 'some string'
	})
	assert str == 'email = ?'
	assert params == [Value('some string')]
}
