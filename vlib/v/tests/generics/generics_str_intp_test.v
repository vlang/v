module main

import strings

pub struct MyStruct[T] {
pub mut:
	result T
}

pub fn (it MyStruct[T]) indent_str[T]() string {
	mut res := strings.new_builder(32)
	res.write_string('${it.result}')
	return res.str()
}

fn test_generics_str_intp() {
	x := MyStruct[int]{
		result: 100
	}

	y := MyStruct[string]{
		result: 'hello'
	}

	assert x.indent_str() == '100'
	assert y.indent_str() == 'hello'
}
