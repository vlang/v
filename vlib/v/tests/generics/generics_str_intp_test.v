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

struct Foo[T] {
	bar T
}

fn (b Foo[T]) str() string {
	return '${b.bar:b}/${b.bar:o}/${b.bar:d}/${b.bar:x}'
}

fn generic_int_format[T](value T) string {
	return '${value:b}/${value:o}/${value:d}/${value:x}'
}

fn generic_int_padded_format[T](value T) string {
	return '${value:08x}/${value:+d}'
}

// vfmt off
fn generic_default_format[T](value T) string {
	return '${value:_}'
}
// vfmt on

fn test_generic_string_interpolation_explicit_int_formats() {
	assert generic_int_format[int](42) == '101010/52/42/2a'
	assert generic_int_padded_format[int](42) == '0000002a/+42'
	assert generic_default_format[int](42) == '42'
	assert generic_default_format[string]('ok') == 'ok'

	baz := Foo[int]{
		bar: 42
	}
	assert baz.str() == '101010/52/42/2a'

	baz_inferred := Foo{
		bar: 56
	}
	assert baz_inferred.str() == '111000/70/56/38'
}
