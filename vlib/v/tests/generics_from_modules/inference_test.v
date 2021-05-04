module main

import v.tests.generics_from_modules.genericmodule

fn test_generic_function_from_another_module() {
	v1 := genericmodule.take<int>(true, 10, 20)
	assert typeof(v1).name == 'int'
	assert v1 == 10
	v2 := genericmodule.take<int>(false, 10, 20)
	assert v2 == 20
}

fn test_generic_type_inference_from_another_module() {
	v1 := genericmodule.take(true, 10, 20)
	assert typeof(v1).name == 'int'
	assert v1 == 10
	v2 := genericmodule.take(false, 10, 20)
	assert v2 == 20
}

fn test_inference_with_strings() {
	v1 := genericmodule.take(true, 'abc', 'def')
	assert typeof(v1).name == 'string'
	assert v1 == 'abc'
	v2 := genericmodule.take(false, 'abc', 'def')
	assert v2 == 'def'
}

fn test_inference_with_f64() {
	v1 := genericmodule.take(true, f64(123), 345)
	assert typeof(v1).name == 'f64'
	assert v1 == 123
	v2 := genericmodule.take(false, f64(123), 345)
	assert v2 == 345
}
