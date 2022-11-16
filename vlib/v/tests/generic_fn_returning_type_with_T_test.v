struct BuildData {
	x int
}

struct Tensor {
	x int
}

fn new_tensor<T>(data BuildData) Tensor {
	println('data: ${data}')
	x := T(data.x)
	println(x)
	return Tensor{data.x}
}

fn test_generic_function_returning_type_starting_with_t() {
	ft := new_tensor<f64>(x: 123)
	println(ft)
	assert typeof(ft).name == 'Tensor'
	assert '${ft}' == 'Tensor{\n    x: 123\n}'
	//
	it := new_tensor<int>(x: 456)
	println(it)
	assert typeof(it).name == 'Tensor'
	assert '${it}' == 'Tensor{\n    x: 456\n}'
}

// the following verifies that returning a generic type T
// works at the same time as returning a type starting with T
fn new_t<T>(o T) T {
	x := T(o)
	return x
}

fn test_generic_function_returning_t_type() {
	f := new_t<f64>(1.23)
	i := new_t<int>(456)
	assert '${f}' == '1.23'
	assert '${i}' == '456'
}
