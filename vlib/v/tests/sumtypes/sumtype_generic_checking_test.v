type Sumtype = string | int
type DemoType[T] = T | fn () T

fn generic_fn[T]() ?T {
	a := Sumtype('123')
	if a is T {
		return a
	}

	b := Sumtype(123)
	if b is T {
		return b
	}

	return none
}

fn test_main() {
	assert '${generic_fn[string]()}' == "Option('123')"
	assert '${generic_fn[int]()}' == 'Option(123)'
}

fn is_function_type[T](data DemoType[T]) bool {
	_ = data
	$if T is $function {
		return true
	} $else {
		return false
	}
}

fn test_generic_sumtype_function_inference() {
	assert is_function_type[string]('test string') == false
	assert is_function_type[string](fn () string {
		return 'function test string'
	}) == false

	_ = is_function_type('test string')
	_ = is_function_type(fn () string {
		return 'function test string'
	})
}
