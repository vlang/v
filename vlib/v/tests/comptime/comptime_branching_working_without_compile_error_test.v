type Value = bool | voidptr

pub fn create[T](val T) Value {
	$if T is bool {
		println('bool ${val}')
		return Value(val)
	} $else {
		$compile_error('11')
		println('not bool ${val}')
	}
	return Value(voidptr(123))
}

fn test_calling_generic_function_that_has_inside_a_comptime_compile_error_directive() {
	assert create(true) == Value(true)
}
