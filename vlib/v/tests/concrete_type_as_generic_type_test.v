type Fn = fn (T)

type FnReturn = fn (T) R

fn func_fn_concrete() Fn[string] {
	return fn (_s string) {}
}

fn func_fn_dynamic[T]() Fn[T] {
	return fn [T](_t T) {}
}

// FIXME: FnReturn[string, string] fails to stencil
// fn func_fn_return_concrete() FnReturn[string, string] {
// 	return fn (s string) string {
// 		return s
// 	}
// }

fn func_fn_return_dynamic[T, R]() FnReturn[T, R] {
	return fn [T, R](t T) R {
		return t.int()
	}
}

fn test_concrete_function_type_as_generic_type() {
	func_fn_concrete()('V')
	func_fn_dynamic[string]()('V')

	assert func_fn_return_dynamic[string, int]()('100') == 100
}
