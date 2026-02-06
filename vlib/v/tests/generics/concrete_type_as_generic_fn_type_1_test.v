type Fn[T] = fn (T)

type FnReturn[T, R] = fn (T) R

type FnMultiReturn[I, O, R] = fn (I) (O, R)

fn func_fn_concrete() Fn[string] {
	return fn (_s string) {}
}

fn func_fn_dynamic[T]() Fn[T] {
	return fn [T](_t T) {}
}

fn func_fn_return_concrete() FnReturn[string, string] {
	return fn (s string) string {
		return s
	}
}

fn func_fn_return_dynamic[T, R]() FnReturn[T, R] {
	return fn [T, R](t T) R {
		return t.int()
	}
}

fn func_fn_multi_return_concrete() FnMultiReturn[string, string, string] {
	return fn (s string) (string, string) {
		return s[..1], s[1..]
	}
}

fn test_concrete_function_type_as_generic_type() {
	func_fn_concrete()('V')
	func_fn_dynamic[string]()('V')

	func_fn_return_concrete()('V')
	assert func_fn_return_dynamic[string, int]()('100') == 100

	s1, s2 := func_fn_multi_return_concrete()('VLang')

	assert s1 == 'V'
	assert s2 == 'Lang'
}
