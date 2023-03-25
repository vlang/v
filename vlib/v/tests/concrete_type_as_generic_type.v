type Fn[T] = fn (T)
type FnReturn[T, R] = fn (T) R

fn func_fn_concrete() Fn[string] {
	return fn (_s string) {}
}

fn func_fn_dynamic[T]() Fn[T] {
	return fn (_t T) {}
}

// FIXME
// fn func_fn_return_concrete() FnReturn[string, string] {
// 	return fn (s string) string {
// 		return s
// 	}
// }

fn func_fn_return_dynamic_1[T]() FnReturn[T, T] {
	return fn (t T) T {
		return t
	}
}

fn func_fn_return_dynamic_2[T, R](r R) FnReturn[T, R] {
	return fn [r] (_t ) string {
		return r
	}
}
