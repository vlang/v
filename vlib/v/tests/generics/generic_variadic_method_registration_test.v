interface Param {}

struct Value[T] {
mut:
	n T
}

struct Gate[T] {
	target T
}

fn new_gate[T](n T) &Gate[T] { return &Gate[T]{ target: n } }

fn (g &Gate[T]) cache(mut result Value[T], args ...Param) ! {
	a := args[0]
	match a {
		Value[T] { result.n = a.n }
		else { return error('bad') }
	}
}

fn apply[T](input &Value[T]) !&Value[T] {
	mut result := &Value[T]{}
	gate := new_gate[T](input.n)
	gate.cache(mut result, input)!
	return result
}

fn test_generic_variadic_method() {
	assert apply(&Value[f64]{ n: 42 })!.n == 42
}
