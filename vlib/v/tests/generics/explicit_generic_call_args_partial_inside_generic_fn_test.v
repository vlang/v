// Regression coverage for explicit type arguments that only pin some of a callee's generic
// parameters, on a call made from inside another generic function.
//
// Positions fixed by the explicit list must survive in every specialization of the caller, even
// when the remaining positions are inferable from a value argument and the caller reuses one of the
// callee's generic parameter names.

fn pick[A, B](_b B) string {
	return '${typeof[A]().name}/${typeof[B]().name}'
}

// `A` deliberately shadows the callee's first generic parameter name.
fn pick_caller[A](value A) string {
	return pick[string, int](value)
}

fn test_explicit_args_survive_when_another_param_is_inferable() {
	assert pick[string, int](1) == 'string/int'
	assert pick_caller(1) == 'string/int'
}

struct Holder[T] {
	value T
}

fn (_h Holder[T]) tagged[U](_u U) string {
	return '${typeof[T]().name}/${typeof[U]().name}'
}

// Method-level generics are the one place V accepts a partial explicit list: the receiver's `T` is
// inferred from the receiver and only the trailing arguments are spelled out.
fn method_caller[T](value T) string {
	h := Holder[string]{}
	return h.tagged[int](1) + ' ' + h.tagged(value)
}

fn test_partial_explicit_method_generics_survive_inside_generic_fn() {
	h := Holder[string]{}
	assert h.tagged[int](1) == 'string/int'
	assert method_caller(1.5) == 'string/int string/f64'
	assert method_caller(7) == 'string/int string/int'
}
