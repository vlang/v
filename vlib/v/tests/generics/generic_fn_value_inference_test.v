import math

fn apply_f64(value f64, f fn (f64) f64) f64 {
	return f(value)
}

fn test_generic_fn_value_is_inferred_from_expected_fn_type() {
	assert apply_f64(-1.25, math.abs) == 1.25
}

struct GenericFnArgFoo {}

fn generic_fn_arg_handler[T](x T) int {
	_ := x
	return 41
}

fn generic_fn_arg_run[T](h fn (T) int) int {
	return h(T{}) + 1
}

fn test_generic_fn_value_is_monomorphized_from_generic_call_arg() {
	assert generic_fn_arg_run[GenericFnArgFoo](generic_fn_arg_handler) == 42
}
