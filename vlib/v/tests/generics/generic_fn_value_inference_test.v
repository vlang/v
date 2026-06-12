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

struct GenericFnArgRunner {}

fn (r GenericFnArgRunner) run[T](h fn (T) int) int {
	_ := r
	return h(T{}) + 1
}

fn test_generic_fn_value_is_monomorphized_from_generic_method_call_arg() {
	assert GenericFnArgRunner{}.run[GenericFnArgFoo](generic_fn_arg_handler) == 42
}

fn generic_fn_arg_named_handler[U](x U) int {
	_ := x
	return 41
}

fn (r GenericFnArgRunner) run_inferred[T](x T, h fn (T) int) int {
	_ := r
	return h(x) + 1
}

fn test_generic_fn_value_is_monomorphized_from_inferred_generic_method_call_arg() {
	assert GenericFnArgRunner{}.run_inferred(GenericFnArgFoo{}, generic_fn_arg_named_handler) == 42
}

fn (r GenericFnArgRunner) run_variadic[T](handlers ...fn (T) int) int {
	_ := r
	return handlers[0](T{}) + 1
}

fn test_generic_fn_value_is_monomorphized_from_variadic_generic_method_call_arg() {
	assert GenericFnArgRunner{}.run_variadic[GenericFnArgFoo](generic_fn_arg_named_handler) == 42
}

fn (r GenericFnArgRunner) run_variadic_inferred[T](x T, handlers ...fn (T) int) int {
	_ := r
	return handlers[0](x) + 1
}

fn test_generic_fn_value_is_monomorphized_from_inferred_variadic_generic_method_call_arg() {
	assert GenericFnArgRunner{}.run_variadic_inferred(GenericFnArgFoo{},
		generic_fn_arg_named_handler) == 42
}
