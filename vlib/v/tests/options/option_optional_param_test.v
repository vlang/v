fn with_optional_delta(base int, delta ?int) int {
	actual_delta := delta or { 0 }
	return base + actual_delta
}

fn optional_only(value ?int) int {
	return value or { -1 }
}

struct OptionalGreeter {}

fn (_ OptionalGreeter) add(base int, delta ?int) int {
	return with_optional_delta(base, delta)
}

type OptionalGreeterFn = fn (int, ?int) int

fn test_trailing_optional_params_can_be_omitted_in_free_function_calls() {
	assert with_optional_delta(5) == 5
	assert with_optional_delta(5, 2) == 7
	assert optional_only() == -1
	assert optional_only(8) == 8
}

fn test_trailing_optional_params_can_be_omitted_in_method_calls() {
	greeter := OptionalGreeter{}
	assert greeter.add(5) == 5
	assert greeter.add(5, 2) == 7
}

fn test_trailing_optional_params_can_be_omitted_in_fn_variable_calls() {
	handler := OptionalGreeterFn(with_optional_delta)
	assert handler(5) == 5
	assert handler(5, 2) == 7
}
