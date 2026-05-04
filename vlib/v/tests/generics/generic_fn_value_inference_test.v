import math

fn apply_f64(value f64, f fn (f64) f64) f64 {
	return f(value)
}

fn test_generic_fn_value_is_inferred_from_expected_fn_type() {
	assert apply_f64(-1.25, math.abs) == 1.25
}
