fn test_forward_comptime_fn_call_in_if() {
	mut value := 0
	$if forward_comptime_value() == 1 {
		value = 1
	}
	assert value == 1
}

@[comptime]
fn forward_comptime_value() int {
	return 1
}

fn test_float_power_in_comptime_if() {
	mut value := 0
	$if f64(2.0) ** f64(2.0) == 4.0 {
		value = 1
	}
	assert value == 1
}
