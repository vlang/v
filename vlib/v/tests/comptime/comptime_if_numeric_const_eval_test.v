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

type ComptimeAddend = u8

fn (a ComptimeAddend) + (b ComptimeAddend) ComptimeAddend {
	return ComptimeAddend(u8(a) + u8(b))
}

fn test_overloaded_numeric_operation_in_comptime_if() {
	mut value := 0
	$if ComptimeAddend(1) + ComptimeAddend(1) == ComptimeAddend(2) {
		value = 1
	}
	assert value == 1
}

type ComptimeSubtractingAddend = i16

fn (a ComptimeSubtractingAddend) + (b ComptimeSubtractingAddend) ComptimeSubtractingAddend {
	return a - b
}

fn test_overloaded_numeric_operation_result_in_comptime_if() {
	mut value := 0
	$if ComptimeSubtractingAddend(1) + ComptimeSubtractingAddend(1) == ComptimeSubtractingAddend(2) {
		value = 1
	} $else {
		value = 2
	}
	assert value == 2
}

type RightOperandComptimeAddend = u8

fn (a RightOperandComptimeAddend) + (b RightOperandComptimeAddend) RightOperandComptimeAddend {
	return a - b
}

fn test_right_operand_overload_is_not_used_in_comptime_if() {
	mut value := 0
	$if u8(1) + RightOperandComptimeAddend(2) == RightOperandComptimeAddend(3) {
		value = 1
	}
	assert value == 1
}
