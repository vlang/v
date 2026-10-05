import math

fn test_plus_interpolation_keeps_width_and_precision() {
	x := 3.14159
	y := -2.5
	assert '${x:+08.2f}|${x:+.2f}|${y:+.1f}|${x:+}|${42:+}|${42:+05}' == '+0003.14|+3.14|-2.5|3.14159|+42|+0042'
	assert '${x:+8.2f}' == '   +3.14'
	assert '${y:+08.2f}' == '-0002.50'
	assert '${x:+.2}' == '+3.14'
	assert '${x:+g}' == '3.14159'
	assert '${f32(3.25):+07.2f}' == '+003.25'
	zero := 0.0
	negative_zero := -zero
	assert '${zero:+06.2f}' == '+00.00'
	assert '${negative_zero:+06.2f}' == '-00.00'
	assert '${math.inf(1):+08.2f}' == '    +inf'
	assert '${math.inf(-1):+08.2f}' == '    -inf'
	assert '${x:+.2e}' == '+3.14e+00'
	assert '${x:+.2E}' == '+3.14E+00'
	assert '${x:+.3g}' == '+3.14'
}

fn test_plus_integer_interpolation() {
	assert '${42:+d}' == '+42'
	assert '${42:+5d}' == '  +42'
	assert '${42:+05d}' == '+0042'
	assert '${-42:+05}' == '-0042'
	assert '${0:+03}' == '+00'
	assert '${42:+2}' == '+42'
	assert '${i8(42):+05}' == '+0042'
	assert '${u8(42):+05}' == '+0042'
	assert '${i64(-9223372036854775807) - 1:+05}' == '-9223372036854775808'
	assert '${u64(18446744073709551615):+05}' == '+18446744073709551615'
}

struct PlusFormatCounter {
mut:
	calls int
}

fn plus_format_next(mut counter PlusFormatCounter) f64 {
	counter.calls++
	return 3.25
}

fn test_plus_interpolation_evaluates_operand_once() {
	mut counter := PlusFormatCounter{}
	assert '${plus_format_next(mut counter):+07.2f}' == '+003.25'
	assert counter.calls == 1
}
