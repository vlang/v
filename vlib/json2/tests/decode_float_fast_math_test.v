import json2

fn test_float_overflow_is_rejected_with_finite_math_optimizations() {
	for input in ['1e400', '-1e400', '[1e400]', '{"value":-1e400}'] {
		if value := json2.decode[json2.Any](input) {
			assert false, 'accepted overflowing value ${input}: ${value}'
		} else {
			assert err.msg().contains('range')
		}
	}
	for input in ['1e400', '-1e400', '"1e400"'] {
		if value := json2.decode[f64](input) {
			assert false, 'accepted overflowing f64 ${input}: ${value}'
		} else {
			assert err.msg().contains('range')
		}
	}
	for input in ['1e39', '-1e39', '"1e39"'] {
		if value := json2.decode[f32](input) {
			assert false, 'accepted overflowing f32 ${input}: ${value}'
		} else {
			assert err.msg().contains('range')
		}
	}
	assert json2.decode[f64]('1.5')! == 1.5
	assert json2.decode[f32]('-2.5')! == f32(-2.5)
}
