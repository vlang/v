import json2
import math

type RangeFloat = f32

struct RangeFloatField {
	value f64
}

fn test_decode_any_rejects_overflow_in_nested_values() {
	for input in ['1e400', '-1e400', '{"k":1e400}', '[1e400]', '{"k":[-1e400]}'] {
		if result := json2.decode[json2.Any](input) {
			assert false, 'accepted ${input}: ${result}'
		} else {
			assert err.msg().contains('range')
		}
	}
}

fn test_decode_floats_rejects_target_overflow() {
	for input in ['1e400', '-1e400', '"1e400"'] {
		if result := json2.decode[f64](input) {
			assert false, 'accepted ${input}: ${result}'
		} else {
			assert err.msg().contains('range')
		}
	}
	for input in ['1e39', '-1e39', '"1e39"'] {
		if result := json2.decode[RangeFloat](input) {
			assert false, 'accepted ${input}: ${result}'
		} else {
			assert err.msg().contains('range')
		}
	}
	if result := json2.decode[RangeFloatField]('{"value":1e400}') {
		assert false, 'accepted overflowing field: ${result}'
	} else {
		assert err.msg().contains('range')
	}
}

fn test_decode_float_range_keeps_finite_values_and_null() {
	assert json2.decode[f64]('1.7976931348623157e308')! == math.max_f64
	assert json2.decode[f64]('5e-324')! > 0
	assert json2.decode[f64]('1e-400')! == 0
	assert math.f64_bits(json2.decode[f64]('-0')!) == u64(1) << 63
	assert json2.decode[f32]('3.4028234663852886e38')! == f32(math.max_f32)
	assert json2.encode(json2.decode[json2.Any]('{"k":null}')!) == '{"k":null}'
}

fn test_decode_reuse_recovers_after_float_overflow() {
	mut buffer := json2.DecodeBuffer{}
	if result := json2.decode_reuse[json2.Any]('{"k":1e400}', mut buffer) {
		assert false, 'accepted overflowing value: ${result}'
	} else {
		assert err.msg().contains('range')
	}
	assert json2.encode(json2.decode_reuse[json2.Any]('{"k":2}', mut buffer)!) == '{"k":2}'
}
