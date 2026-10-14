module json2

import math

type FormatFloatAlias = f64

struct FormatFloatHolder {
	value f64
}

fn assert_float_encoding[T](value T, expected string) {
	assert encode(value) == expected
	assert encode(Any(value)) == expected
	mut output := 'prefix:'.bytes()
	encode_append(value, mut output)
	assert output.bytestr() == 'prefix:' + expected
}

fn test_float_notation_cutoffs() {
	for value, expected in {
		f64(0):               '0'
		1e-7:                 '1e-7'
		1e-6:                 '0.000001'
		1e-5:                 '0.00001'
		1e-4:                 '0.0001'
		1e6:                  '1000000'
		1.234567890123456e10: '12345678901.23456'
		1e20:                 '100000000000000000000'
		1e21:                 '1e+21'
		1e100:                '1e+100'
		1e-100:               '1e-100'
	} {
		assert_float_encoding(value, expected)
		if value != 0 {
			assert_float_encoding(-value, '-' + expected)
		}
	}
	assert encode(FormatFloatAlias(1e20)) == '100000000000000000000'
	assert encode(FormatFloatHolder{1e6}) == '{"value":1000000}'
	assert encode([f64(1e-6), 1e-7, 1e20, 1e21]) == '[0.000001,1e-7,100000000000000000000,1e+21]'
}

fn test_f32_notation_cutoffs() {
	for value, expected in {
		f32(0):    '0'
		f32(1e-7): '1e-7'
		f32(1e-6): '0.000001'
		f32(1e6):  '1000000'
		f32(1e20): '100000000000000000000'
		f32(1e21): '1e+21'
	} {
		assert_float_encoding(value, expected)
		if value != 0 {
			assert_float_encoding(-value, '-' + expected)
		}
	}
}

fn test_float_notation_preserves_round_trip_and_special_values() {
	for value in [math.nextafter(1e-6, 0), 1e-6, math.nextafter(1e-6, 1), math.nextafter(1e21, 0),
		1e21, math.nextafter(1e21, math.inf(1)), math.f64_from_bits(1),
		math.f64_from_bits(0x7fefffffffffffff)] {
		text := encode(value)
		assert math.f64_bits(decode[f64](text)!) == math.f64_bits(value)
	}
	assert encode(math.f64_from_bits(0x8000000000000000)) == '-0'
	assert encode(math.f32_from_bits(0x80000000)) == '-0'
	assert encode(math.nan()) == 'null'
	assert encode(math.inf(1)) == 'null'
	assert encode(math.inf(-1)) == 'null'
	// JSON formatting does not change ordinary float display.
	assert f64(1e6).str() == '1e+06'
	assert f32(1e-7).str() == '1e-07'
}
