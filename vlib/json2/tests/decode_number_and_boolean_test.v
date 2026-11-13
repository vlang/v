// vtest vflags: -w
import json2 as json

fn test_number() {
	// Test u8
	assert json.decode[u8]('0')! == 0
	assert json.decode[u8]('1')! == 1
	assert json.decode[u8]('201')! == 201

	// Test u16
	assert json.decode[u16]('0')! == 0
	assert json.decode[u16]('1')! == 1
	assert json.decode[u16]('201')! == 201

	// Test u32
	assert json.decode[u32]('0')! == 0
	assert json.decode[u32]('1')! == 1
	assert json.decode[u32]('201')! == 201

	// Test u64
	assert json.decode[u64]('0')! == 0
	assert json.decode[u64]('1')! == 1
	assert json.decode[u64]('201')! == 201

	// Test i8
	assert json.decode[i8]('0')! == 0
	assert json.decode[i8]('1')! == 1
	assert json.decode[i8]('127')! == 127

	assert json.decode[i8]('-1')! == -1
	assert json.decode[i8]('-127')! == -127

	// Test i16
	assert json.decode[i16]('0')! == 0
	assert json.decode[i16]('1')! == 1
	assert json.decode[i16]('201')! == 201

	assert json.decode[i16]('-1')! == -1
	assert json.decode[i16]('-201')! == -201

	// Test int
	assert json.decode[int]('0')! == 0
	assert json.decode[int]('1')! == 1
	assert json.decode[int]('201')! == 201

	assert json.decode[int]('-1')! == -1
	assert json.decode[int]('-201')! == -201

	assert json.decode[int]('1234567890')! == 1234567890
	assert json.decode[int]('-1234567890')! == -1234567890

	// Test i64
	assert json.decode[i64]('0')! == 0
	assert json.decode[i64]('1')! == 1
	assert json.decode[i64]('201')! == 201

	assert json.decode[i64]('-1')! == -1
	assert json.decode[i64]('-201')! == -201

	assert json.decode[i64]('1234567890')! == 1234567890
	assert json.decode[i64]('-1234567890')! == -1234567890

	// Test f32
	assert json.decode[f32]('0')! == 0.0
	assert json.decode[f32]('1')! == 1.0
	assert json.decode[f32]('1.2')! == 1.2
	assert json.decode[f32]('-1.2')! == -1.2
	assert json.decode[f32]('201')! == 201.0

	assert json.decode[f32]('-1')! == -1.0
	assert json.decode[f32]('-201')! == -201.0

	assert json.decode[f32]('1234567890')! == 1234567890.0
	assert json.decode[f32]('-1234567890')! == -1234567890.0

	// Test f64
	assert json.decode[f64]('0')! == 0.0
	assert json.decode[f64]('1')! == 1.0
	assert json.decode[f64]('1.2')! == 1.2
	assert json.decode[f64]('201')! == 201.0

	assert json.decode[f64]('-1')! == -1.0
	assert json.decode[f64]('-1.2')! == -1.2
	assert json.decode[f64]('-201')! == -201.0

	assert json.decode[f64]('1234567890')! == 1234567890.0
	assert json.decode[f64]('-1234567890')! == -1234567890.0

	assert json.decode[f64]('1e10')! == 10000000000
	assert json.decode[f64]('1E10')! == 10000000000
	assert json.decode[f64]('1e+10')! == 10000000000
	assert json.decode[f64]('1e-10')! == 0.0000000001
	assert json.decode[f64]('-1e10')! == -10000000000
	assert json.decode[f64]('-1E-10')! == -0.0000000001
	assert json.decode[f64]('0.123e3')! - 123 < 0.0000001
	assert json.decode[f64]('10.5E+3')! == 10500

	// Test Over/Underflow
	assert json.decode[i8]('127')! == 127
	assert json.decode[i8]('-128')! == -128

	if _ := json.decode[i8]('128') {
		assert false
	}
	if _ := json.decode[i8]('130') {
		assert false
	}
	if _ := json.decode[i8]('1000') {
		assert false
	}
	if _ := json.decode[i8]('-129') {
		assert false
	}
	if _ := json.decode[i8]('-130') {
		assert false
	}
	if _ := json.decode[i8]('-1000') {
		assert false
	}
}

fn test_boolean() {
	assert json.decode[bool]('false')! == false
	assert json.decode[bool]('true')! == true
}

struct NumericIntegerValues {
mut:
	ux8  u8
	ux16 u16
	ux32 u32
	ux64 u64
	sx8  i8
	sx16 i16
	sx32 int
	sx64 i64
}

struct ScientificIntegerValue {
	size i64
}

struct Numeric64BitIntegers {
	signed   i64
	unsigned u64
}

struct ScientificIntegerDecodeCase {
	input    string
	expected i64
}

struct FloatToIntegerDecodeCase {
	input             string
	expected_signed   i64
	expected_unsigned u64
}

struct IntegerStringDecodeCase {
	input    string
	expected int
}

struct FloatStringDecodeCase {
	input    string
	expected f64
}

fn test_all_primitive_integer_types_are_encodable_and_decodable() {
	value := NumericIntegerValues{
		ux8:  1
		ux16: 2
		ux32: 3
		ux64: 4
		sx8:  -1
		sx16: -2
		sx32: -3
		sx64: -4
	}
	encoded := json.encode[NumericIntegerValues](value)
	assert encoded == '{"ux8":1,"ux16":2,"ux32":3,"ux64":4,"sx8":-1,"sx16":-2,"sx32":-3,"sx64":-4}'
	assert json.decode[NumericIntegerValues](encoded)! == value
}

fn test_decode_i64_from_scientific_notation() {
	cases := [
		ScientificIntegerDecodeCase{
			input:    '{"size":1.7596215426069998e+12}'
			expected: i64(1.7596215426069998e+12)
		},
		ScientificIntegerDecodeCase{
			input:    '{"size":"1.7596215426069998e+12"}'
			expected: i64(1.7596215426069998e+12)
		},
	]
	for case in cases {
		decoded := json.decode[ScientificIntegerValue](case.input)!
		assert decoded.size == case.expected
	}
	if _ := json.decode[ScientificIntegerValue]('{"size":123.45}') {
		assert false, 'Expected a fractional number to fail when decoding to i64'
	}
}

fn test_decode_float_values_into_64bit_integer_fields() {
	cases := [
		FloatToIntegerDecodeCase{
			input:             '{"signed": -1756811041916.0, "unsigned": 1756811041916.0}'
			expected_signed:   i64(-1756811041916)
			expected_unsigned: u64(1756811041916)
		},
		FloatToIntegerDecodeCase{
			input:             '{"signed": "-1756811041916.0", "unsigned": "1756811041916.0"}'
			expected_signed:   i64(-1756811041916)
			expected_unsigned: u64(1756811041916)
		},
	]
	for case in cases {
		actual := json.decode[Numeric64BitIntegers](case.input)!
		assert actual.signed == case.expected_signed
		assert actual.unsigned == case.expected_unsigned
	}
}

fn assert_strict_number_decode_fails[T](input string) {
	json.decode[T](input, strict: true) or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in strict mode: ${input}'
}

fn test_strict_strings_are_not_decoded_as_numbers() {
	for input in ['"0"', '"100"'] {
		assert_strict_number_decode_fails[int](input)
	}
	assert_strict_number_decode_fails[f64]('"-23.6e1"')
	assert_strict_number_decode_fails[[]int]('["100", 99, "98", 97]')
}

fn test_default_string_decoded_as_int() {
	cases := [
		IntegerStringDecodeCase{
			input:    '"0"'
			expected: 0
		},
		IntegerStringDecodeCase{
			input:    '"100"'
			expected: 100
		},
		IntegerStringDecodeCase{
			input:    '"-50"'
			expected: -50
		},
	]
	for case in cases {
		assert json.decode[int](case.input)! == case.expected
	}
}

fn test_default_string_decoded_as_float() {
	cases := [
		FloatStringDecodeCase{
			input:    '"-23.6e1"'
			expected: -236.0
		},
		FloatStringDecodeCase{
			input:    '"3.14"'
			expected: 3.14
		},
	]
	for case in cases {
		assert json.decode[f64](case.input)! == case.expected
	}
}

fn test_default_string_in_array_decoded_as_int() {
	for input in ['["100", "99", "98", "97"]', '["100", 99, "98", 97]'] {
		assert json.decode[[]int](input)! == [100, 99, 98, 97]
	}
}

fn test_valid_unquoted_numbers() {
	integer_cases := [
		IntegerStringDecodeCase{
			input:    '0'
			expected: 0
		},
		IntegerStringDecodeCase{
			input:    '100'
			expected: 100
		},
		IntegerStringDecodeCase{
			input:    '-50'
			expected: -50
		},
	]
	for case in integer_cases {
		assert json.decode[int](case.input)! == case.expected
	}
	float_cases := [
		FloatStringDecodeCase{
			input:    '-23.6e1'
			expected: -236.0
		},
	]
	for case in float_cases {
		assert json.decode[f64](case.input)! == case.expected
	}
	assert json.decode[[]int]('[100, 99, 98, 97]')! == [100, 99, 98, 97]
	for input in ['0', '100'] {
		assert json.decode[int](input, strict: true)! == input.int()
	}
}

fn test_invalid_number_string_fails() {
	if _ := json.decode[int]('"not_a_number"') {
		assert false, 'Expected an invalid number string to fail'
	}
}

fn test_leading_plus_in_string() {
	assert json.decode[int]('"+100"')! == 100
	assert_strict_number_decode_fails[int]('"+100"')
}
