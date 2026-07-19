import json2.decoder2 as json

struct IntegerField {
	n int
}

fn test_number() {
	// Test u8
	assert json.decode[u8]('0')! == 0
	assert json.decode[u8]('1')! == 1
	assert json.decode[u8]('201')! == 201
	assert json.decode[u8]('255')! == max_u8

	// Test u16
	assert json.decode[u16]('0')! == 0
	assert json.decode[u16]('1')! == 1
	assert json.decode[u16]('201')! == 201
	assert json.decode[u16]('65535')! == max_u16

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
	assert json.decode[i8]('-128')! == min_i8

	// Test i16
	assert json.decode[i16]('0')! == 0
	assert json.decode[i16]('1')! == 1
	assert json.decode[i16]('201')! == 201

	assert json.decode[i16]('-1')! == -1
	assert json.decode[i16]('-201')! == -201
	assert json.decode[i16]('-32768')! == min_i16

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
	assert json.decode[i64]('-9223372036854775808')! == min_i64

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

	assert json.decode[f64]('1e3')! == 1000.0
	assert json.decode[f64]('1E+3')! == 1000.0
	assert json.decode[f64]('-2.5e-2')! == -0.025
	assert json.decode[int]('1e3')! == 1000
	assert json.decode[int]('123.0')! == 123
	assert json.decode[int]('1.2e1')! == 12
	assert json.decode[int]('10e-1')! == 1
	assert json.decode[i64]('9007199254740993e0')! == i64(9007199254740993)
	assert json.decode[i64]('900719925474099.3e1')! == i64(9007199254740993)
	assert json.decode[u64]('18446744073709551615e0')! == u64(18446744073709551615)
}

fn test_fractional_numbers_are_rejected_for_integer_targets() {
	for input in ['123.45', '-123.45', '1e-1', '1.2e0'] {
		mut failed := false
		json.decode[int](input) or { failed = true }
		assert failed, 'expected `${input}` to be rejected for an integer target'
	}

	mut failed := false
	json.decode[IntegerField]('{"n":123.45}') or { failed = true }
	assert failed
}

fn assert_integer_decode_fails[T](input string) {
	json.decode[T](input) or { return }
	assert false, 'expected `${input}` to be rejected for ${typeof(T{}).name}'
}

fn test_out_of_range_integers_are_rejected() {
	assert_integer_decode_fails[u8]('-1')
	assert_integer_decode_fails[u8]('256')
	assert_integer_decode_fails[u16]('-1')
	assert_integer_decode_fails[u16]('65536')
	assert_integer_decode_fails[u32]('-1')
	assert_integer_decode_fails[u32]('4294967296')
	assert_integer_decode_fails[u64]('-1')
	assert_integer_decode_fails[u64]('18446744073709551616')

	assert_integer_decode_fails[i8]('-129')
	assert_integer_decode_fails[i8]('128')
	assert_integer_decode_fails[i8]('128e0')
	assert_integer_decode_fails[i16]('-32769')
	assert_integer_decode_fails[i16]('32768')
	assert_integer_decode_fails[i32]('-2147483649')
	assert_integer_decode_fails[i32]('2147483648')
	assert_integer_decode_fails[i64]('-9223372036854775809')
	assert_integer_decode_fails[i64]('9223372036854775808')
}

fn test_boolean() {
	assert json.decode[bool]('false')! == false
	assert json.decode[bool]('true')! == true
}
