import x.json2 as json

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

	if x := json.decode[i8]('128') {
		assert false
	}
	if x := json.decode[i8]('130') {
		assert false
	}
	if x := json.decode[i8]('1000') {
		assert false
	}
	if x := json.decode[i8]('-129') {
		assert false
	}
	if x := json.decode[i8]('-130') {
		assert false
	}
	if x := json.decode[i8]('-1000') {
		assert false
	}
}

fn test_boolean() {
	assert json.decode[bool]('false')! == false
	assert json.decode[bool]('true')! == true
}
