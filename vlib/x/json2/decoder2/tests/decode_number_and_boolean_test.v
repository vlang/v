import x.json2.decoder2 as json

fn test_number() {
	// Test u8
	assert json.decode[u8]('0')! == 0
	assert json.decode[u8]('1')! == 1
	assert json.decode[u8]('201')! == 201

	assert json.decode[u8]('-1')! == u8(-1)
	assert json.decode[u8]('-127')! == u8(-127)

	// Test u16
	assert json.decode[u16]('0')! == 0
	assert json.decode[u16]('1')! == 1
	assert json.decode[u16]('201')! == 201

	assert json.decode[u16]('-1')! == u16(-1)
	assert json.decode[u16]('-201')! == u16(-201)

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
}

fn test_boolean() {
	assert json.decode[bool]('false')! == false
	assert json.decode[bool]('true')! == true
}
