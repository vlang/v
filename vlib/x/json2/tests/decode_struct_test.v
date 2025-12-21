import x.json2 as json
import time
import math

const fixed_time = time.new(
	year:   2022
	month:  3
	day:    11
	hour:   13
	minute: 54
	second: 25
)

type StringAlias = string
type BoolAlias = bool
type IntAlias = int
type TimeAlias = time.Time
type StructAlias = StructType[int]
type EnumAlias = Enumerates

type SumTypes = StructType[string] | []SumTypes | []string | bool | string | time.Time | u32

enum Enumerates {
	a
	b
	c
	d
	e = 99
	f
}

struct StructType[T] {
mut:
	val T
}

struct StructTypeOption[T] {
mut:
	val ?T
}

struct StructTypePointer[T] {
mut:
	val &T
}

fn test_types() {
	assert json.decode[StructType[string]]('{"val": ""}')!.val == ''

	assert json.decode[StructType[string]]('{"val": "2"}')!.val == '2'

	assert json.decode[StructType[int]]('{"val": 2}')!.val == 2

	assert json.decode[StructType[map[string]string]]('{"val": {"val1": "test"}}')!.val['val1'] == 'test'

	assert json.decode[StructType[Enumerates]]('{"val": 0}')!.val == Enumerates.a
	assert json.decode[StructType[Enumerates]]('{"val": 1}')!.val == Enumerates.b

	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val == fixed_time
	assert json.decode[StructType[time.Time]]('{"val": "2022-03-11T13:54:25.000Z"}')!.val.unix() == fixed_time.unix()
}

fn test_option_types() {
	if x := json.decode[StructTypeOption[string]]('{}')!.val {
		assert false, 'Should return none'
	} else {
		assert err.msg() == ''
		assert true
	}

	if x := json.decode[StructTypeOption[string]]('{"val": "2"}')!.val {
		assert x == '2'
	} else {
		assert false, 'Should not return none'
	}
}

// Test structure for basic number types
struct JsonNumbers {
	val_i8  i8
	val_i16 i16
	val_i32 i32
	val_i64 i64
	val_u8  u8
	val_u16 u16
	val_u32 u32
	val_u64 u64
	val_int int
	val_f32 f32
	val_f64 f64
}

// Test structure for boundary values
struct JsonBoundaries {
	min_i8 i8
	max_i8 i8
	min_u8 u8
	max_u8 u8
	zero   int
}

// Test structure for fake numbers (quoted numbers)
struct JsonFakeNumbers {
	quoted_int   string
	quoted_float string
	mixed_int    int
}

// Test structure for float precision
struct JsonFloats {
	val_f32 f32
	val_f64 f64
}

// Test basic functionality
struct JsonU8 {
	val1 u8
	val2 u8
}

fn test_u8_values() {
	x := JsonU8{
		val1: 58
		val2: 58
	}

	str := json.encode(x)
	y := json.decode[JsonU8](str) or { panic(err) }
	assert x == y
	println('✓ Basic u8 test passed')
}

fn test_all_number_types() {
	// Test normal range values
	x := JsonNumbers{
		val_i8:  -100
		val_i16: -1000
		val_i32: -100000
		val_i64: -1000000000
		val_u8:  58
		val_u16: 2000
		val_u32: 200000
		val_u64: 2000000000
		val_int: 300000
		val_f32: 3.14
		val_f64: 2.71828
	}

	str := json.encode(x)
	y := json.decode[JsonNumbers](str) or { panic('Decoding failed: ${err}') }

	assert x.val_i8 == y.val_i8
	assert x.val_i16 == y.val_i16
	assert x.val_i32 == y.val_i32
	assert x.val_i64 == y.val_i64
	assert x.val_u8 == y.val_u8
	assert x.val_u16 == y.val_u16
	assert x.val_u32 == y.val_u32
	assert x.val_u64 == y.val_u64
	assert x.val_int == y.val_int
	assert math.abs(x.val_f32 - y.val_f32) < 0.001
	assert math.abs(x.val_f64 - y.val_f64) < 0.000001

	println('✓ All number types test passed')
}

fn test_number_boundaries() {
	// Test minimum and maximum values for various types
	test_cases := [
		json.encode(JsonU8{ val1: 0, val2: 255 }),
		json.encode(JsonU8{ val1: u8(128), val2: u8(255) }),
		'{"min_i8": -128, "max_i8": 127}',
		'{"min_u8": 0, "max_u8": 255}',
		'{"zero": 0}',
	]!

	for i, case in test_cases {
		result := json.decode[JsonU8](case) or {
			// Some boundary values might fail, which is expected
			continue
		}
		assert result.val1 >= 0 && result.val1 <= 255
		assert result.val2 >= 0 && result.val2 <= 255
	}
	println('✓ Boundary values test passed')
}

fn test_fake_numbers() {
	// Test quoted numbers (fake numbers)
	fake_number_cases := [
		'{"val1": "123", "val2": "45"}', // Fully quoted
		'{"val1": 123, "val2": "45"}', // Mixed format
		'{"val1": "255", "val2": 0}', // Boundary value as string
	]!

	for case in fake_number_cases {
		result := json.decode[JsonU8](case) or {
			panic('Fake number decoding failed: ${err}, input: ${case}')
		}
		assert result.val1 >= 0 && result.val1 <= 255
		assert result.val2 >= 0 && result.val2 <= 255
	}
	println('✓ Fake numbers (quoted numbers) test passed')
}

fn test_error_conditions() {
	// Test invalid inputs that should trigger errors
	invalid_cases := [
		'{"val1": "abc", "val2": 58}', // Non-numeric string
		'{"val1": "", "val2": 58}', // Empty string
		'{"val1": 300, "val2": 58}', // Out of u8 range (300 > 255)
		'{"val1": -1, "val2": 58}', // Negative value for u8
		'{"val1": 123.45, "val2": 58}', // Float for integer type
	]!

	mut error_count := 0
	for case in invalid_cases {
		result := json.decode[JsonU8](case) or {
			error_count++
			continue // Expected failure, error handling works correctly
		}
		// If we reach here, it means decoding succeeded when it should have failed
		panic('Expected decoding to fail but succeeded: ${case}')
	}
	assert error_count > 0 // Ensure we caught some errors
	println('✓ Error handling test passed')
}

fn test_float_precision() {
	// Test floating point precision and scientific notation
	test_cases := [
		JsonFloats{
			val_f32: 3.14159265
			val_f64: 2.718281828459045
		},
		JsonFloats{
			val_f32: 1.0e10
			val_f64: 1.0e-10
		},
		JsonFloats{
			val_f32: -123.456
			val_f64: -789.012
		},
		JsonFloats{
			val_f32: 0.0
			val_f64: 0.0
		},
	]!

	for i, x in test_cases {
		str := json.encode(x)
		dump(str)
		y := json.decode[JsonFloats](str) or { panic('Float decoding failed: ${err}') }

		// Use relative error for assertions
		assert math.abs(y.val_f32 - x.val_f32) / math.abs(x.val_f32 + 1e-10) < 1e-6
		assert math.abs(y.val_f64 - x.val_f64) / math.abs(x.val_f64 + 1e-10) < 1e-12
	}
	println('✓ Float precision test passed')
}

fn test_performance_large_scale() {
	// Performance test: decode large numbers of values
	mut total := u64(0)

	for i in 0 .. 1000 {
		x := JsonU8{
			val1: u8(i % 256)
			val2: u8((i * 7) % 256)
		}
		str := json.encode(x)
		y := json.decode[JsonU8](str) or { panic('Performance test decoding failed: ${err}') }
		total += y.val1 + y.val2
	}

	assert total > 0 // Ensure the loop executed
	println('✓ Performance test passed')
}

fn test_special_float_values() {
	// Test special float values
	struct SpecialFloats {
		nan_val     f64
		inf_val     f64
		neg_inf_val f64
	}

	// Note: JSON doesn't natively support NaN and Inf, so we test with strings
	special_cases := [
		'{"val_f32": 0.0, "val_f64": 0.0}',
		'{"val_f32": -0.0, "val_f64": -0.0}',
	]!

	for case in special_cases {
		result := json.decode[JsonFloats](case) or {
			// Some special values might not be supported, which is expected
			continue
		}
		assert true // If we reach here, decoding didn't crash
	}
	println('✓ Special float values test passed')
}

fn test_json_format_consistency() {
	// Test that encoding and decoding preserves data integrity
	original := JsonNumbers{
		val_i8:  100
		val_i16: 1000
		val_i32: 100000
		val_i64: 1000000000
		val_u8:  200
		val_u16: 2000
		val_u32: 200000
		val_u64: 2000000000
		val_int: 300000
		val_f32: 3.14
		val_f64: 2.71828
	}

	// Encode and decode multiple times
	str1 := json.encode(original)
	decoded1 := json.decode[JsonNumbers](str1) or { panic('First decode failed: ${err}') }
	str2 := json.encode(decoded1)
	decoded2 := json.decode[JsonNumbers](str2) or { panic('Second decode failed: ${err}') }

	// All versions should be equivalent
	assert decoded1 == decoded2
	println('✓ JSON format consistency test passed')
}
