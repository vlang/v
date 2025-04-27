import x.json2 as json

const sample_data = {
	'u8':   json.Any(u8(1))
	'u16':  json.Any(u16(2))
	'u32':  json.Any(u32(3))
	'u64':  json.Any(u64(4))
	'i8':   json.Any(i8(5))
	'i16':  json.Any(i16(6))
	'i32':  json.Any(i32(7))
	'int':  json.Any(int(8))
	'i64':  json.Any(i64(9))
	'f32':  json.Any(f32(2.3))
	'f64':  json.Any(f64(1.283))
	'bool': json.Any(false)
	'str':  json.Any('test')
	'null': json.Any(json.null)
	'arr':  json.Any([json.Any('lol')])
	'obj':  json.Any({
		'foo': json.Any(10)
	})
}

fn is_null(f json.Any) bool {
	match f {
		json.Null { return true }
		else { return false }
	}
}

fn test_f32() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.f32() == 1.0
	assert sample_data['u16'] or { 0 }.f32() == 2.0
	assert sample_data['u32'] or { 0 }.f32() == 3.0
	assert sample_data['u64'] or { 0 }.f32() == 4.0
	assert sample_data['i8'] or { 0 }.f32() == 5.0
	assert sample_data['i16'] or { 0 }.f32() == 6.0
	assert sample_data['i32'] or { 0 }.f32() == 7.0
	assert sample_data['int'] or { 0 }.f32() == 8.0
	assert sample_data['i64'] or { 0 }.f32() == 9.0
	assert sample_data['f32'] or { 0 }.f32() == 2.3
	assert sample_data['f64'] or { 0 }.f32() == 1.2829999923706055
	// invalid conversions
	assert sample_data['bool'] or { 0 }.f32() == 0.0
	assert sample_data['str'] or { 0 }.f32() == 0.0
	assert sample_data['null'] or { 0 }.f32() == 0.0
	assert sample_data['arr'] or { 0 }.f32() == 0.0
	assert sample_data['obj'] or { 0 }.f32() == 0.0
}

fn test_f64() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.f64() == 1.0
	assert sample_data['u16'] or { 0 }.f64() == 2.0
	assert sample_data['u32'] or { 0 }.f64() == 3.0
	assert sample_data['u64'] or { 0 }.f64() == 4.0
	assert sample_data['i8'] or { 0 }.f64() == 5.0
	assert sample_data['i16'] or { 0 }.f64() == 6.0
	assert sample_data['i32'] or { 0 }.f64() == 7.0
	assert sample_data['int'] or { 0 }.f64() == 8.0
	assert sample_data['i64'] or { 0 }.f64() == 9.0
	assert sample_data['f32'] or { 0 }.f64() == 2.299999952316284
	assert sample_data['f64'] or { 0 }.f64() == 1.283
	// invalid conversions
	assert sample_data['bool'] or { 0 }.f64() == 0.0
	assert sample_data['str'] or { 0 }.f64() == 0.0
	assert sample_data['null'] or { 0 }.f64() == 0.0
	assert sample_data['arr'] or { 0 }.f64() == 0.0
	assert sample_data['obj'] or { 0 }.f64() == 0.0
}

fn test_i8() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.i8() == 1
	assert sample_data['u16'] or { 0 }.i8() == 2
	assert sample_data['u32'] or { 0 }.i8() == 3
	assert sample_data['u64'] or { 0 }.i8() == 4
	assert sample_data['i8'] or { 0 }.i8() == 5
	assert sample_data['i16'] or { 0 }.i8() == 6
	assert sample_data['i32'] or { 0 }.i8() == 7
	assert sample_data['int'] or { 0 }.i8() == 8
	assert sample_data['i64'] or { 0 }.i8() == 9
	assert sample_data['f32'] or { 0 }.i8() == 2
	assert sample_data['f64'] or { 0 }.i8() == 1
	assert json.Any(true).i8() == 1
	assert json.Any('123').i8() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.i8() == 0
	assert sample_data['arr'] or { 0 }.i8() == 0
	assert sample_data['obj'] or { 0 }.i8() == 0
}

fn test_i16() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.i16() == 1
	assert sample_data['u16'] or { 0 }.i16() == 2
	assert sample_data['u32'] or { 0 }.i16() == 3
	assert sample_data['u64'] or { 0 }.i16() == 4
	assert sample_data['i8'] or { 0 }.i16() == 5
	assert sample_data['i16'] or { 0 }.i16() == 6
	assert sample_data['i32'] or { 0 }.i16() == 7
	assert sample_data['int'] or { 0 }.i16() == 8
	assert sample_data['i64'] or { 0 }.i16() == 9
	assert sample_data['f32'] or { 0 }.i16() == 2
	assert sample_data['f64'] or { 0 }.i16() == 1
	assert json.Any(true).i16() == 1
	assert json.Any('123').i16() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.i16() == 0
	assert sample_data['arr'] or { 0 }.i16() == 0
	assert sample_data['obj'] or { 0 }.i16() == 0
}

fn test_i32() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.i32() == 1
	assert sample_data['u16'] or { 0 }.i32() == 2
	assert sample_data['u32'] or { 0 }.i32() == 3
	assert sample_data['u64'] or { 0 }.i32() == 4
	assert sample_data['i8'] or { 0 }.i32() == 5
	assert sample_data['i16'] or { 0 }.i32() == 6
	assert sample_data['i32'] or { 0 }.i32() == 7
	assert sample_data['int'] or { 0 }.i32() == 8
	assert sample_data['i64'] or { 0 }.i32() == 9
	assert sample_data['f32'] or { 0 }.i32() == 2
	assert sample_data['f64'] or { 0 }.i32() == 1
	assert json.Any(true).i32() == 1
	assert json.Any('123').i32() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.i32() == 0
	assert sample_data['arr'] or { 0 }.i32() == 0
	assert sample_data['obj'] or { 0 }.i32() == 0
}

fn test_int() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.int() == 1
	assert sample_data['u16'] or { 0 }.int() == 2
	assert sample_data['u32'] or { 0 }.int() == 3
	assert sample_data['u64'] or { 0 }.int() == 4
	assert sample_data['i8'] or { 0 }.int() == 5
	assert sample_data['i16'] or { 0 }.int() == 6
	assert sample_data['i32'] or { 0 }.int() == 7
	assert sample_data['int'] or { 0 }.int() == 8
	assert sample_data['i64'] or { 0 }.int() == 9
	assert sample_data['f32'] or { 0 }.int() == 2
	assert sample_data['f64'] or { 0 }.int() == 1
	assert json.Any(true).int() == 1
	assert json.Any('123').int() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.int() == 0
	assert sample_data['arr'] or { 0 }.int() == 0
	assert sample_data['obj'] or { 0 }.int() == 0
}

fn test_i64() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.i64() == 1
	assert sample_data['u16'] or { 0 }.i64() == 2
	assert sample_data['u32'] or { 0 }.i64() == 3
	assert sample_data['u64'] or { 0 }.i64() == 4
	assert sample_data['i8'] or { 0 }.i64() == 5
	assert sample_data['i16'] or { 0 }.i64() == 6
	assert sample_data['i32'] or { 0 }.i64() == 7
	assert sample_data['int'] or { 0 }.i64() == 8
	assert sample_data['i64'] or { 0 }.i64() == 9
	assert sample_data['f32'] or { 0 }.i64() == 2
	assert sample_data['f64'] or { 0 }.i64() == 1
	assert json.Any(true).i64() == 1
	assert json.Any('123').i64() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.i64() == 0
	assert sample_data['arr'] or { 0 }.i64() == 0
	assert sample_data['obj'] or { 0 }.i64() == 0
}

fn test_u8() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.u8() == 1
	assert sample_data['u16'] or { 0 }.u8() == 2
	assert sample_data['u32'] or { 0 }.u8() == 3
	assert sample_data['u64'] or { 0 }.u8() == 4
	assert sample_data['i8'] or { 0 }.u8() == 5
	assert sample_data['i16'] or { 0 }.u8() == 6
	assert sample_data['i32'] or { 0 }.u8() == 7
	assert sample_data['int'] or { 0 }.u8() == 8
	assert sample_data['i64'] or { 0 }.u8() == 9
	assert sample_data['f32'] or { 0 }.u8() == 2
	assert sample_data['f64'] or { 0 }.u8() == 1
	assert json.Any(true).u8() == 1
	assert json.Any('123').u8() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.u8() == 0
	assert sample_data['arr'] or { 0 }.u8() == 0
	assert sample_data['obj'] or { 0 }.u8() == 0
}

fn test_u16() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.u16() == 1
	assert sample_data['u16'] or { 0 }.u16() == 2
	assert sample_data['u32'] or { 0 }.u16() == 3
	assert sample_data['u64'] or { 0 }.u16() == 4
	assert sample_data['i8'] or { 0 }.u16() == 5
	assert sample_data['i16'] or { 0 }.u16() == 6
	assert sample_data['i32'] or { 0 }.u16() == 7
	assert sample_data['int'] or { 0 }.u16() == 8
	assert sample_data['i64'] or { 0 }.u16() == 9
	assert sample_data['f32'] or { 0 }.u16() == 2
	assert sample_data['f64'] or { 0 }.u16() == 1
	assert json.Any(true).u16() == 1
	assert json.Any('123').u16() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.u16() == 0
	assert sample_data['arr'] or { 0 }.u16() == 0
	assert sample_data['obj'] or { 0 }.u16() == 0
}

fn test_u32() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.u32() == 1
	assert sample_data['u16'] or { 0 }.u32() == 2
	assert sample_data['u32'] or { 0 }.u32() == 3
	assert sample_data['u64'] or { 0 }.u32() == 4
	assert sample_data['i8'] or { 0 }.u32() == 5
	assert sample_data['i16'] or { 0 }.u32() == 6
	assert sample_data['i32'] or { 0 }.u32() == 7
	assert sample_data['int'] or { 0 }.u32() == 8
	assert sample_data['i64'] or { 0 }.u32() == 9
	assert sample_data['f32'] or { 0 }.u32() == 2
	assert sample_data['f64'] or { 0 }.u32() == 1
	assert json.Any(true).u32() == 1
	assert json.Any('123').u32() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.u32() == 0
	assert sample_data['arr'] or { 0 }.u32() == 0
	assert sample_data['obj'] or { 0 }.u32() == 0
}

fn test_u64() {
	// valid conversions
	assert sample_data['u8'] or { 0 }.u64() == 1
	assert sample_data['u16'] or { 0 }.u64() == 2
	assert sample_data['u32'] or { 0 }.u64() == 3
	assert sample_data['u64'] or { 0 }.u64() == 4
	assert sample_data['i8'] or { 0 }.u64() == 5
	assert sample_data['i16'] or { 0 }.u64() == 6
	assert sample_data['i32'] or { 0 }.u64() == 7
	assert sample_data['int'] or { 0 }.u64() == 8
	assert sample_data['i64'] or { 0 }.u64() == 9
	assert sample_data['f32'] or { 0 }.u64() == 2
	assert sample_data['f64'] or { 0 }.u64() == 1
	assert json.Any(true).u64() == 1
	assert json.Any('123').u64() == 123
	// invalid conversions
	assert sample_data['null'] or { 0 }.u64() == 0
	assert sample_data['arr'] or { 0 }.u64() == 0
	assert sample_data['obj'] or { 0 }.u64() == 0
}

fn test_as_map() {
	assert sample_data['u8'] or { 0 }.as_map()['0'] or { 0 }.u8() == 1
	assert sample_data['u16'] or { 0 }.as_map()['0'] or { 0 }.u16() == 2
	assert sample_data['u32'] or { 0 }.as_map()['0'] or { 0 }.u32() == 3
	assert sample_data['u64'] or { 0 }.as_map()['0'] or { 0 }.u64() == 4
	assert sample_data['i8'] or { 0 }.as_map()['0'] or { 0 }.i8() == 5
	assert sample_data['i16'] or { 0 }.as_map()['0'] or { 0 }.i16() == 6
	assert sample_data['i32'] or { 0 }.as_map()['0'] or { 0 }.i32() == 7
	assert sample_data['int'] or { 0 }.as_map()['0'] or { 0 }.int() == 8
	assert sample_data['i64'] or { 0 }.as_map()['0'] or { 0 }.i64() == 9
	assert sample_data['f32'] or { 0 }.as_map()['0'] or { 0 }.f32() == 2.3
	assert sample_data['f64'] or { 0 }.as_map()['0'] or { 0 }.f64() == 1.283
	assert sample_data['bool'] or { 0 }.as_map()['0'] or { 0 }.bool() == false
	assert sample_data['str'] or { 0 }.as_map()['0'] or { 0 }.str() == 'test'
	assert is_null(sample_data['null'] or { 0 }.as_map()['0'] or { 0 }) == true
	assert sample_data['arr'] or { 0 }.as_map()['0'] or { 0 }.str() == 'lol'
	assert sample_data['obj'] or { 0 }.as_map()['foo'] or { 0 }.int() == 10
}

fn test_as_map_of_strings() {
	assert sample_data['obj']!.as_map() == {
		'foo': json.Any(10)
	}
	assert sample_data['obj']!.as_map_of_strings() == {
		'foo': '10'
	}
}

fn test_arr() {
	assert sample_data['u8'] or { 0 }.arr()[0].u8() == 1
	assert sample_data['u16'] or { 0 }.arr()[0].u16() == 2
	assert sample_data['u32'] or { 0 }.arr()[0].u32() == 3
	assert sample_data['u64'] or { 0 }.arr()[0].u64() == 4
	assert sample_data['i8'] or { 0 }.arr()[0].i8() == 5
	assert sample_data['i16'] or { 0 }.arr()[0].i16() == 6
	assert sample_data['i32'] or { 0 }.arr()[0].i32() == 7
	assert sample_data['int'] or { 0 }.arr()[0].int() == 8
	assert sample_data['i64'] or { 0 }.arr()[0].i64() == 9
	assert sample_data['f32'] or { 0 }.arr()[0].f32() == 2.3
	assert sample_data['f64'] or { 0 }.arr()[0].f64() == 1.283
	assert sample_data['bool'] or { 0 }.arr()[0].bool() == false
	assert sample_data['str'] or { 0 }.arr()[0].str() == 'test'
	assert is_null(sample_data['null'] or { 0 }.arr()[0]) == true
	assert sample_data['arr'] or { 0 }.arr()[0].str() == 'lol'
	assert sample_data['obj'] or { 0 }.arr()[0].int() == 10
}

fn test_bool() {
	// valid conversions
	assert sample_data['bool'] or { 0 }.bool() == false
	assert json.Any('true').bool() == true
	assert sample_data['u8'] or { 0 }.bool() == true
	assert sample_data['u16'] or { 0 }.bool() == true
	assert sample_data['u32'] or { 0 }.bool() == true
	assert sample_data['u64'] or { 0 }.bool() == true
	assert sample_data['i8'] or { 0 }.bool() == true
	assert sample_data['i16'] or { 0 }.bool() == true
	assert sample_data['i32'] or { 0 }.bool() == true
	assert sample_data['int'] or { 0 }.bool() == true
	assert sample_data['i64'] or { 0 }.bool() == true
	assert sample_data['f32'] or { 0 }.bool() == true
	assert sample_data['f64'] or { 0 }.bool() == true
	// invalid conversions
	assert sample_data['null'] or { 0 }.bool() == false
	assert sample_data['arr'] or { 0 }.bool() == false
	assert sample_data['obj'] or { 0 }.bool() == false
}

fn test_str() {
	assert sample_data['u8'] or { 0 }.str() == '1'
	assert sample_data['u16'] or { 0 }.str() == '2'
	assert sample_data['u32'] or { 0 }.str() == '3'
	assert sample_data['u64'] or { 0 }.str() == '4'
	assert sample_data['i8'] or { 0 }.str() == '5'
	assert sample_data['i16'] or { 0 }.str() == '6'
	assert sample_data['i32'] or { 0 }.str() == '7'
	assert sample_data['int'] or { 0 }.str() == '8'
	assert sample_data['i64'] or { 0 }.str() == '9'
	assert sample_data['f32'] or { 0 }.str() == '2.299999952316284'
	assert sample_data['f64'] or { 0 }.str() == '1.283'
	assert sample_data['bool'] or { 0 }.str() == 'false'
	assert sample_data['str'] or { 0 }.str() == 'test'
	assert sample_data['null'] or { 0 }.str() == 'null'
	assert sample_data['arr'] or { 'not lol' }.str() == '["lol"]'
	assert sample_data.str() == '{"u8":1,"u16":2,"u32":3,"u64":4,"i8":5,"i16":6,"i32":7,"int":8,"i64":9,"f32":2.299999952316284,"f64":1.283,"bool":false,"str":"test","null":null,"arr":["lol"],"obj":{"foo":10}}'
}
