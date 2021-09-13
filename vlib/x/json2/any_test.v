import x.json2

const (
	sample_data = {
		'int':  json2.Any(int(1))
		'i64':  json2.Any(i64(128))
		'f32':  json2.Any(f32(2.0))
		'f64':  json2.Any(f64(1.283))
		'bool': json2.Any(false)
		'str':  json2.Any('test')
		'null': json2.Any(json2.null)
		'arr':  json2.Any([json2.Any('lol')])
		'obj':  json2.Any({
			'foo': json2.Any(10)
		})
	}
)

fn is_null(f json2.Any) bool {
	match f {
		json2.Null { return true }
		else { return false }
	}
}

fn test_f32() {
	// valid conversions
	assert sample_data['int'] or { 0 }.f32() == 1.0
	assert sample_data['i64'] or { 0 }.f32() == 128.0
	assert sample_data['f32'] or { 0 }.f32() == 2.0
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
	assert sample_data['int'] or { 0 }.f64() == 1.0
	assert sample_data['i64'] or { 0 }.f64() == 128.0
	assert sample_data['f32'] or { 0 }.f64() == 2.0
	assert sample_data['f64'] or { 0 }.f64() == 1.283
	// invalid conversions
	assert sample_data['bool'] or { 0 }.f64() == 0.0
	assert sample_data['str'] or { 0 }.f64() == 0.0
	assert sample_data['null'] or { 0 }.f64() == 0.0
	assert sample_data['arr'] or { 0 }.f64() == 0.0
	assert sample_data['obj'] or { 0 }.f64() == 0.0
}

fn test_int() {
	// valid conversions
	assert sample_data['int'] or { 0 }.int() == 1
	assert sample_data['i64'] or { 0 }.int() == 128
	assert sample_data['f32'] or { 0 }.int() == 2
	assert sample_data['f64'] or { 0 }.int() == 1
	assert json2.Any(true).int() == 1
	// invalid conversions
	assert json2.Any('123').int() == 0
	assert sample_data['null'] or { 0 }.int() == 0
	assert sample_data['arr'] or { 0 }.int() == 0
	assert sample_data['obj'] or { 0 }.int() == 0
}

fn test_i64() {
	// valid conversions
	assert sample_data['int'] or { 0 }.i64() == 1
	assert sample_data['i64'] or { 0 }.i64() == 128
	assert sample_data['f32'] or { 0 }.i64() == 2
	assert sample_data['f64'] or { 0 }.i64() == 1
	assert json2.Any(true).i64() == 1
	// invalid conversions
	assert json2.Any('123').i64() == 0
	assert sample_data['null'] or { 0 }.i64() == 0
	assert sample_data['arr'] or { 0 }.i64() == 0
	assert sample_data['obj'] or { 0 }.i64() == 0
}

fn test_as_map() {
	assert sample_data['int'] or { 0 }.as_map()['0'] or { 0 }.int() == 1
	assert sample_data['i64'] or { 0 }.as_map()['0'] or { 0 }.i64() == 128.0
	assert sample_data['f32'] or { 0 }.as_map()['0'] or { 0 }.f32() == 2.0
	assert sample_data['f64'] or { 0 }.as_map()['0'] or { 0 }.f64() == 1.283
	assert sample_data['bool'] or { 0 }.as_map()['0'] or { 0 }.bool() == false
	assert sample_data['str'] or { 0 }.as_map()['0'] or { 0 }.str() == 'test'
	assert is_null(sample_data['null'] or { 0 }.as_map()['0'] or { 0 }) == true
	assert sample_data['arr'] or { 0 }.as_map()['0'] or { 0 }.str() == 'lol'
	assert sample_data['obj'] or { 0 }.as_map()['foo'] or { 0 }.int() == 10
}

fn test_arr() {
	assert sample_data['int'] or { 0 }.arr()[0].int() == 1
	assert sample_data['i64'] or { 0 }.arr()[0].i64() == 128.0
	assert sample_data['f32'] or { 0 }.arr()[0].f32() == 2.0
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
	assert json2.Any('true').bool() == true
	// invalid conversions
	assert sample_data['int'] or { 0 }.bool() == false
	assert sample_data['i64'] or { 0 }.bool() == false
	assert sample_data['f32'] or { 0 }.bool() == false
	assert sample_data['f64'] or { 0 }.bool() == false
	assert sample_data['null'] or { 0 }.bool() == false
	assert sample_data['arr'] or { 0 }.bool() == false
	assert sample_data['obj'] or { 0 }.bool() == false
}

fn test_str() {
	assert sample_data['int'] or { 0 }.str() == '1'
	assert sample_data['i64'] or { 0 }.str() == '128'
	assert sample_data['f32'] or { 0 }.str() == '2.0'
	assert sample_data['f64'] or { 0 }.str() == '1.283'
	assert sample_data['bool'] or { 0 }.str() == 'false'
	assert sample_data['str'] or { 0 }.str() == 'test'
	assert sample_data['null'] or { 0 }.str() == 'null'
	assert sample_data['arr'] or { 0 }.str() == '["lol"]'
	assert sample_data.str() == '{"int":1,"i64":128,"f32":2.0,"f64":1.283,"bool":false,"str":"test","null":null,"arr":["lol"],"obj":{"foo":10}}'
}
