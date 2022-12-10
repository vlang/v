import x.json2 as json

const (
	sample_data = {
		'int':  json.Any(int(1))
		'i64':  json.Any(i64(128))
		'f32':  json.Any(f32(2.0))
		'f64':  json.Any(f64(1.283))
		'bool': json.Any(false)
		'str':  json.Any('test')
		'null': json.Any(json.null)
		'arr':  json.Any([json.Any('lol')])
		'obj':  json.Any({
			'foo': json.Any(10)
		})
	}
)

fn is_null(f json.Any) bool {
	match f {
		json.Null { return true }
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
	assert json.Any(true).int() == 1
	assert json.Any('123').int() == 123
	// invalid conversions
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
	assert json.Any(true).i64() == 1
	assert json.Any('123').i64() == 123
	// invalid conversions
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
	assert json.Any('true').bool() == true
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
