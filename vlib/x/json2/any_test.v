import x.json2

const (
	sample_data = map{
		'int':  json2.Any(int(1))
		'i64':  json2.Any(i64(128))
		'f32':  json2.Any(f32(2.0))
		'f64':  json2.Any(f64(1.283))
		'bool': json2.Any(false)
		'str':  json2.Any('test')
		'null': json2.Any(json2.null)
		'arr':  json2.Any([json2.Any('lol')])
		'obj':  json2.Any(map{
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
	assert sample_data['int'].f32() == 1.0
	assert sample_data['i64'].f32() == 128.0
	assert sample_data['f32'].f32() == 2.0
	assert sample_data['f64'].f32() == 1.2829999923706055
	// invalid conversions
	assert sample_data['bool'].f32() == 0.0
	assert sample_data['str'].f32() == 0.0
	assert sample_data['null'].f32() == 0.0
	assert sample_data['arr'].f32() == 0.0
	assert sample_data['obj'].f32() == 0.0
}

fn test_f64() {
	// valid conversions
	assert sample_data['int'].f64() == 1.0
	assert sample_data['i64'].f64() == 128.0
	assert sample_data['f32'].f64() == 2.0
	assert sample_data['f64'].f64() == 1.283
	// invalid conversions
	assert sample_data['bool'].f64() == 0.0
	assert sample_data['str'].f64() == 0.0
	assert sample_data['null'].f64() == 0.0
	assert sample_data['arr'].f64() == 0.0
	assert sample_data['obj'].f64() == 0.0
}

fn test_int() {
	// valid conversions
	assert sample_data['int'].int() == 1
	assert sample_data['i64'].int() == 128
	assert sample_data['f32'].int() == 2
	assert sample_data['f64'].int() == 1
	assert json2.Any(true).int() == 1
	// invalid conversions
	assert json2.Any('123').int() == 0
	assert sample_data['null'].int() == 0
	assert sample_data['arr'].int() == 0
	assert sample_data['obj'].int() == 0
}

fn test_i64() {
	// valid conversions
	assert sample_data['int'].i64() == 1
	assert sample_data['i64'].i64() == 128
	assert sample_data['f32'].i64() == 2
	assert sample_data['f64'].i64() == 1
	assert json2.Any(true).i64() == 1
	// invalid conversions
	assert json2.Any('123').i64() == 0
	assert sample_data['null'].i64() == 0
	assert sample_data['arr'].i64() == 0
	assert sample_data['obj'].i64() == 0
}

fn test_as_map() {
	assert sample_data['int'].as_map()['0'].int() == 1
	assert sample_data['i64'].as_map()['0'].i64() == 128.0
	assert sample_data['f32'].as_map()['0'].f32() == 2.0
	assert sample_data['f64'].as_map()['0'].f64() == 1.283
	assert sample_data['bool'].as_map()['0'].bool() == false
	assert sample_data['str'].as_map()['0'].str() == 'test'
	assert is_null(sample_data['null'].as_map()['0']) == true
	assert sample_data['arr'].as_map()['0'].str() == 'lol'
	assert sample_data['obj'].as_map()['foo'].int() == 10
}

fn test_arr() {
	assert sample_data['int'].arr()[0].int() == 1
	assert sample_data['i64'].arr()[0].i64() == 128.0
	assert sample_data['f32'].arr()[0].f32() == 2.0
	assert sample_data['f64'].arr()[0].f64() == 1.283
	assert sample_data['bool'].arr()[0].bool() == false
	assert sample_data['str'].arr()[0].str() == 'test'
	assert is_null(sample_data['null'].arr()[0]) == true
	assert sample_data['arr'].arr()[0].str() == 'lol'
	assert sample_data['obj'].arr()[0].int() == 10
}

fn test_bool() {
	// valid conversions
	assert sample_data['bool'].bool() == false
	assert json2.Any('true').bool() == true
	// invalid conversions
	assert sample_data['int'].bool() == false
	assert sample_data['i64'].bool() == false
	assert sample_data['f32'].bool() == false
	assert sample_data['f64'].bool() == false
	assert sample_data['null'].bool() == false
	assert sample_data['arr'].bool() == false
	assert sample_data['obj'].bool() == false
}

fn test_str() {
	assert sample_data['int'].str() == '1'
	assert sample_data['i64'].str() == '128'
	assert sample_data['f32'].str() == '2.0'
	assert sample_data['f64'].str() == '1.283'
	assert sample_data['bool'].str() == 'false'
	assert sample_data['str'].str() == 'test'
	assert sample_data['null'].str() == 'null'
	assert sample_data['arr'].str() == '["lol"]'
	assert sample_data.str() == '{"int":1,"i64":128,"f32":2.0,"f64":1.283,"bool":false,"str":"test","null":null,"arr":["lol"],"obj":{"foo":10}}'
}
