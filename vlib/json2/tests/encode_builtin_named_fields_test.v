import json2

struct BuiltinNamedFields {
	i64    i64
	u64    u64
	f32    f32
	f64    f64
	int    int
	i8     i8
	u8     u8
	i16    i16
	bool   bool
	string string
	rest   []string
}

struct SingleBuiltinNamedField {
	i64 i64 @[json: 'number']
}

struct EmbeddedBuiltinNamedFields {
	BuiltinNamedFields
	extra string
}

fn test_encode_fields_named_after_builtin_types() {
	value := BuiltinNamedFields{
		i64:    1
		u64:    2
		f32:    3
		f64:    4
		int:    5
		i8:     6
		u8:     7
		i16:    8
		bool:   true
		string: 'text'
		rest:   ['a']
	}
	expected := '{"i64":1,"u64":2,"f32":3,"f64":4,"int":5,"i8":6,"u8":7,"i16":8,"bool":true,"string":"text","rest":["a"]}'
	assert json2.encode(value) == expected
	assert json2.encode(SingleBuiltinNamedField{ i64: 9 }) == '{"number":9}'
	assert json2.encode(EmbeddedBuiltinNamedFields{
		BuiltinNamedFields: value
		extra:              'z'
	}) == expected[..expected.len - 1] + ',"extra":"z"}'
	$for field in BuiltinNamedFields.fields {
		assert !field.is_embed
	}
}
