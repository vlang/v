type Duration = i64

struct Abc {
	d Duration
}

fn test_string_interpolation_of_alias() {
	x := Abc{
		d: i64(9_123_456_789)
	}
	assert '${x}' == 'Abc{\n    d:     Duration(9123456789)\n}'
}
