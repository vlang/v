struct Ab {
	a []int
}

struct ComplexStruct {
	a int
	b []Ab
	c string
	d map[string]int
}

fn encode_struct[T](val T) []string {
	mut out := []string{}
	$for field in T.fields {
		value := val.$(field.name)
		out << field.name
		out << typeof(value).idx.str()
		out << typeof(value).name
	}
	return out
}

fn test_typeof_in_comptime_for_in_fields() {
	s := ComplexStruct{}
	mut out := encode_struct(s)
	dump(out)
	assert out[0] == 'a'
	assert out[1] == typeof[int]().idx.str()
	assert out[2] == typeof[int]().name
	assert out[3] == 'b'
	assert out[4] == typeof[[]Ab]().idx.str()
	assert out[5] == typeof[[]Ab]().name
	assert out[6] == 'c'
	assert out[7] == typeof[string]().idx.str()
	assert out[8] == typeof[string]().name
	assert out[9] == 'd'
	assert out[10] == typeof[map[string]int]().idx.str()
	assert out[11] == typeof[map[string]int]().name
}
