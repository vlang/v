type SS = string

struct ST {
	data &SS
}

fn test_struct_with_reference_alias_fields() {
	mut val := ST{
		data: &SS('hi')
	}
	println(val.data)
	assert '${val.data}' == 'hi'
}
