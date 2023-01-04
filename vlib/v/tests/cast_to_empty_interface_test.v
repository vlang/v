interface Any {}

fn thing(any Any) string {
	return match any {
		int { 'int${*any:17}' }
		f64 { 'f64${*any:20}' }
		else { 'literal type tag?${any:10}' }
	}
}

fn test_cast_to_empty_interface() {
	mut ret_strings := []string{}
	mut arr := [Any(int(11)), int(22), Any(8888), 9999, Any(f64(1.11)), f64(2.22), Any(8.88), 9.99]

	for i in arr {
		println(thing(i))
		ret_strings << thing(i)
	}

	assert ret_strings.len == 8
	assert ret_strings[0] == 'int               11'
	assert ret_strings[1] == 'int               22'
	assert ret_strings[2] == 'int             8888'
	assert ret_strings[3] == 'int             9999'
	assert ret_strings[4] == 'f64               1.110'
	assert ret_strings[5] == 'f64               2.220'
	assert ret_strings[6] == 'f64               8.880'
	assert ret_strings[7] == 'f64               9.990'
}
