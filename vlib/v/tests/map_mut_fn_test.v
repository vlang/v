fn print_all(mut m map[string]string) {
	for k, v in m {
		println(k)
		println(v)
		m[k] = 'foo'
	}
	assert m['test'] == 'foo'
}

fn test_map_mutable_arg() {
	mut m := map[string]string{}
	m['test'] = 'test'

	for k, v in m {
		println(k)
		println(v)
	}

	print_all(mut m)
}
