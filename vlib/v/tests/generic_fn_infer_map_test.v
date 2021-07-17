fn print_map<K, V>(x map[K]V) string {
	println(x)
	return '$x'
}

fn test_generics_infer_map_type() {
	m1 := map{
		'one': 1
	}
	assert print_map(m1) == "{'one': 1}"

	m2 := map{
		'one': 1.1
	}
	assert print_map(m2) == "{'one': 1.1}"

	m3 := map{
		'one': 'a'
	}
	assert print_map(m3) == "{'one': 'a'}"

	m4 := map{
		1: 'one'
	}
	assert print_map(m4) == "{1: 'one'}"

	m5 := map{
		1.1: 'one'
	}
	assert print_map(m5) == "{1.1: 'one'}"

	m6 := map{
		'a': 'one'
	}
	assert print_map(m6) == "{'a': 'one'}"
}
