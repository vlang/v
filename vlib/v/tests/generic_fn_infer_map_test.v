fn print_map<T>(x map[string]T) string {
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
}
