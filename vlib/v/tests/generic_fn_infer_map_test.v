fn print_map<K, V>(x map[K]V) string {
	println(x)
	return '${x}'
}

fn test_generics_infer_map_type() {
	m1 := {
		'one': 1
	}
	assert print_map(m1) == "{'one': 1}"

	m2 := {
		'one': 1.1
	}
	assert print_map(m2) == "{'one': 1.1}"

	m3 := {
		'one': 'a'
	}
	assert print_map(m3) == "{'one': 'a'}"

	m4 := {
		1: 'one'
	}
	assert print_map(m4) == "{1: 'one'}"

	m5 := {
		1.1: 'one'
	}
	assert print_map(m5) == "{1.1: 'one'}"

	m6 := {
		'a': 'one'
	}
	assert print_map(m6) == "{'a': 'one'}"
}
