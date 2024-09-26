fn generic1[K, V](a map[K]V) string {
	t := expect_map[K, V](a)
	dump(t)
	return '${t}'
}

fn generic2[K, V](a map[K]V) string {
	t := expect_map(a)
	dump(t)
	return '${t}'
}

fn expect_map[K, V](a map[K]V) map[K]V {
	return a
}

fn test_generics_with_generics_fn_return_map_type() {
	a := {
		'a': 1
	}
	b := {
		1: 'a'
	}
	assert generic1(a) == "{'a': 1}"
	assert generic1(b) == "{1: 'a'}"
	assert generic2(a) == "{'a': 1}"
	assert generic2(b) == "{1: 'a'}"
}
