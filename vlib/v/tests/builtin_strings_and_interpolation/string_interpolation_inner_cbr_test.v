struct St {}

fn foo() ?int {
	return 22
}

fn test_string_interpolation_inner_cbr() {
	s1 := '${foo() or { 11 }}'
	println(s1)
	assert s1 == '22'

	s2 := '${St{}}'
	println(s2)
	assert s2 == 'St{}'

	s3 := '${{
		'a': 1
	}}'
	println(s3)
	assert s3 == "{'a': 1}"
}
