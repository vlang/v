import json2

// `Any` here is a main sum type, distinct from `json2.Any`.
type Any = string | f32 | bool

fn test_main_sum_type_str_does_not_use_the_imported_homonym() {
	values := [Any('hi'), Any(true)]
	assert values.str() == "[Any('hi'), Any(true)]"
	assert '${Any(true)}' == 'Any(true)'
	assert json2.Any('x').str() == 'x'
}

fn test_decode_into_main_sum_type_named_any() {
	assert json2.decode[Any]('"hi"')! == Any('hi')
	assert json2.decode[Any]('true')! == Any(true)
	assert json2.decode[Any]('1.5')! == Any(f32(1.5))
}
