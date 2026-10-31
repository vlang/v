import json2

type Any = int | string

fn test_local_sum_type_str_does_not_use_same_named_imported_method() {
	text := Any('hi')
	number := Any(7)
	assert text.str() == "Any('hi')"
	assert number.str() == 'Any(7)'
	assert '${text}' == "Any('hi')"
	assert '${number}' == 'Any(7)'
	println(text)
	assert dump(text) == text
	assert json2.Any(1).str() == '1'
	assert '${json2.Any(1)}' == '1'
}

fn test_local_sum_type_collections_keep_the_local_str_method() {
	values := [Any('hi'), Any(7)]
	assert values.str() == "[Any('hi'), Any(7)]"
	assert '${values}' == "[Any('hi'), Any(7)]"
	assert dump(values) == values
	assert [json2.Any(1)].str() == '[1]'
}
