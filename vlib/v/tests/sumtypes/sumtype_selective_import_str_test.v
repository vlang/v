import json2 { Any }

fn test_selectively_imported_sum_type_keeps_its_custom_str() {
	value := Any(1)
	assert value.str() == '1'
	assert '${value}' == '1'
	assert [value].str() == '[1]'
	assert dump(value) == value
}
