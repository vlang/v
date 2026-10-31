interface Value {}

fn prepend_strings() []Value {
	mut values := [Value(['DE'])]
	strings := ['IT', 'US']
	values.prepend(...strings)
	return values
}

fn insert_strings() []Value {
	mut values := [Value(['DE'])]
	strings := ['IT', 'US']
	values.insert(1, ...strings)
	return values
}

fn assert_array_value(value Value, expected []string) {
	assert value is []string
	assert value == expected
}

fn test_array_prepend_spread_boxes_each_concrete_element() {
	values := prepend_strings()
	assert values.len == 3
	assert '${values}' == "[Value('IT'), Value('US'), Value(['DE'])]"
	assert values[0] as string == 'IT'
	assert values[1] as string == 'US'
	assert_array_value(values[2], ['DE'])
}

fn test_array_insert_spread_boxes_each_concrete_element() {
	values := insert_strings()
	assert values.len == 3
	assert '${values}' == "[Value(['DE']), Value('IT'), Value('US')]"
	assert_array_value(values[0], ['DE'])
	assert values[1] as string == 'IT'
	assert values[2] as string == 'US'
}

fn test_array_prepend_spread_inserts_existing_interface_elements() {
	mut values := [Value('DE')]
	strings := [Value('IT'), Value('US')]
	values.prepend(...strings)
	assert values.len == 3
	assert '${values}' == "[Value('IT'), Value('US'), Value('DE')]"
}

fn test_array_insert_spread_inserts_existing_interface_elements() {
	mut values := [Value('DE')]
	strings := [Value('IT'), Value('US')]
	values.insert(1, ...strings)
	assert values.len == 3
	assert '${values}' == "[Value('DE'), Value('IT'), Value('US')]"
}
