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

fn test_array_prepend_spread_keeps_concrete_array_as_one_interface_value() {
	values := prepend_strings()
	assert values.len == 2
	assert '${values}' == "[Value(['IT', 'US']), Value(['DE'])]"
	assert_array_value(values[0], ['IT', 'US'])
	assert_array_value(values[1], ['DE'])
}

fn test_array_insert_spread_keeps_concrete_array_as_one_interface_value() {
	values := insert_strings()
	assert values.len == 2
	assert '${values}' == "[Value(['DE']), Value(['IT', 'US'])]"
	assert_array_value(values[0], ['DE'])
	assert_array_value(values[1], ['IT', 'US'])
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
