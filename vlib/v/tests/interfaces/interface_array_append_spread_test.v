interface Value {}

type Node = int | string

fn test_spread_appends_every_element_to_interface_array() {
	mut params := []Value{}
	codes := ['IT', 'US']
	params << ...codes
	assert params.len == 2
	assert params[0] as string == 'IT'
	assert params[1] as string == 'US'
}

fn test_plain_append_still_pushes_the_whole_array() {
	mut params := []Value{}
	codes := ['IT']
	params << codes
	assert params.len == 1
	assert params[0] as []string == ['IT']
}

fn test_spread_of_array_literal_or_call_into_interface_array() {
	mut params := []Value{}
	params << ...['IT', 'US']
	assert params.len == 2
	assert params[1] as string == 'US'
	params << ...more()
	assert params.len == 3
	assert params[2] as string == 'DE'
}

fn test_spread_of_matching_element_types_is_unchanged() {
	mut params := []Value{}
	values := [Value('IT'), Value('US')]
	params << ...values
	assert params.len == 2
	mut plain := []string{}
	plain << ...['IT', 'US']
	assert plain == ['IT', 'US']
}

fn test_spread_of_int_elements_into_sum_type_array() {
	mut nodes := []Node{}
	numbers := [1, 2]
	nodes << ...numbers
	assert nodes.len == 2
	assert nodes[0] as int == 1
	assert nodes[1] as int == 2
}

fn test_insert_spread_boxes_every_element() {
	mut params := []Value{}
	params << 'kept'
	params.insert(0, ...['IT', 'US'])
	assert params.len == 3
	assert params[0] as string == 'IT'
	assert params[1] as string == 'US'
	assert params[2] as string == 'kept'
}

fn test_prepend_spread_boxes_every_element() {
	mut params := []Value{}
	params << 'kept'
	params.prepend(...['IT'])
	assert params.len == 2
	assert params[0] as string == 'IT'
	assert params[1] as string == 'kept'
}

fn test_map_value_append_with_spread() {
	mut params := map[string][]Value{}
	params['codes'] = []Value{}
	params['codes'] << ...['IT']
	assert params['codes'].len == 1
	assert params['codes'][0] as string == 'IT'
}

fn more() []string {
	return ['DE']
}
