fn parenthesized_int(fail bool) !int {
	if fail { return error('failed') }
	return 7
}

fn parenthesized_strings(fail bool) ![]string {
	if fail { return error('failed') }
	return ['a']
}

fn test_parenthesized_result_or_method_calls() {
	assert (parenthesized_int(false) or { 0 }).str() == '7'
	assert (parenthesized_int(true) or { 0 }).str() == '0'
	assert (parenthesized_strings(false) or { [] }).str() == "['a']"
	assert (parenthesized_strings(true) or { [] }).str() == '[]'
}
