fn issue27903_value(x ?string) string {
	return x or { 'NONE' }
}

fn issue27903_condition(value bool) bool {
	return value
}

fn test_if_expression_passed_as_option_argument() {
	assert issue27903_value(if issue27903_condition(true) { 'a' } else { none }) == 'a'
	assert issue27903_value(if issue27903_condition(false) { 'a' } else { none }) == 'NONE'
}
