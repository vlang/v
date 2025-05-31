fn make_option() ?string {
	return none
}

fn test_assign_option_of_array_index() {
	arr := [make_option()]
	unwrapped := arr[99] or { 'unknown' } // <- out of bounds access!
	assert '${unwrapped}' == "Option('unknown')"
}
