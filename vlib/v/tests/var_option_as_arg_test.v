fn is_none(a ?string) bool {
	return a == none
}

fn is_map_none(a ?map[string]int) bool {
	return a == none
}

fn test_opt_call_arg() {
	assert is_none(?string(none))
	var := ?string('foo')
	assert is_none(var) == false

	mut var2 := ?map[string]int{}
	assert var2 == none
	var2 = {
		'a': 1
	}
	assert is_map_none(var2) == false
}
