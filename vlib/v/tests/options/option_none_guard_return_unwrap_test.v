fn guarded_name_len(name ?string) int {
	if name == none {
		return 0
	}
	return name.len
}

fn test_option_none_guard_return_unwrap() {
	assert guarded_name_len(none) == 0
	assert guarded_name_len('guarded') == 7
}
