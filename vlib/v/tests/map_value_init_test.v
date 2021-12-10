struct Test {
	s string
}

fn test_map_value_init() {
	m := map[string]Test{}
	empty := m['not-here']
	assert !isnil(empty.s.str)
}
