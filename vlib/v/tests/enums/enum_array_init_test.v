enum Enum {
	thing = 10
}

fn test_enum_array_init() {
	x := []Enum{len: 10, init: .thing}
	assert x[0] == .thing
	assert x.all(it == .thing)
}
