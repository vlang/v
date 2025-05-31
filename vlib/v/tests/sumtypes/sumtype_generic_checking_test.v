type Sumtype = string | int

fn generic_fn[T]() ?T {
	a := Sumtype('123')
	if a is T {
		return a
	}

	b := Sumtype(123)
	if b is T {
		return b
	}

	return none
}

fn test_main() {
	assert '${generic_fn[string]()}' == "Option('123')"
	assert '${generic_fn[int]()}' == 'Option(123)'
}
