fn g[T](arr []T) {
	mut r := []T{}
	r << arr
	assert arr.len > 0
}

fn test_generic_array_append() {
	g([1, 2, 3])
	g([1.1, 2.2, 3.3])
	g(['aa', 'bb', 'cc'])
	gs[[]string]()!
}

fn gs[T]() !T {
	mut typ := T{
		cap: 10
	}
	if T.name == '[]string' {
		typ << 'strings'
	} else {
		return error('only string arrays are supported')
	}
	return typ
}
