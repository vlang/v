struct Cmp[K] {
	cmp ?fn (K, K) bool
}

fn (c Cmp[K]) compare(a K, b K) bool {
	cmp := c.cmp or { return a < b }
	return cmp(a, b)
}

fn test_generic_struct_with_option_fn() {
	c := Cmp[int]{}
	print(c.compare(1, 2))
	assert c.compare(1, 2)
}
