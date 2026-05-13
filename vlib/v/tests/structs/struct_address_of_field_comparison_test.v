// Regression test for https://github.com/vlang/v/issues/27089
// Comparing a stored reference with `&expr` should compare addresses,
// not perform a deep struct equality check.
struct Data {
mut:
	a f32
	b int
	c string
}

struct Holder {
mut:
	buf Data
	ref &Data = unsafe { nil }
}

fn test_address_of_field_compares_addresses() {
	mut t := Holder{}
	// `t.ref` is nil, so address comparison must be true (nil != &t.buf).
	assert t.ref != &t.buf
	t.ref = &t.buf
	// Now both refer to the same address.
	assert t.ref == &t.buf
	other := Data{}
	// Different addresses with structurally equal contents must still be unequal.
	assert &other != &t.buf
}
