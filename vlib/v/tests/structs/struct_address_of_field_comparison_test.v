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

type DataRef = &Data

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

fn references_are_equal(a &Data, b &Data) bool {
	return a == b
}

// vfmt off
fn parenthesized_references_are_equal(a &Data, b &Data) bool {
	return (a) == (b)
}
// vfmt on

fn reference_aliases_are_equal(a DataRef, b DataRef) bool {
	return a == b
}

fn test_reference_parameters_compare_addresses() {
	a := Data{}
	b := Data{}
	assert !references_are_equal(a, b)
	assert references_are_equal(a, a)
	assert !parenthesized_references_are_equal(a, b)
	assert parenthesized_references_are_equal(a, a)
	assert !reference_aliases_are_equal(&a, &b)
	assert reference_aliases_are_equal(&a, &a)

	compare := fn (left &Data, right &Data) bool {
		return left == right
	}
	assert !compare(a, b)
	assert compare(a, a)

	alias_compare := fn (left DataRef, right DataRef) bool {
		return left == right
	}
	assert !alias_compare(&a, &b)
	assert alias_compare(&a, &a)
}

fn test_indexed_lvalue_addresses_compare_addresses() {
	items := [Data{}, Data{}]
	assert &items[0] != &items[1]
	assert &items[0] == &items[0]
}

fn test_dereferenced_lvalue_addresses_compare_addresses() {
	a := Data{}
	b := Data{}
	p := &a
	q := &b
	assert &*p != &*q
	assert &*p == &*p
}
