// Casting a string rvalue (a slice or a concatenation) into a sumtype must
// materialize the rvalue into a temporary before taking its address, otherwise
// cgen emits `&<rvalue>` which strict C compilers reject. See issue #27548.
type Value = int | string

fn slice(s string) Value {
	return Value(s[1..3])
}

fn paren_slice(s string) Value {
	// `is_lvalue()` recurses through parentheses, so a parenthesized slice must
	// also be materialized via ADDR.
	return Value((s[1..3]))
}

fn slice_len(s string) Value {
	// a selector read rooted in a slice rvalue (`(s[a..b]).len`) is still an
	// rvalue; `is_lvalue()` recurses through selectors, so it must use ADDR too.
	return Value((s[1..3]).len)
}

struct Foo {
	name string
}

fn slice_elem_field(arr []Foo) Value {
	// an index + selector read rooted in an array-slice rvalue.
	return Value(arr[1..3][0].name)
}

fn concat(a string, b string) Value {
	return Value(a + b)
}

fn test_string_slice_cast_to_sumtype() {
	v := slice('hello')
	assert v is string
	assert (v as string) == 'el'
}

fn test_string_paren_slice_cast_to_sumtype() {
	v := paren_slice('hello')
	assert v is string
	assert (v as string) == 'el'
}

fn test_int_slice_len_cast_to_sumtype() {
	v := slice_len('hello')
	assert v is int
	assert (v as int) == 2
}

fn test_string_slice_elem_field_cast_to_sumtype() {
	v := slice_elem_field([Foo{'a'}, Foo{'b'}, Foo{'c'}, Foo{'d'}])
	assert v is string
	assert (v as string) == 'b'
}

fn test_string_concat_cast_to_sumtype() {
	v := concat('foo', 'bar')
	assert v is string
	assert (v as string) == 'foobar'
}
