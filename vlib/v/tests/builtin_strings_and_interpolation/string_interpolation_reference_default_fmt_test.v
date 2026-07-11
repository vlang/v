// Reference printing follows Go's `%v` rule: a pointer to a scalar
// (int, float, bool, string, rune) prints as its address, while a pointer to a
// struct/array/map prints as `&` followed by the pointed-to value.
// See https://github.com/vlang/v/issues/23461 and #15405.

struct Point {
	x int
	y int
}

fn test_scalar_reference_prints_as_address() {
	i := 5
	s := 'Hello'
	b := true
	f := 1.5
	r := `A`
	// `${&x}` for a scalar equals `ptr_str` - the raw address, no `&`, no value
	assert '${&i}' == ptr_str(&i)
	assert '${&s}' == ptr_str(&s)
	assert '${&b}' == ptr_str(&b)
	assert '${&f}' == ptr_str(&f)
	assert '${&r}' == ptr_str(&r)
	sp := &s
	assert '${sp}' == ptr_str(sp)
}

fn test_compound_reference_prints_as_value() {
	p := Point{1, 2}
	arr := [1, 2, 3]
	m := {
		'a': 1
	}
	assert '${&p}' == '&${p}'
	assert '${&arr}' == '&${arr}'
	assert '${&m}' == '&${m}'
}
