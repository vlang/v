struct Holder {
	q []int
}

fn test_empty_array_literal_has_nil_data() {
	a := []int{}
	assert isnil(a.data)
	assert a.len == 0
	assert a.cap == 0
}

fn test_default_struct_array_field_has_nil_data() {
	h := Holder{}
	assert isnil(h.q.data)
	assert h.q.len == 0
	assert h.q.cap == 0
}

fn test_empty_array_grows_on_push() {
	mut b := []int{}
	assert isnil(b.data)
	b << 42
	assert b.len == 1
	assert b[0] == 42
	assert !isnil(b.data)
}

fn test_empty_array_clone_has_nil_data() {
	a := []int{}
	c := a.clone()
	assert isnil(c.data)
	assert c.len == 0
	assert c.cap == 0
}

fn test_array_with_cap_still_allocates() {
	d := []int{cap: 5}
	assert !isnil(d.data)
	assert d.cap == 5
}

fn test_array_with_len_still_allocates() {
	e := []int{len: 3}
	assert !isnil(e.data)
	assert e.len == 3
}
