struct Node {
	x int
}

const const_zero = 0

fn handle_is_null(handle voidptr) bool {
	return handle == C.NULL
}

fn take_voidptr(handle voidptr) bool {
	return handle == voidptr(0)
}

fn test_c_null_comparisons_stay_allowed() {
	assert C.NULL == C.NULL
	assert C.NULL == voidptr(0)
	assert voidptr(0) == C.NULL
	assert handle_is_null(C.NULL)
	assert handle_is_null(voidptr(0))
	assert take_voidptr(C.NULL)
}

fn test_c_null_explicit_unsafe_casts_stay_allowed() {
	p := unsafe { &Node(C.NULL) }
	assert isnil(p)
	q := unsafe { &Node(voidptr(0)) }
	assert isnil(q)
	r := unsafe { &int(1 - 1) }
	assert r == unsafe { nil }
	assert unsafe { nil } == C.NULL
	assert unsafe { nil } == voidptr(0)
}

fn test_typed_null_through_unsafe_nil() {
	mut n := &Node{
		x: 1
	}
	assert !isnil(n)
	n = unsafe { nil }
	assert isnil(n)
	assert n == unsafe { nil }
	assert n == C.NULL
	assert n == voidptr(0)
}

fn test_voidptr_zero_forms_stay_allowed() {
	zero := 0
	a := voidptr(zero)
	assert a == voidptr(0)
	assert a == C.NULL
	b := voidptr(0)
	assert b == C.NULL
	assert handle_is_null(a)
	assert handle_is_null(b)
	c := voidptr(const_zero)
	assert c == voidptr(0)
	assert c == C.NULL
	d := unsafe { nil }
	assert d == voidptr(0)
	assert d == C.NULL
}
