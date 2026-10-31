// The null constants `nil`, `0` and `voidptr(0)` are rejected where they are converted
// implicitly to a reference outside `unsafe` (see
// vlib/v/checker/tests/null_constant_as_reference_err.vv). These forms stay valid.
struct Node {
	x int
}

struct Holder {
mut:
	node   &Node = unsafe { nil }
	opt    ?&Node
	handle voidptr = voidptr(0)
}

@[unsafe]
fn null_node() &Node {
	return unsafe { nil }
}

fn null_node_in_unsafe_block() &Node {
	unsafe {
		return voidptr(0)
	}
}

@[unsafe]
fn null_node_from_unsafe_expr() &Node {
	return unsafe { voidptr(0) }
}

fn no_node() ?&Node {
	return none
}

fn null_handle() voidptr {
	return voidptr(0)
}

fn zero_handle() voidptr {
	return 0
}

fn handle_is_null(handle voidptr) bool {
	return handle == voidptr(0)
}

// A `voidptr` value that is only known at run time still converts to a reference.
fn node_from_handle(handle voidptr) &Node {
	return handle
}

@[unsafe]
fn null_of[T]() &T {
	return unsafe { nil }
}

@[unsafe]
fn test_explicit_unsafe_null_references() {
	assert isnil(null_node())
	assert isnil(null_node_in_unsafe_block())
	assert isnil(null_node_from_unsafe_expr())
	assert isnil(null_of[Node]())
	mut p := &Node{
		x: 1
	}
	assert !isnil(p)
	p = unsafe { nil }
	assert isnil(p)
	unsafe {
		p = &Node{
			x: 2
		}
		p = nil
		assert isnil(p)
		p = voidptr(0)
		assert isnil(p)
	}
	h := Holder{
		node: unsafe { nil }
	}
	assert isnil(h.node)
}

@[unsafe]
fn test_comparisons_with_null_constants() {
	n := &Node{
		x: 3
	}
	assert n != voidptr(0)
	assert n != unsafe { nil }
	assert n != nil
	assert n != 0
	p := null_node()
	assert p == voidptr(0)
	assert p == unsafe { nil }
	assert p == nil
	assert p == 0
	if p != voidptr(0) {
		assert false
	}
}

fn test_option_references() {
	mut h := Holder{}
	assert h.opt == none
	h.opt = &Node{
		x: 4
	}
	if node := h.opt {
		assert node.x == 4
	} else {
		assert false
	}
	h.opt = none
	assert h.opt == none
	if _ := no_node() {
		assert false
	}
}

fn test_voidptr_targets() {
	mut h := Holder{}
	assert h.handle == voidptr(0)
	h.handle = voidptr(0)
	assert handle_is_null(h.handle)
	assert handle_is_null(voidptr(0))
	assert handle_is_null(0)
	assert handle_is_null(null_handle())
	assert handle_is_null(zero_handle())
	mut handle := voidptr(0)
	assert handle_is_null(handle)
	n := &Node{
		x: 5
	}
	handle = n
	assert !handle_is_null(handle)
	assert node_from_handle(handle).x == 5
	assert unsafe { &Node(handle) }.x == 5
}
