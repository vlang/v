// The bindings of a multi-value guard in a returned `if` can leave the function by address
// (V1 generates invalid C for the address of such a binding).
fn maybe_pair(v int) ?(int, int) {
	if v == 0 {
		return none
	}
	return v, v * 2
}

@[noinline]
fn overwrite_stack() int {
	mut filler := [64]int{}
	for i in 0 .. filler.len {
		filler[i] = 0x55555555
	}
	return filler[7]
}

fn larger_binding(v int) &int {
	return if a, b := maybe_pair(v) {
		p := if a > b { &a } else { &b }
		p
	} else {
		&int(unsafe { nil })
	}
}

fn second_binding_changed_after_address(v int) &int {
	return if a, mut b := maybe_pair(v) {
		p := &b
		b += a
		p
	} else {
		&int(unsafe { nil })
	}
}

fn test_multiple_bindings_in_returned_if() {
	p := larger_binding(6)
	q := larger_binding(-6)
	r := second_binding_changed_after_address(6)
	overwrite_stack()
	assert *p == 12
	assert *q == -6
	assert *r == 18
	assert isnil(larger_binding(0))
	assert isnil(second_binding_changed_after_address(0))
}
