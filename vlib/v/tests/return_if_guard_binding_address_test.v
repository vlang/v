// A guard binding in a returned `if` has the storage it has in a guard statement: when
// its address leaves the function it lives on the heap, so the pointer stays valid and
// keeps naming the binding rather than a copy of it.
struct Counter {
mut:
	n int
}

fn maybe_counter(v int) ?Counter {
	if v == 0 {
		return none
	}
	return Counter{v}
}

fn checked_counter(v int) !Counter {
	if v == 0 {
		return error('zero')
	}
	return Counter{v}
}

fn maybe_int(v int) ?int {
	if v == 0 {
		return none
	}
	return v
}

fn checked_int(v int) !int {
	if v == 0 {
		return error('zero')
	}
	return v
}

@[noinline]
fn overwrite_stack() int {
	mut filler := [64]int{}
	for i in 0 .. filler.len {
		filler[i] = 0x55555555
	}
	return filler[7]
}

fn option_binding(v int) &Counter {
	return if x := maybe_counter(v) { &x } else { &Counter(unsafe { nil }) }
}

fn result_binding(v int) &Counter {
	return if x := checked_counter(v) { &x } else { &Counter(unsafe { nil }) }
}

fn option_binding_changed_after_address(v int) &Counter {
	return if mut x := maybe_counter(v) {
		p := &x
		x.n += 100
		p
	} else {
		&Counter(unsafe { nil })
	}
}

fn result_binding_changed_after_address(v int) &Counter {
	return if mut x := checked_counter(v) {
		p := &x
		x.n += 100
		p
	} else {
		&Counter(unsafe { nil })
	}
}

fn option_int_changed_after_address(v int) &int {
	return if mut x := maybe_int(v) {
		p := &x
		x += 100
		p
	} else {
		&int(unsafe { nil })
	}
}

fn result_int_changed_after_address(v int) &int {
	return if mut x := checked_int(v) {
		p := &x
		x += 100
		p
	} else {
		&int(unsafe { nil })
	}
}

fn else_if_binding(v int) &int {
	return if v < 0 {
		&int(unsafe { nil })
	} else if mut x := maybe_int(v) {
		p := &x
		x += 100
		p
	} else {
		&int(unsafe { nil })
	}
}

fn binding_beside_another_value(v int) (&int, int) {
	return if mut x := maybe_int(v) {
		p := &x
		x += 100
		p
	} else {
		&int(unsafe { nil })
	}, v * 2
}

fn binding_in_option_return(v int) ?&int {
	return if mut x := maybe_int(v) {
		p := &x
		x += 100
		p
	} else {
		none
	}
}

fn result_error_in_else(v int) string {
	return if x := checked_int(v) { 'value ${x}' } else { 'error ${err.msg()}' }
}

fn test_option_binding_address_outlives_the_function() {
	p := option_binding(7)
	q := option_binding(8)
	overwrite_stack()
	assert p.n == 7
	assert q.n == 8
	assert isnil(option_binding(0))
}

fn test_result_binding_address_outlives_the_function() {
	p := result_binding(9)
	q := result_binding(10)
	overwrite_stack()
	assert p.n == 9
	assert q.n == 10
	assert isnil(result_binding(0))
}

fn test_address_names_the_binding_not_a_copy() {
	a := option_binding_changed_after_address(1)
	b := result_binding_changed_after_address(2)
	c := option_int_changed_after_address(3)
	d := result_int_changed_after_address(4)
	overwrite_stack()
	assert a.n == 101
	assert b.n == 102
	assert *c == 103
	assert *d == 104
}

fn test_guard_in_else_if_of_returned_if() {
	p := else_if_binding(5)
	overwrite_stack()
	assert *p == 105
	assert isnil(else_if_binding(-1))
	assert isnil(else_if_binding(0))
}

fn test_guard_in_return_with_several_values() {
	p, n := binding_beside_another_value(6)
	overwrite_stack()
	assert *p == 106
	assert n == 12
	q, m := binding_beside_another_value(0)
	assert isnil(q)
	assert m == 0
}

fn test_guard_in_returned_if_of_option_fn() {
	p := binding_in_option_return(7) or { panic('expected a value') }
	overwrite_stack()
	assert *p == 107
	if _ := binding_in_option_return(0) {
		assert false
	}
}

fn test_else_of_returned_result_guard_sees_the_error() {
	assert result_error_in_else(3) == 'value 3'
	assert result_error_in_else(0) == 'error zero'
}
