struct Counter {
mut:
	value int
}

fn (mut c Counter) next() int {
	c.value++
	return c.value
}

fn test_array_init_keeps_call_order_when_if_expr_needs_tmp() {
	mut c := Counter{}
	actual := [c.next(), if c.value > 0 {
		_ := 1
		c.next()
	} else {
		0
	}]
	assert actual == [1, 2]
}

fn test_fixed_array_init_keeps_call_order_when_if_expr_needs_tmp() {
	mut c := Counter{}
	actual := [c.next(), if c.value > 0 {
		_ := 1
		c.next()
	} else {
		0
	}]!
	assert actual == [1, 2]!
}
