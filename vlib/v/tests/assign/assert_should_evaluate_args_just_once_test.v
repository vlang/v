struct Abc {
	ncalls int
}

struct AssertCounter {
mut:
	value int
}

@[unsafe]
fn fn_that_should_be_called_just_once_array() []int {
	mut static ncalls := 0
	ncalls++
	println('${@FN} calls: ${ncalls}')
	if ncalls > 1 {
		assert false
	}
	return []int{len: ncalls, init: ncalls}
}

@[unsafe]
fn fn_that_should_be_called_just_once_struct() Abc {
	mut static ncalls := 0
	ncalls++
	println('${@FN} calls: ${ncalls}')
	if ncalls > 1 {
		assert false
	}
	return Abc{
		ncalls: ncalls
	}
}

fn (mut c AssertCounter) next() int {
	c.value++
	return c.value
}

fn test_assert_calls_a_function_returning_an_array_just_once() {
	unsafe {
		assert fn_that_should_be_called_just_once_array().len == 1
	}
}

fn test_assert_calls_a_function_returning_a_struct_just_once() {
	unsafe {
		assert fn_that_should_be_called_just_once_struct().ncalls == 1
	}
}

fn test_assert_array_literal_keeps_call_order_when_if_expr_needs_tmp() {
	mut c := AssertCounter{}
	assert [c.next(), if c.value > 0 {
		_ := 1
		c.next()
	} else {
		0
	}] == [1, 2]
}
