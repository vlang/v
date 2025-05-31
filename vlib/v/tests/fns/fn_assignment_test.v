fn test() int {
	return 10
}

fn test1() int {
	return 11
}

fn test_fn_assignment_var() {
	mut a := 0
	mut b := 0
	a, b = test(), test1()

	assert a == 10
	assert b == 11

	a, b = test(), test()
	assert a == 10
	assert b == 10

	a, b = test(), 12

	assert a == 10
	assert b == 12

	a, b = 12, test()

	assert a == 12
	assert b == 10
}

fn test_fn_assignment_array() {
	mut a := [0, 1]

	a[0], a[1] = test(), test1()

	assert a[0] == 10
	assert a[1] == 11

	a[0], a[1] = test(), test()

	assert a[0] == 10
	assert a[1] == 10

	a[0], a[1] = test(), 12

	assert a[0] == 10
	assert a[1] == 12

	a[0], a[1] = 12, test()
	assert a[0] == 12
	assert a[1] == 10
}

fn test_fn_variables_can_be_assigned_pointers() {
	mut fn_ptr := fn (_ voidptr, _ u64) {}
	// println(voidptr(fn_ptr))
	assert fn_ptr != unsafe { nil }
	fn_ptr = unsafe { nil }
	// aprintln(voidptr(fn_ptr))
	assert fn_ptr == unsafe { nil }
}
