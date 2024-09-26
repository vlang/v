fn f(n int) ?int {
	if n < 0 {
		return none
	}
	return n
}

fn test_lhs_option() {
	mut a := [0, 1, 2, 3, 4]
	a[f(-1) or { 2 }] = 7
	assert a == [0, 1, 7, 3, 4]
}

fn ret_no_opt(n int) int {
	return f(n) or { panic(err) }
}

fn test_opt_return_no_opt() {
	aa := ret_no_opt(3)
	assert aa == 3
}

fn test_range_for_and_array_push() {
	mut a := []int{}
	for n in f(-3) or { -1 } .. f(3) or { 12 } {
		a << f(n) or { -5 }
	}
	assert a == [-5, 0, 1, 2]
}

fn test_channel_push() {
	ch := chan f64{cap: 2}
	ch <- 12.25
	ch.close()
	mut res := []f64{cap: 3}
	for _ in 0 .. 3 {
		res << <-ch or { -6.75 }
	}
	assert res == [12.25, -6.75, -6.75]
}

fn test_thread_wait() {
	thrs := [
		spawn f(3),
		spawn f(-7),
		spawn f(12),
	]
	mut res := []int{cap: 3}
	for t in thrs {
		res << t.wait() or { -13 }
	}
	assert res == [3, -13, 12]
}

fn test_nested_opt() {
	a := f(f(f(-3) or { -7 }) or { 4 }) or { 17 }
	assert a == 4
}

fn foo() ?string {
	return 'hi'
}

fn test_opt_subexp_field() {
	assert foo()?.len == 2
}
