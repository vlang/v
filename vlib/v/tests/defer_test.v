fn foo() string {
	println('foo()')
	return 'foo'
}

fn foo2() string {
	println('start')
	defer {
		println('defer')
	}
	defer {
		println('defer2')
	}
	println('end')
	return foo()
}

fn test_defer() {
	assert foo2() == 'foo'
}

fn set_num(i int, n mut Num) {
	defer {
		println('exiting')
		n.val++
	}
	println('Hi')
	if i < 5 {
		return
	}
	else {
		n.val++
	}
}

fn set_num_opt(n mut Num) ?int {
	defer {
		n.val = 1
	}
	return 99
}

struct Num {
mut:
	val int
}

fn test_defer_early_exit() {
	mut sum := Num{0}
	for i in 0 .. 10 {
		set_num(i, mut sum)
	}
	println('sum: $sum.val')
	assert sum.val == 15
}

fn test_defer_option() {
	mut ok := Num{0}
	set_num_opt(mut ok) or {}
	assert ok.val == 1
}
