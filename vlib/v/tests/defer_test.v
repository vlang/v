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

fn set_num(i int, mut n Num) {
	defer {
		println('exiting')
		n.val++
	}
	println('Hi')
	if i < 5 {
		return
	} else {
		n.val++
	}
}

fn set_num_opt(mut n Num) ?int {
	defer {
		n.val = 1
	}
	return 99
}

struct Num {
mut:
	val int
}

fn (n Num) add(i int) int {
	return n.val + i
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

fn test_defer_with_anon_fn() {
	mut f := &Num{
		val: 110
	}
	defer {
		assert f.add(1) == 111
	}

	go fn () {
		defer {
			println('deferred 1')
		}
	}()
	x := fn () {
		defer {
			println('defered 2')
		}
		return
	}
	x()
	return
}

fn set_num_if(mut n Num, v int, cond bool) {
	if cond {
		defer {
			n.val = v
		}
	}
}

fn test_defer_with_if() {
	mut n := Num{0}
	set_num_if(mut n, 10, true)
	assert n.val == 10
	set_num_if(mut n, 20, false)
	assert n.val == 10
}

fn test_defer_order() {
	mut i := 0
	defer {
		i++
		assert i == 3
	}
	defer {
		i++
		assert i == 2
	}
	defer {
		i++
		assert i == 1
	}
}
