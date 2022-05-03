// vtest flaky: true
// vtest retry: 3

fn f(x f64) f64 {
	y := x * x
	return y
}

fn test_array_thread_f64_wait() {
	mut r := []thread f64{cap: 10}
	for i in 0 .. 10 {
		r << go f(f64(i) + 0.5)
	}
	x := r.wait()
	assert x == [0.25, 2.25, 6.25, 12.25, 20.25, 30.25, 42.25, 56.25, 72.25, 90.25]
}

fn g(shared a []int, i int) {
	lock a {
		a[i] *= a[i] + 1
	}
}

fn test_array_thread_void_wait() {
	shared a := [2, 3, 5, 7, 11, 13, 17]
	t := [
		go g(shared a, 0),
		go g(shared a, 3),
		go g(shared a, 6),
		go g(shared a, 2),
		go g(shared a, 1),
		go g(shared a, 5),
		go g(shared a, 4),
	]
	println('threads started')
	t.wait()
	rlock a {
		assert a == [6, 12, 30, 56, 132, 182, 306]
	}
}

fn test_void_thread_decl() {
	shared a := [2, 3, 9]
	mut t1 := thread(0)
	mut tarr := []thread{len: 2}
	t1 = go g(shared a, 0)
	tarr[0] = go g(shared a, 1)
	tarr[1] = go g(shared a, 2)
	t1.wait()
	tarr.wait()
	rlock a {
		assert a == [6, 12, 90]
	}
}
