fn f1() {
	println(1)
}

fn f2() {
	println(2)
}

fn f3(f fn ()) {
	f()
}

fn func_defer() {
	mut func := f1
	println('Before')
	defer {
		func()
	}
	defer {
		f3(func)
	}
	func = f2
	println('After')
	assert true
}

fn test_defer_with_fn_var() {
	func_defer()
}
