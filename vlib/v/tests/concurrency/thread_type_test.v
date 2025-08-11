fn f() ! {
	println('hi')
}

fn g() int {
	println('hi')
	return 23
}

fn test_main() {
	_ = go f()
	_ := spawn g()
}
