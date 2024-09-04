fn foo(mut a &int) ? {
	println('XXX')
	a++
}

fn test_main() {
	mut a := 1
	println(foo(mut &a))
	assert a == 2
}
