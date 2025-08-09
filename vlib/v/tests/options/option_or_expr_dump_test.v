fn f() ? {
	println('hello')
	return none
}

fn test_main() {
	f() or { dump(err) }
}
