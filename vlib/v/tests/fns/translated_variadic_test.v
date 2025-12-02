@[translated]
module main

fn foo(args ...string) {
	println(args)
}

fn test_main() {
	foo(42.str(), 'bar')
}
