fn foo() {
	a := [u8(1), 2, 3]!
	defer {
		println('deffered println: ${a}')
		assert a == [u8(1), 2, 3]!
	}
}

fn test_main() {
	foo()
}
