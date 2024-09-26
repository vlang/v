const first_const = 1000
const second_const = 2000

enum Test {
	example = first_const
}

fn test_main() {
	a := Test.example
	dump(Test.example)
	assert a == unsafe { Test(1000) }
	assert Test.example == unsafe { Test(1000) }
}
