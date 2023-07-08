type Mat = [2][2]int

fn test_main() {
	a := Mat([[1, 2]!, [3, 4]!]!)
	println(a.foo())
	z := a.foo()
	assert z == [[1, 2]!, [3, 4]!]!
}

fn (v Mat) foo() [2][2]int {
	return v
}

fn bar() [2][2]int {
	return Mat([[1, 2]!, [3, 4]!]!)
}
