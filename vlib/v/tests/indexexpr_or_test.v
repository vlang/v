struct Element {
	children []int
}

fn f() []Element {
	return [Element{
		children: [1, 2, 3]
	}]
}

fn test_main() {
	a := f()[0]!.children[0]!.str()
	assert dump(a) == '1'
}
