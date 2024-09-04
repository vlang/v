struct Foo {
	index int
}

const foo5 = Foo{
	index: 5
}
const foo2 = Foo{
	index: 2
}

const foos = [foo5.index, foo2.index]!

fn test_main() {
	fooz := [foo5.index, foo2.index]!
	assert foos == fooz
}
