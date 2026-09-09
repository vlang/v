module main

enum TestEnum {
	one
	two
	_max
}

const te_max = int(TestEnum._max)

struct TestStruct {
mut:
	list [te_max]int
}
