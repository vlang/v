struct Test {}

struct Test2 {
	a [10]?Test
}

struct Test3 {
	a [10]?Test
}

fn test_main() {
	a := Test2{}
	b := Test3{}
	assert dump(Test2{}) == a
	assert dump(Test3{}) == b
}
