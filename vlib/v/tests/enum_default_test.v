enum TestEnum {
	two = 1
	one
}

struct Test {
	a TestEnum
	b TestEnum = .one
}

fn test_main() {
	assert dump(Test{}) == Test{
		a: .two
		b: .one
	}
	assert dump(Test{}) == Test{}

	assert dump(Test{
		a: .one
	}) == Test{
		a: .one
	}
	assert dump(Test{
		b: .two
	}) == Test{
		b: .two
	}
}
