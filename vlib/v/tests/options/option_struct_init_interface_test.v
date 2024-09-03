interface IPerson {
	name string
	age  int
}

struct PersonImpl {
	name string
	age  int
}

struct Person {
	other ?IPerson = PersonImpl{}
}

fn test_main() {
	a := Person{}
	assert a.other != none
}
