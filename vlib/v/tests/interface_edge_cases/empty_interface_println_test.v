interface IEmpty {
}

struct Abc {
	abc int
}

fn test_an_empty_interface_can_be_printed() {
	println(IEmpty(Abc{123}))
	assert true
}
