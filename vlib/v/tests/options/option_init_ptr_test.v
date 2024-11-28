struct Abc {
	a int
}

fn test_option_init() {
	a := ?Abc{
		a: 1
	}
	dump(a)
	b := &?Abc{
		a: 1
	}
	dump(b)

	assert *b? == a?
}

fn test_option_empty() {
	a := ?Abc{}
	dump(a)
	b := &?Abc{}
	dump(b)

	assert a == none
	assert b == none
}
