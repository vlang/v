struct Id {
	v int
}

fn cmp(a ?Id, b ?Id) bool {
	return a != b
}

fn test_cmp() {
	assert cmp(Id{ v: 1 }, Id{
		v: 2
	})
}
