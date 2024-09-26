struct AA {
	a shared []int
}

fn test_shared_str() {
	a := AA{
		a: [1, 2]
	}
	assert a.str() == 'AA{\n    a: [1, 2]\n}'
}
