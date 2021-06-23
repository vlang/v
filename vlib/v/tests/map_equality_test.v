fn test_map_equality() {
	a1 := map{
		'a': 1
		'b': 2
	}
	b1 := map{
		'b': 2
		'a': 1
	}
	c1 := map{
		'a': 2
		'b': 1
	}

	assert a1 == b1
	assert a1 != c1

	a2 := map{
		'a': 1
	}
	b2 := map{
		'a': 1
		'b': 2
	}

	assert a2 != b2

	a3 := map{
		'a': '1'
		'b': '2'
	}
	b3 := map{
		'b': '2'
		'a': '1'
	}

	assert a3 == b3
}
