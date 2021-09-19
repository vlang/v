fn test_is_letter() {
	for ra in `a` .. `z` {
		assert ra.is_letter() == true
	}

	for ra in `A` .. `Z` {
		assert ra.is_letter() == true
	}

	assert `ɀ`.is_letter() == true
	assert `ȶ`.is_letter() == true
	assert `ȹ`.is_letter() == true
}
