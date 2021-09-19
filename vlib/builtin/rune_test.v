fn test_is_letter() {
	for ra in `a` .. `z` {
		assert ra.is_letter() == true
	}

	for ra in `A` .. `Z` {
		assert ra.is_letter() == true
	}
}
