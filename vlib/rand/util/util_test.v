import rand.util

fn test_sample_nr() {
	length := 5
	a := ['one', 'two', 'three', 'four', 'five', 'six', 'seven']
	b := util.sample_nr(a, length)
	assert b.len == length
	for element in b {
		assert element in a
		// make sure every element occurs once
		mut count := 0
		for e in b {
			if e == element {
				count++
			}
		}
		assert count == 1
	}
}

fn test_sample_r() {
	k := 20
	a := ['heads', 'tails']
	b := util.sample_r(a, k)
	assert b.len == k
	for element in b {
		assert element in a
	}
}
