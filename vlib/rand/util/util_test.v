import rand
import rand.util

fn test_sample_nr() {
	lengths := [1, 3, 4, 5, 6, 7]
	a := ['one', 'two', 'three', 'four', 'five', 'six', 'seven']
	for length in lengths {
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

fn test_shuffle() {
	rand.seed([u32(1), 2]) // set seed to produce same results in order
	a := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	mut b := a.clone()
	mut c := a.clone()
	util.shuffle(mut b, 0)
	util.shuffle(mut c, 0)
	assert b == [6, 4, 5, 1, 9, 2, 10, 3, 8, 7]
	assert c == [1, 6, 5, 8, 7, 2, 10, 9, 3, 4]
	// test shuffling a slice
	mut d := a.clone()
	util.shuffle(mut d[..5], 0)
	assert d == [5, 2, 1, 3, 4, 6, 7, 8, 9, 10]
	assert d[5..] == a[5..]
	// test shuffling n items
	mut e := a.clone()
	util.shuffle(mut e, 5)
	assert e[..5] == [10, 3, 1, 8, 4]
	assert e[5..] == [6, 7, 5, 9, 2]
	// test shuffling empty array
	mut f := a[..0]
	util.shuffle(mut f, 0)
	assert f == []int{}
}
