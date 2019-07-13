import rand

fn trial(seed int) []int {
	mut randoms := [0; 20]
	rand.seed(seed)
	for _ in 0..20 {
		randoms << rand.next(100)
	}
	return randoms
}

fn test_rand_reproducibility() {
	mut randoms1 := trial(42)
	mut randoms2 := trial(42)
	assert randoms1.len == randoms2.len

	mut len := randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}

	randoms1 = trial(256)
	randoms2 = trial(256)
	assert randoms1.len == randoms2.len

	len = randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}
}
