import rand

fn gen_randoms(seed int) []int {
	mut randoms := [0; 20]
	rand.seed(seed)
	for _ in 0..20 {
		randoms << rand.next(100)
	}
	return randoms
}

fn test_rand_reproducibility() {
	mut randoms1 := gen_randoms(42)
	mut randoms2 := gen_randoms(42)
	assert randoms1.len == randoms2.len

	mut len := randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}

	randoms1 = gen_randoms(256)
	randoms2 = gen_randoms(256)
	assert randoms1.len == randoms2.len

	len = randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}
}
