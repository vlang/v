import rand

fn gen_randoms(seed int) []int {
	mut randoms := [0].repeat(20)
	rand.seed(seed)
	for i in 0..20 {
		randoms[i] = rand.next(100)
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

fn gen_randoms_r(seed int) []int {
	mut randoms := [0].repeat(20)
	for i in 0..20 {
		randoms[i] = rand.rand_r(&seed)
	}
	return randoms
}

fn test_rand_r_reproducibility() {
	mut randoms1 := gen_randoms_r(42)
	mut randoms2 := gen_randoms_r(42)
	assert randoms1.len == randoms2.len

	mut len := randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}

	randoms1 = gen_randoms_r(256)
	randoms2 = gen_randoms_r(256)
	assert randoms1.len == randoms2.len

	len = randoms1.len
	for i in 0..len {
		assert randoms1[i] == randoms2[i]
	}
}
