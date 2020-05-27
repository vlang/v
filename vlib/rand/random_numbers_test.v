import rand

const (
	rnd_count = 40
	seeds     = [42, 256]
)

fn test_rand_reproducibility() {
	for seed in seeds {
		mut randoms1 := gen_randoms(seed)
		mut randoms2 := gen_randoms(seed)
		assert_randoms_equal(randoms1, randoms2)
	}
}

fn test_rand_r_reproducibility() {
	for seed in seeds {
		mut randoms1 := gen_randoms_r(seed)
		mut randoms2 := gen_randoms_r(seed)
		assert_randoms_equal(randoms1, randoms2)
	}
}

fn test_rand_r_seed_update() {
	seed := 10
	for _ in 0 .. rnd_count {
		prev_seed := seed
		_ := rand.rand_r(&seed)
		assert prev_seed != seed
	}
}

fn gen_randoms(seed int) []int {
	mut randoms := [0].repeat(rnd_count)
	rand.seed(seed)
	for i in 0 .. rnd_count {
		randoms[i] = rand.next(100)
	}
	return randoms
}

fn gen_randoms_r(seed int) []int {
	mut randoms := [0].repeat(rnd_count)
	for i in 0 .. rnd_count {
		randoms[i] = rand.rand_r(&seed)
	}
	return randoms
}

fn assert_randoms_equal(r1, r2 []int) {
	for i in 0 .. rnd_count {
		assert r1[i] == r2[i]
	}
}

fn test_rand_f32() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f32(100.0)
			assert res >= 0.0
			assert res < 100.0
		}
	}
}

fn test_rand_f32_in_range() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f32_in_range(1.0, 2048.0)
			assert res >= 1.0
			assert res < 2048.0
		}
	}
}

fn test_rand_f64() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f64(100.0)
			assert res >= 0.0
			assert res < 100.0
		}
	}
}

fn test_rand_f64_in_range() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f64_in_range(1.0, 2048.0)
			assert res >= 1.0
			assert res < 2048.0
		}
	}
}

fn test_rand_uniform_f32() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_uniform_f32()
			assert res >= 0.0
			assert res < 1.0
		}
	}
}

fn test_rand_uniform_f64() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_uniform_f64()
			assert res >= 0.0
			assert res < 1.0
		}
	}
}
