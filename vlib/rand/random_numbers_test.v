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
		_ = rand.rand_r(&seed)
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

fn test_rand_u32n() {
	max := u32(16384)
	for _ in 0 .. rnd_count {
		value := rand.u32n(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_u64n() {
	max := u64(379091181005)
	for _ in 0 .. rnd_count {
		value := rand.u64n(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_u32_in_range() {
	max := u32(484468466)
	min := u32(316846)
	for _ in 0 .. rnd_count {
		value := rand.u32_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_u64_in_range() {
	max := u64(216468454685163)
	min := u64(6848646868)
	for _ in 0 .. rnd_count {
		value := rand.u64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_intn() {
	max := 2525642
	for _ in 0 .. rnd_count {
		value := rand.intn(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_i64n() {
	max := i64(3246727724653636)
	for _ in 0 .. rnd_count {
		value := rand.i64n(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_int_in_range() {
	min := -4252
	max := 23054962
	for _ in 0 .. rnd_count {
		value := rand.int_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_i64_in_range() {
	min := i64(-24095)
	max := i64(324058)
	for _ in 0 .. rnd_count {
		value := rand.i64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_int31() {
	max_u31 := 0x7FFFFFFF
	sign_mask := 0x80000000
	for _ in 0 .. rnd_count {
		value := rand.int31()
		assert value >= 0
		assert value <= max_u31
		// This statement ensures that the sign bit is zero
		assert (value & sign_mask) == 0
	}
}

fn test_rand_int63() {
	max_u63 := i64(0x7FFFFFFFFFFFFFFF)
	sign_mask := i64(0x8000000000000000)
	for _ in 0 .. rnd_count {
		value := rand.int63()
		assert value >= 0
		assert value <= max_u63
		assert (value & sign_mask) == 0
	}
}

fn test_rand_f32() {
	for _ in 0 .. rnd_count {
		value := rand.f32()
		assert value >= 0.0
		assert value < 1.0
	}
}

fn test_rand_f64() {
	for _ in 0 .. rnd_count {
		value := rand.f64()
		assert value >= 0.0
		assert value < 1.0
	}
}

fn test_rand_f32n() {
	max := f32(357.0)
	for _ in 0 .. rnd_count {
		value := rand.f32n(max)
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f64n() {
	max := f64(1.52e6)
	for _ in 0 .. rnd_count {
		value := rand.f64n(max)
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f32_in_range() {
	min := f32(-24.0)
	max := f32(125.0)
	for _ in 0 .. rnd_count {
		value := rand.f32_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_f64_in_range() {
	min := f64(-548.7)
	max := f64(5015.2)
	for _ in 0 .. rnd_count {
		value := rand.f64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}
