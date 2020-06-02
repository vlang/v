import rand
import math

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

fn test_rand_f32_old() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f32(100.0)
			assert res >= 0.0
			assert res < 100.0
		}
	}
}

fn test_rand_f32_in_range_old() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f32_in_range(1.0, 2048.0)
			assert res >= 1.0
			assert res < 2048.0
		}
	}
}

fn test_rand_f64_old() {
	for seed in seeds {
		rand.seed(seed)
		for _ in 0 .. rnd_count {
			res := rand.rand_f64(100.0)
			assert res >= 0.0
			assert res < 100.0
		}
	}
}

fn test_rand_f64_in_range_old() {
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

fn test_rand_u32n() {
	max := u32(4287502)
	for _ in 0 .. rnd_count {
		assert rand.u32n(max) < max
	}
}

fn test_rand_u64n() {
	max := u64(23442353534587502)
	for _ in 0 .. rnd_count {
		assert rand.u64n(max) < max
	}
}

fn test_rand_u32_in_range() {
	min := u32(5256)
	max := u32(4287502)
	for _ in 0 .. rnd_count {
		value := rand.u32_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_u64_in_range() {
	min := u64(4265266246)
	max := u64(23442353534587502)
	for _ in 0 .. rnd_count {
		value := rand.u64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_intn() {
	max := 720948723
	for _ in 0 .. rnd_count {
		value := rand.intn(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_i64n() {
	max := i64(209487239094)
	for _ in 0 .. rnd_count {
		value := rand.i64n(max)
		assert value >= 0
		assert value < max
	}
}

fn test_rand_int_in_range() {
	min := -34058
	max := -10542
	for _ in 0 .. rnd_count {
		value := rand.int_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_i64_in_range() {
	min := i64(-5026245)
	max := i64(209487239094)
	for _ in 0 .. rnd_count {
		value := rand.i64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_int31() {
	for _ in 0 .. rnd_count {
		value := rand.int31()
		assert value >= 0
		assert value <= math.max_i32
	}
}

fn test_rand_int63() {
	for _ in 0 .. rnd_count {
		value := rand.int63()
		assert value >= 0
		assert value <= math.max_i64
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
	max := f32(34.52)
	for _ in 0 .. rnd_count {
		value := rand.f32n(max)
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f64n() {
	max := 3495.2
	for _ in 0 .. rnd_count {
		value := rand.f64n(max)
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f32_in_range() {
	min := f32(-10.4)
	max := f32(43.2)
	for _ in 0 .. rnd_count {
		value := rand.f32_in_range(min, max)
		assert value >= min
		assert value < max
	}
}

fn test_rand_f64_in_range() {
	min := -10980.4
	max := -2.0
	for _ in 0 .. rnd_count {
		value := rand.f64_in_range(min, max)
		assert value >= min
		assert value < max
	}
}