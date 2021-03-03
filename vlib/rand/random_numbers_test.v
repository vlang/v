import rand
import rand.splitmix64
import rand.musl
import rand.mt19937

const (
	rnd_count = 40
	seeds     = [[u32(42), 0], [u32(256), 0]]
)

fn get_n_random_ints(seed_data []u32, n int) []int {
	mut values := []int{cap: n}
	rand.seed(seed_data)
	for _ in 0 .. n {
		values << rand.intn(n)
	}
	return values
}

fn test_rand_reproducibility() {
	for seed in seeds {
		array1 := get_n_random_ints(seed, 1000)
		array2 := get_n_random_ints(seed, 1000)
		assert array1.len == array2.len
		assert array1 == array2
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
	max_u31 := int(0x7FFFFFFF)
	sign_mask := int(0x80000000)
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

fn test_rand_byte() {
	mut all := []byte{}
	for _ in 0 .. 256 {
		x := rand.byte()
		assert x >= 0
		assert x <= 255
		all << x
	}
	all.sort(a < b)
	assert all[0] != all[255]
	assert all[0] != all[128]
}

const (
	string_count = 25
)

fn test_rand_string_from_set() {
	sets := [
		'0123456789',
		'qwertyuiop',
		'abcdefghijklmnopqrstuvwxyz',
	]
	for charset in sets {
		for _ in 0 .. string_count {
			len := rand.intn(rnd_count)
			str := rand.string_from_set(charset, len)
			assert str.len == len
			for character in str {
				position := charset.index(character.ascii_str()) or { -1 }
				assert position > -1
			}
		}
	}
}

fn test_rand_string() {
	rand.seed([u32(0), 1])
	outputs := [
		'rzJfVBJgvAyCNpEdXIteDQezg',
		'AJOeswgoelDOCfcrSUWzVPjeL',
		'NQfKauQqsXYXSUMFPGnXXPJIn',
		'vfBGUKbpLoBMQVYXfkvRplWih',
		'aYHLjMJqvUJmJJHGxEnrEmQGl',
		'rBJXkQZcembAteaRFoxXmECJo',
		'HYVLfHmDOCTlSbiSzHrsAIaBH',
		'zgOiwyISjLSdLGhLzJsSKHVBi',
		'UiAtobWXGcHsEtgzuNatxfkoI',
		'NisnYlffJgFEcIdcgzWcGjnHy',
	]
	for output in outputs {
		assert rand.string(25) == output
	}
}

fn test_rand_hex() {
	rand.seed([u32(0), 1])
	outputs := [
		'fc30e495deee09e008e15ffc3',
		'4320efa837788397fb59b28f4',
		'4995210abf33b6765c240ce62',
		'f3d20dbe0a8aa6b9c88cd1f6f',
		'8d7d58b256ab00213dd519cf7',
		'fa2251284bc20a21eff48127c',
		'5fef90cdc0c37143117599092',
		'2a6170531c76dfb50c54126bc',
		'a686dfd536042d1c1a9afdaf4',
		'7f12013f6e1177e2d63726de3',
	]
	for output in outputs {
		assert rand.hex(25) == output
	}
}

fn test_rand_ascii() {
	rand.seed([u32(0), 1])
	outputs := [
		"2Z:&PeD'V;9=mn\$C>yKg'DIr%",
		'Ub7ix,}>I=&#2QJki{%FHKv&K',
		'1WStRylMO|p.R~qqRtr&AOEsd',
		'yka<GPZ&m+r0^Zi!ShB*1dU~W',
		'uDA?.zU2X,<DkKT#_-halW\\ki',
		'fsx!@uRc?re/fSPXj`Y&\\BU}p',
		'fI_qM"):2;CUno!<dX:Yv*FX$',
		'FnA(Fr|D`WZVWEzp<k)O;auub',
		"QRkxH!kjXh&/j{)uSe&{D'v?|",
		"_CyaU\$z':#}At*v2|xDu6w=;1",
	]
	for output in outputs {
		assert rand.ascii(25) == output
	}
}

fn ensure_same_output(mut rng rand.PRNG) {
	for _ in 0 .. 100 {
		assert rand.int() == rng.int()
		assert rand.intn(45) == rng.intn(45)
		assert rand.u64() == rng.u64()
		assert rand.f64() == rng.f64()
		assert rand.u32n(25) == rng.u32n(25)
	}
}

fn test_new_global_rng() {
	old := rand.get_current_rng()

	// MuslRNG	
	mut rng1a := musl.MuslRNG{}
	mut rng1b := musl.MuslRNG{}
	seed1 := [u32(1234)]

	rand.set_rng(rng1a)
	rand.seed(seed1)
	rng1b.seed(seed1)
	ensure_same_output(mut rng1b)

	// SplitMix64RNG
	mut rng2a := splitmix64.SplitMix64RNG{}
	mut rng2b := splitmix64.SplitMix64RNG{}
	seed2 := [u32(2325), 14]

	rand.set_rng(rng2a)
	rand.seed(seed2)
	rng2b.seed(seed2)
	ensure_same_output(mut rng2b)

	// MT19937RNG
	mut rng3a := mt19937.MT19937RNG{}
	mut rng3b := mt19937.MT19937RNG{}
	seed3 := [u32(0xcafe), 234]

	rand.set_rng(rng3a)
	rand.seed(seed3)
	rng3b.seed(seed3)
	ensure_same_output(mut rng3b)

	rand.set_rng(old)
}
