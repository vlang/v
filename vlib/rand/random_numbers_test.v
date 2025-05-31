import rand
import rand.splitmix64
import rand.musl
import rand.mt19937

const rnd_count = 40
const seeds = [[u32(42), 0], [u32(256), 0]]

fn get_n_random_ints(seed_data []u32, n int) []int {
	mut values := []int{cap: n}
	rand.seed(seed_data)
	for _ in 0 .. n {
		values << rand.intn(n) or { panic("Couldn't obtain int") }
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
		value := rand.u32n(max) or { panic("Couldn't obtain u32") }
		assert value >= 0
		assert value < max
	}
}

fn test_rand_u64n() {
	max := u64(379091181005)
	for _ in 0 .. rnd_count {
		value := rand.u64n(max) or { panic("Couldn't obtain u64") }
		assert value >= 0
		assert value < max
	}
}

fn test_rand_u32_in_range() {
	max := u32(484468466)
	min := u32(316846)
	for _ in 0 .. rnd_count {
		value := rand.u32_in_range(min, max) or { panic("Couldn't obtain u32 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_u64_in_range() {
	max := u64(216468454685163)
	min := u64(6848646868)
	for _ in 0 .. rnd_count {
		value := rand.u64_in_range(min, max) or { panic("Couldn't obtain u64 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_intn() {
	max := 2525642
	for _ in 0 .. rnd_count {
		value := rand.intn(max) or { panic("Couldn't obtain int") }
		assert value >= 0
		assert value < max
	}
}

fn test_rand_i32n() {
	max := i32(2525642)
	for _ in 0 .. rnd_count {
		value := rand.i32n(max) or { panic("Couldn't obtain i32") }
		assert value >= 0
		assert value < max
	}
}

fn test_rand_i64n() {
	max := i64(3246727724653636)
	for _ in 0 .. rnd_count {
		value := rand.i64n(max) or { panic("Couldn't obtain i64") }
		assert value >= 0
		assert value < max
	}
}

fn test_rand_int_in_range() {
	min := -4252
	max := 23054962
	for _ in 0 .. rnd_count {
		value := rand.int_in_range(min, max) or { panic("Couldn't obtain int in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_i32_in_range() {
	min := i32(-4252)
	max := i32(23054962)
	for _ in 0 .. rnd_count {
		value := rand.i32_in_range(min, max) or { panic("Couldn't obtain i32 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_i64_in_range() {
	min := i64(-24095)
	max := i64(324058)
	for _ in 0 .. rnd_count {
		value := rand.i64_in_range(min, max) or { panic("Couldn't obtain i64 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_int31() {
	max_u31 := int(0x7FFFFFFF)
	sign_mask := int(u32(0x80000000))
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
	sign_mask := i64(u64(0x8000000000000000))
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
		value := rand.f32n(max) or { panic("Couldn't obtain f32") }
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f64n() {
	max := f64(1.52e6)
	for _ in 0 .. rnd_count {
		value := rand.f64n(max) or { panic("Couldn't obtain f64") }
		assert value >= 0.0
		assert value < max
	}
}

fn test_rand_f32_in_range() {
	min := f32(-24.0)
	max := f32(125.0)
	for _ in 0 .. rnd_count {
		value := rand.f32_in_range(min, max) or { panic("Couldn't obtain f32 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_f64_in_range() {
	min := f64(-548.7)
	max := f64(5015.2)
	for _ in 0 .. rnd_count {
		value := rand.f64_in_range(min, max) or { panic("Couldn't obtain f64 in range") }
		assert value >= min
		assert value < max
	}
}

fn test_rand_u8() {
	mut all := []u8{}
	for _ in 0 .. 256 {
		x := rand.u8()
		assert x >= 0
		assert x <= 255
		all << x
	}
	all.sort(a < b)
	assert all[0] != all[255]
	assert all[0] != all[128]
}

fn test_rand_u16() {
	mut all := []u16{}
	mut same_as_previous := 0
	mut previous := u16(0)
	for _ in 0 .. 65536 {
		x := rand.u16()
		assert x >= 0
		assert x <= 65535
		all << x
		if previous == x {
			same_as_previous++
			// dump(previous)
			// dump(x)
		}
		previous = x
	}
	assert same_as_previous < 1000
	all.sort(a < b)
	assert all[0] != all[65535]
	assert all[0] != all[32768]
	// dump( all[0] )
	// dump( all[65535] )
	// dump( all[32768] )
}

const string_count = 25

fn test_rand_string_from_set() {
	sets := [
		'0123456789',
		'qwertyuiop',
		'abcdefghijklmnopqrstuvwxyz',
	]
	for charset in sets {
		for _ in 0 .. string_count {
			len := rand.intn(rnd_count) or { panic("Couldn't obtain int") }
			str := rand.string_from_set(charset, len)
			assert str.len == len
			for character in str {
				position := charset.index(character.ascii_str()) or { -1 }
				assert position > -1
			}
		}
	}
}

fn test_rand_fill_buffer_from_set() {
	rand.seed([u32(0), 1])
	outputs := [
		[u8(52), 48, 55, 57, 50, 49, 53, 49, 53, 53],
		[u8(57), 51, 56, 53, 56, 55, 56, 52, 56, 51],
		[u8(57), 54, 52, 53, 57, 56, 57, 57, 48, 57],
		[u8(57), 54, 50, 50, 52, 57, 53, 55, 50, 57],
		[u8(51), 48, 55, 54, 49, 55, 53, 54, 52, 57],
		[u8(57), 50, 48, 50, 48, 49, 52, 54, 50, 48],
		[u8(55), 54, 51, 48, 51, 54, 49, 55, 56, 52],
		[u8(52), 56, 52, 54, 50, 50, 50, 56, 54, 53],
		[u8(53), 53, 55, 52, 51, 54, 55, 56, 51, 51],
		[u8(52), 50, 51, 57, 54, 52, 50, 48, 49, 53],
		[u8(49), 51, 54, 57, 55, 51, 48, 51, 51, 50],
		[u8(56), 54, 50, 54, 51, 54, 49, 55, 57, 49],
	]
	for output in outputs {
		mut buf := []u8{len: 10}
		rand.fill_buffer_from_set('0123456789', mut buf)
		assert buf == output
	}
}

fn test_rand_string() {
	rand.seed([u32(0), 1])
	outputs := [
		'oIfPOHLBZTlvGhYtCMolfssbZ',
		'yHFGzDYeWIRldsBzMtkDhzQqF',
		'vwoeerAKsEZiludKtRKoCoiuE',
		'EQAaJDRZkvKTKNLkEPhWeEKFX',
		'rDIhxzIbDUIusiTuzLHRslfzu',
		'KCUoAEugYvUwzXcKRrAiwMzXH',
		'NIOXerfCpEwbfhLmbbWKjoxbL',
		'baJWQWarRRRmXCvMKcEjxQBpk',
		'CkVLxbJEPhviBTohEVBnMAFHZ',
		'ZdnGGhYShqzwnDXqHncLgLcdo',
		'zRiSLsgnApmvtlIVrQQaBzOJD',
		'VeeBcztImGquJnzEsXCdUaUed',
	]
	for output in outputs {
		assert rand.string(25) == output
	}
}

fn test_rand_hex() {
	rand.seed([u32(0), 1])
	outputs := [
		'ead1c993f5fdcb270ea39e69b',
		'453459a8ca7fbe31ef2531a47',
		'd6a449a86a38f4f4ff0206046',
		'62e4753bad85cb52a1fcce035',
		'99afb9e9de2868945d57a3514',
		'04a6e60621a2116cf92ce69d1',
		'f6490d14bee1935419cc92fd5',
		'58b0e841bbf01c568ee13ebf6',
		'caf5bdf21f94f5a7a3f5a6b9f',
		'bb908760b8121510516de9eb6',
		'93045e61ab45b7e3962c31c31',
		'bc07ed76c4c4b51eedc768a0b',
		'1b23e1d08a6ba3d32cc4c85ee',
		'96c44362a86d3e317eb56a053',
	]
	for output in outputs {
		assert rand.hex(25) == output
	}
}

fn test_rand_ascii() {
	rand.seed([u32(0), 1])
	outputs := [
		r"KqdNI|*bDh42kn'z-}}nhmKd~",
		r'IZ4wVRC-Q3@TviD>G4#Z(2}s4',
		r"l7'1Ute)i?4Efo$sX^sOk;s%m",
		r"3}3s^l(PeNY>I8&'a>$)AW14*",
		r'V.a^b>GN"\\9e-Vs"&.vS0"F_',
		r"U-;S}OY+e>Ca>p'UD|7{}?6`x",
		r'$/EN5*2w@/KdN~pU||c=*yn6|',
		r'FsLkK{gFrPn)>EVW53uJLa<8?',
		r'1#PB<"P}pLtY@F}^\TfNyCDB$',
	]
	for output in outputs {
		assert rand.ascii(25) == output
	}
}

fn ensure_same_output(mut rng rand.PRNG) {
	for _ in 0 .. 100 {
		assert rand.int() == rng.int()
		assert rand.i32() == rng.i32()
		assert rand.intn(45) or { 0 } == rng.intn(45) or { 0 }
		assert rand.i32n(45) or { 0 } == rng.i32n(45) or { 0 }
		assert rand.u64() == rng.u64()
		assert rand.f64() == rng.f64()
		assert rand.u32n(25) or { 0 } == rng.u32n(25) or { 0 }
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

fn test_shuffle() {
	mut arrays := [][]int{}
	arrays << [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	arrays << [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
	for seed in seeds {
		a := get_n_random_ints(seed, 10)
		arrays << a
	}
	mut digits := []map[int]int{len: 10}
	for digit in 0 .. 10 {
		digits[digit] = {}
		for idx in 0 .. 10 {
			digits[digit][idx] = 0
		}
	}
	for mut a in arrays {
		o := a.clone()
		for _ in 0 .. 100 {
			rand.shuffle(mut a) or { panic('shuffle failed') }
			assert *a != o
			for idx in 0 .. 10 {
				digits[idx][a[idx]]++
			}
		}
	}
	for digit in 1 .. 10 {
		assert digits[0] != digits[digit]
	}
	for digit in 0 .. 10 {
		for idx in 0 .. 10 {
			assert digits[digit][idx] >= 10
		}
		// eprintln('digits[$digit]: ${digits[digit]}')
	}
}

fn test_shuffle_partial() {
	mut a := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	mut b := a.clone()

	rand.shuffle(mut a, start: 4)!
	assert a[..4] == b[..4]

	a = b.clone()
	rand.shuffle(mut a, start: 3, end: 7)!
	assert a[..3] == b[..3]
	assert a[7..] == b[7..]
}

fn test_shuffle_clone() {
	original := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
	mut a := original.clone()
	mut results := [][]int{}
	for _ in 0 .. 10 {
		results << rand.shuffle_clone(a) or { panic('shuffle failed') }
	}
	assert original == a
	for idx in 1 .. 10 {
		assert results[idx].len == 10
		assert results[idx] != results[0]
		assert results[idx] != original
	}
}

fn test_choose() {
	lengths := [1, 3, 4, 5, 6, 7]
	a := ['one', 'two', 'three', 'four', 'five', 'six', 'seven']
	for length in lengths {
		b := rand.choose(a, length)!
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

fn test_sample() {
	k := 20
	a := ['heads', 'tails']
	b := rand.sample(a, k)
	assert b.len == k
	for element in b {
		assert element in a
	}
}

fn test_element1() {
	a := ['one', 'two', 'four', 'five', 'six', 'seven']
	for _ in 0 .. 30 {
		e := rand.element(a)!
		assert e in a
		assert 'three' != e
	}
}

fn test_element2() {
	for _ in 0 .. 30 {
		e := rand.element([1, 2, 5, 6, 7, 8])!
		assert e in [1, 2, 5, 6, 7, 8]
		assert 3 != e
		assert 4 != e
	}
}

fn test_proper_masking() {
	under32 := []int{len: 10, init: index * 0 + rand.intn(1073741823)!}
	assert under32 != [0].repeat(10)

	over32 := []int{len: 10, init: index * 0 + rand.intn(1073741824)!}
	assert over32 != [0].repeat(10)

	under64 := []i64{len: 10, init: index * 0 + rand.i64n(i64(4611686018427387903))!}
	assert under64 != [i64(0)].repeat(10)

	over64 := []i64{len: 10, init: index * 0 + rand.i64n(i64(4611686018427387904))!}
	assert over64 != [i64(0)].repeat(10)

	almost_full32 := []int{len: 10, init: index * 0 + rand.intn(2147483647)!}
	assert almost_full32 != [0].repeat(10)

	almost_full64 := []i64{len: 10, init: index * 0 + rand.i64n(i64(9223372036854775807))!}
	assert almost_full64 != [i64(0)].repeat(10)
}
