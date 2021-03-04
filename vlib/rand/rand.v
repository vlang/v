// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import rand.seed
import rand.wyrand
import time

// PRNGConfigStruct is a configuration struct for creating a new instance of the default RNG.
// Note that the RNGs may have a different number of u32s required for seeding. The default
// generator WyRand used 64 bits, ie. 2 u32s so that is the default. In case your desired generator
// uses a different number of u32s, use the `seed.time_seed_array()` method with the correct
// number of u32s.
pub struct PRNGConfigStruct {
	seed []u32 = seed.time_seed_array(2)
}

// PRNG is a common interface for all PRNGs that can be used seamlessly with the rand
// modules's API. It defines all the methods that a PRNG (in the vlib or custom made) must
// implement in order to ensure that _all_ functions can be used with the generator.
pub interface PRNG {
	seed(seed_data []u32)
	u32() u32
	u64() u64
	u32n(max u32) u32
	u64n(max u64) u64
	u32_in_range(min u32, max u32) u32
	u64_in_range(min u64, max u64) u64
	int() int
	i64() i64
	int31() int
	int63() i64
	intn(max int) int
	i64n(max i64) i64
	int_in_range(min int, max int) int
	i64_in_range(min i64, max i64) i64
	f32() f32
	f64() f64
	f32n(max f32) f32
	f64n(max f64) f64
	f32_in_range(min f32, max f32) f32
	f64_in_range(min f64, max f64) f64
}

__global ( default_rng &PRNG )

// init initializes the default RNG.
fn init() {
	default_rng = new_default({})
}

// new_default returns a new instance of the default RNG. If the seed is not provided, the current time will be used to seed the instance.
pub fn new_default(config PRNGConfigStruct) &PRNG {
	mut rng := &wyrand.WyRandRNG{}
	rng.seed(config.seed)
	return rng
}

// get_current_rng returns the PRNG instance currently in use. If it is not changed, it will be an instance of wyrand.WyRandRNG.
pub fn get_current_rng() &PRNG {
	return default_rng
}

// set_rng changes the default RNG from wyrand.WyRandRNG (or whatever the last RNG was) to the one
// provided by the user. Note that this new RNG must be seeded manually with a constant seed or the 
// `seed.time_seed_array()` method. Also, it is recommended to store the old RNG in a variable and
// should be restored if work with the custom RNG is complete. It is not necessary to restore if the
// program terminates soon afterwards.
pub fn set_rng(rng &PRNG) {
	default_rng = rng
}

// seed sets the given array of `u32` values as the seed for the `default_rng`. It is recommended to use
pub fn seed(seed []u32) {
	default_rng.seed(seed)
}

// u32 returns a uniformly distributed `u32` in range `[0, 2³²)`.
pub fn u32() u32 {
	return default_rng.u32()
}

// u64 returns a uniformly distributed `u64` in range `[0, 2⁶⁴)`.
pub fn u64() u64 {
	return default_rng.u64()
}

// u32n returns a uniformly distributed pseudorandom 32-bit signed positive `u32` in range `[0, max)`.
pub fn u32n(max u32) u32 {
	return default_rng.u32n(max)
}

// u64n returns a uniformly distributed pseudorandom 64-bit signed positive `u64` in range `[0, max)`.
pub fn u64n(max u64) u64 {
	return default_rng.u64n(max)
}

// u32_in_range returns a uniformly distributed pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
pub fn u32_in_range(min u32, max u32) u32 {
	return default_rng.u32_in_range(min, max)
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
pub fn u64_in_range(min u64, max u64) u64 {
	return default_rng.u64_in_range(min, max)
}

// int returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) `int`.
pub fn int() int {
	return default_rng.int()
}

// intn returns a uniformly distributed pseudorandom 32-bit signed positive `int` in range `[0, max)`.
pub fn intn(max int) int {
	return default_rng.intn(max)
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
pub fn byte() byte {
	return byte(default_rng.u32() & 0xff)
}

// int_in_range returns a uniformly distributed pseudorandom  32-bit signed int in range `[min, max)`.
// Both `min` and `max` can be negative, but we must have `min < max`.
pub fn int_in_range(min int, max int) int {
	return default_rng.int_in_range(min, max)
}

// int31 returns a uniformly distributed pseudorandom 31-bit signed positive `int`.
pub fn int31() int {
	return default_rng.int31()
}

// i64 returns a uniformly distributed pseudorandom 64-bit signed (possibly negative) `i64`.
pub fn i64() i64 {
	return default_rng.i64()
}

// i64n returns a uniformly distributed pseudorandom 64-bit signed positive `i64` in range `[0, max)`.
pub fn i64n(max i64) i64 {
	return default_rng.i64n(max)
}

// i64_in_range returns a uniformly distributed pseudorandom 64-bit signed `i64` in range `[min, max)`.
pub fn i64_in_range(min i64, max i64) i64 {
	return default_rng.i64_in_range(min, max)
}

// int63 returns a uniformly distributed pseudorandom 63-bit signed positive `i64`.
pub fn int63() i64 {
	return default_rng.int63()
}

// f32 returns a uniformly distributed 32-bit floating point in range `[0, 1)`.
pub fn f32() f32 {
	return default_rng.f32()
}

// f64 returns a uniformly distributed 64-bit floating point in range `[0, 1)`.
pub fn f64() f64 {
	return default_rng.f64()
}

// f32n returns a uniformly distributed 32-bit floating point in range `[0, max)`.
pub fn f32n(max f32) f32 {
	return default_rng.f32n(max)
}

// f64n returns a uniformly distributed 64-bit floating point in range `[0, max)`.
pub fn f64n(max f64) f64 {
	return default_rng.f64n(max)
}

// f32_in_range returns a uniformly distributed 32-bit floating point in range `[min, max)`.
pub fn f32_in_range(min f32, max f32) f32 {
	return default_rng.f32_in_range(min, max)
}

// f64_in_range returns a uniformly distributed 64-bit floating point in range `[min, max)`.
pub fn f64_in_range(min f64, max f64) f64 {
	return default_rng.f64_in_range(min, max)
}

const (
	english_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
	hex_chars       = 'abcdef0123456789'
	ascii_chars     = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~'
)

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`
pub fn string_from_set(charset string, len int) string {
	if len == 0 {
		return ''
	}
	mut buf := unsafe { malloc(len) }
	for i in 0 .. len {
		unsafe {
			buf[i] = charset[intn(charset.len)]
		}
	}
	return unsafe { buf.vstring_with_len(len) }
}

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn string(len int) string {
	return string_from_set(rand.english_letters, len)
}

// hex returns a hexadecimal number of length `len` containing random characters in range `[a-f0-9]`.
pub fn hex(len int) string {
	return string_from_set(rand.hex_chars, len)
}

// ascii returns a random string of the printable ASCII characters with length `len`.
pub fn ascii(len int) string {
	return string_from_set(rand.ascii_chars, len)
}

// uuid_v4 generates a random (v4) UUID
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
pub fn uuid_v4() string {
	buflen := 36
	mut buf := unsafe { malloc(37) }
	mut i_buf := 0
	mut x := u64(0)
	mut d := byte(0)
	for i_buf < buflen {
		mut c := 0
		x = default_rng.u64()
		// do most of the bit manipulation at once:
		x &= 0x0F0F0F0F0F0F0F0F
		x += 0x3030303030303030
		// write the ASCII codes to the buffer:
		for c < 8 && i_buf < buflen {
			d = byte(x)
			unsafe {
				buf[i_buf] = if d > 0x39 { d + 0x27 } else { d }
			}
			i_buf++
			c++
			x = x >> 8
		}
	}
	// there are still some random bits in x:
	x = x >> 8
	d = byte(x)
	unsafe {
		buf[19] = if d > 0x39 { d + 0x27 } else { d }
		buf[8] = `-`
		buf[13] = `-`
		buf[18] = `-`
		buf[23] = `-`
		buf[14] = `4`
		buf[buflen] = 0
		return buf.vstring_with_len(buflen)
	}
}

const (
	ulid_encoding = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'
)

// ulid generates an Unique Lexicographically sortable IDentifier.
// See https://github.com/ulid/spec .
// NB: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return ulid_at_millisecond(time.utc().unix_time_milli())
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_time_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	buflen := 26
	mut buf := unsafe { malloc(27) }
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = rand.ulid_encoding[t & 0x1F]
		}
		t = t >> 5
		i--
	}
	// first rand set
	mut x := default_rng.u64()
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	// second rand set
	x = default_rng.u64()
	for i < 26 {
		unsafe {
			buf[i] = rand.ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	unsafe {
		buf[26] = 0
		return buf.vstring_with_len(buflen)
	}
}
