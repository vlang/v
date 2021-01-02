// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module rand

import rand.util
import rand.wyrand
import time

// PRNGConfigStruct is a configuration struct for creating a new instance of the default RNG.
pub struct PRNGConfigStruct {
	seed []u32 = util.time_seed_array(2)
}

__global ( default_rng &wyrand.WyRandRNG )

// init initializes the default RNG.
fn init() {
	default_rng = new_default({})
}

// new_default returns a new instance of the default RNG. If the seed is not provided, the current time will be used to seed the instance.
pub fn new_default(config PRNGConfigStruct) &wyrand.WyRandRNG {
	mut rng := &wyrand.WyRandRNG{}
	rng.seed(config.seed)
	return rng
}

// seed sets the given array of `u32` values as the seed for the `default_rng`.
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
	chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
)

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn string(len int) string {
	mut buf := malloc(len)
	for i in 0 .. len {
		unsafe {
			buf[i] = chars[intn(chars.len)]
		}
	}
	return unsafe {buf.vstring_with_len(len)}
}

// uuid_v4 generates a random (v4) UUID
// See https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_(random)
pub fn uuid_v4() string {
	buflen := 36
	mut buf := malloc(37)
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
	}
	return unsafe {buf.vstring_with_len(buflen)}
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
	mut buf := malloc(27)
	mut t := unix_time_milli
	mut i := 9
	for i >= 0 {
		unsafe {
			buf[i] = ulid_encoding[t & 0x1F]
		}
		t = t >> 5
		i--
	}
	// first rand set
	mut x := default_rng.u64()
	i = 10
	for i < 19 {
		unsafe {
			buf[i] = ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	// second rand set
	x = default_rng.u64()
	for i < 26 {
		unsafe {
			buf[i] = ulid_encoding[x & 0x1F]
		}
		x = x >> 5
		i++
	}
	unsafe {
		buf[26] = 0
	}
	return unsafe {buf.vstring_with_len(buflen)}
}
