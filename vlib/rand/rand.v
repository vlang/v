// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module rand

import math.bits
import rand.config
import rand.wyrand
import time

// PRNG is a common interface for all PRNGs that can be used seamlessly with the rand modules's API.
// It defines all the methods that a PRNG (in the vlib or custom made) must
// implement in order to ensure that _all_ functions can be used with the generator.
pub interface PRNG {
mut:
	seed(seed_data []u32)
	u8() u8
	u16() u16
	u32() u32
	u64() u64
	block_size() int
	free()
}

// bytes returns a buffer of `bytes_needed` random bytes
@[inline]
pub fn (mut rng PRNG) bytes(bytes_needed int) ![]u8 {
	if bytes_needed < 0 {
		return error('can not read < 0 random bytes')
	}

	mut buffer := []u8{len: bytes_needed}
	read_internal(mut rng, mut buffer)

	return buffer
}

// read fills in `buf` with a maximum of `buf.len` random bytes.
pub fn (mut rng PRNG) read(mut buf []u8) {
	read_internal(mut rng, mut buf)
}

// i32n returns a uniformly distributed pseudorandom 32-bit signed positive `i32` in range `[0, max)`.
@[inline]
pub fn (mut rng PRNG) i32n(max i32) !i32 {
	return i32(rng.intn(max)!)
}

// u32n returns a uniformly distributed pseudorandom 32-bit unsigned positive `u32` in range `[0, max)`.
@[inline]
pub fn (mut rng PRNG) u32n(max u32) !u32 {
	if max == 0 {
		return error('max must be positive integer')
	}
	// Owing to the pigeon-hole principle, we can't simply do
	// val := rng.u32() % max.
	// It'll wreck the properties of the distribution unless
	// max evenly divides 2^32. So we divide evenly to
	// the closest power of two. Then we loop until we find
	// an int in the required range
	bit_len := bits.len_32(max)
	if _unlikely_(bit_len == 32) {
		for {
			value := rng.u32()
			if value < max {
				return value
			}
		}
	} else {
		mask := if _unlikely_(bit_len == 31) {
			u32(0x7FFFFFFF)
		} else {
			(u32(1) << (bit_len + 1)) - 1
		}
		for {
			value := rng.u32() & mask
			if value < max {
				return value
			}
		}
	}
	return u32(0)
}

// u64n returns a uniformly distributed pseudorandom 64-bit signed positive `u64` in range `[0, max)`.
@[inline]
pub fn (mut rng PRNG) u64n(max u64) !u64 {
	if max == 0 {
		return error('max must be positive integer')
	}
	bit_len := bits.len_64(max)
	if _unlikely_(bit_len == 64) {
		for {
			value := rng.u64()
			if value < max {
				return value
			}
		}
	} else {
		mask := if _unlikely_(bit_len == 63) {
			u64(0x7FFFFFFFFFFFFFFF)
		} else {
			(u64(1) << (bit_len + 1)) - 1
		}
		for {
			value := rng.u64() & mask
			if value < max {
				return value
			}
		}
	}
	return u64(0)
}

// u32_in_range returns a uniformly distributed pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
@[inline]
pub fn (mut rng PRNG) u32_in_range(min u32, max u32) !u32 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.u32n(max - min)!
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
@[inline]
pub fn (mut rng PRNG) u64_in_range(min u64, max u64) !u64 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.u64n(max - min)!
}

// i8 returns a (possibly negative) pseudorandom 8-bit `i8`.
@[inline]
pub fn (mut rng PRNG) i8() i8 {
	return i8(rng.u8())
}

// i16 returns a (possibly negative) pseudorandom 16-bit `i16`.
@[inline]
pub fn (mut rng PRNG) i16() i16 {
	return i16(rng.u16())
}

// i32 returns a (possibly negative) pseudorandom 32-bit `i32`.
@[inline]
pub fn (mut rng PRNG) i32() i32 {
	return i32(rng.u32())
}

// int returns a (possibly negative) pseudorandom 32-bit `int`.
@[inline]
pub fn (mut rng PRNG) int() int {
	return int(rng.u32())
}

// i64 returns a (possibly negative) pseudorandom 64-bit `i64`.
@[inline]
pub fn (mut rng PRNG) i64() i64 {
	return i64(rng.u64())
}

// int31 returns a positive pseudorandom 31-bit `int`.
@[inline]
pub fn (mut rng PRNG) int31() int {
	return int(rng.u32() & u32(0x7FFFFFFF)) // Set the 32nd bit to 0.
}

// int63 returns a positive pseudorandom 63-bit `i64`.
@[inline]
pub fn (mut rng PRNG) int63() i64 {
	return i64(rng.u64() & u64(0x7FFFFFFFFFFFFFFF)) // Set the 64th bit to 0.
}

// intn returns a pseudorandom `int` in range `[0, max)`.
@[inline]
pub fn (mut rng PRNG) intn(max int) !int {
	if max <= 0 {
		return error('max has to be positive.')
	}
	return int(rng.u32n(u32(max))!)
}

// i64n returns a pseudorandom int that lies in `[0, max)`.
@[inline]
pub fn (mut rng PRNG) i64n(max i64) !i64 {
	if max <= 0 {
		return error('max has to be positive.')
	}
	return i64(rng.u64n(u64(max))!)
}

// int_in_range returns a pseudorandom `int` in range `[min, max)`.
@[inline]
pub fn (mut rng PRNG) int_in_range(min int, max int) !int {
	if max <= min {
		return error('max must be greater than min')
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + rng.intn(max - min)!
}

// int_in_range returns a pseudorandom `int` in range `[min, max)`.
@[inline]
pub fn (mut rng PRNG) i32_in_range(min i32, max i32) !i32 {
	if max <= min {
		return error('max must be greater than min')
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + i32(rng.intn(max - min)!)
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max)`.
@[inline]
pub fn (mut rng PRNG) i64_in_range(min i64, max i64) !i64 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.i64n(max - min)!
}

// smallest mantissa with exponent 0 (un normalized)
const reciprocal_2_23rd = 1.0 / f64(u32(1) << 23)
const reciprocal_2_52nd = 1.0 / f64(u64(1) << 52)
const ieee754_mantissa_f32_mask = (u32(1) << 23) - 1 // 23 bits for f32
const ieee754_mantissa_f64_mask = (u64(1) << 52) - 1 // 52 bits for f64

// f32 returns a pseudorandom `f32` value in range `[0, 1)`
// using rng.u32() multiplied by an f64 constant.
@[inline]
pub fn (mut rng PRNG) f32() f32 {
	return f32((rng.u32() >> 9) * reciprocal_2_23rd)
}

// f32cp returns a pseudorandom `f32` value in range `[0, 1)`
// with full precision (mantissa random between 0 and 1
// and the exponent varies as well.)
// See https://allendowney.com/research/rand/ for background on the method.
@[inline]
pub fn (mut rng PRNG) f32cp() f32 {
	mut x := rng.u32()
	mut exp := u32(126)
	mut mask := u32(1) << 31

	// check if prng returns 0; rare but keep looking for precision
	if _unlikely_(x == 0) {
		x = rng.u32()
		exp -= 31
	}
	// count leading one bits and scale exponent accordingly
	for {
		if x & mask != 0 {
			mask >>= 1
			exp -= 1
		} else {
			break
		}
	}
	// if we used any high-order mantissa bits; replace x
	if exp < (126 - 8) {
		x = rng.u32()
	}

	// Assumes little-endian IEEE floating point.
	x = (exp << 23) | (x >> 8) & ieee754_mantissa_f32_mask
	return bits.f32_from_bits(x)
}

// f64 returns a pseudorandom `f64` value in range `[0, 1)`
// using rng.u64() multiplied by a constant.
@[inline]
pub fn (mut rng PRNG) f64() f64 {
	return f64((rng.u64() >> 12) * reciprocal_2_52nd)
}

// f64cp returns a pseudorandom `f64` value in range `[0, 1)`
// with full precision (mantissa random between 0 and 1
// and the exponent varies as well.)
// See https://allendowney.com/research/rand/ for background on the method.
@[inline]
pub fn (mut rng PRNG) f64cp() f64 {
	mut x := rng.u64()
	mut exp := u64(1022)
	mut mask := u64(1) << 63
	mut bitcount := u32(0)

	// check if prng returns 0; unlikely.
	if _unlikely_(x == 0) {
		x = rng.u64()
		exp -= 31
	}
	// count leading one bits and scale exponent accordingly
	for {
		if x & mask != 0 {
			mask >>= 1
			bitcount += 1
		} else {
			break
		}
	}
	exp -= bitcount
	if bitcount > 11 {
		x = rng.u64()
	}
	x = (exp << 52) | (x & ieee754_mantissa_f64_mask)
	return bits.f64_from_bits(x)
}

// f32n returns a pseudorandom `f32` value in range `[0, max]`.
@[inline]
pub fn (mut rng PRNG) f32n(max f32) !f32 {
	if max < 0 {
		return error('max has to be non-negative.')
	}
	return rng.f32() * max
}

// f64n returns a pseudorandom `f64` value in range `[0, max]`.
@[inline]
pub fn (mut rng PRNG) f64n(max f64) !f64 {
	if max < 0 {
		return error('max has to be non-negative.')
	}
	return rng.f64() * max
}

// f32_in_range returns a pseudorandom `f32` in range `[min, max]`.
@[inline]
pub fn (mut rng PRNG) f32_in_range(min f32, max f32) !f32 {
	if max < min {
		return error('max must be greater than or equal to min')
	}
	return min + rng.f32n(max - min)!
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max]`.
@[inline]
pub fn (mut rng PRNG) f64_in_range(min f64, max f64) !f64 {
	if max < min {
		return error('max must be greater than or equal to min')
	}
	return min + rng.f64n(max - min)!
}

// ulid generates an unique lexicographically sortable identifier.
// See https://github.com/ulid/spec .
// Note: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn (mut rng PRNG) ulid() string {
	return internal_ulid_at_millisecond(mut rng, u64(time.utc().unix_milli()))
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_milli`.
pub fn (mut rng PRNG) ulid_at_millisecond(unix_time_milli u64) string {
	return internal_ulid_at_millisecond(mut rng, unix_time_milli)
}

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`.
pub fn (mut rng PRNG) string_from_set(charset string, len int) string {
	return internal_string_from_set(mut rng, charset, len)
}

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn (mut rng PRNG) string(len int) string {
	return internal_string_from_set(mut rng, english_letters, len)
}

// hex returns a hexadecimal number of length `len` containing random characters in range `[a-f0-9]`.
pub fn (mut rng PRNG) hex(len int) string {
	return internal_string_from_set(mut rng, hex_chars, len)
}

// ascii returns a random string of the printable ASCII characters with length `len`.
pub fn (mut rng PRNG) ascii(len int) string {
	return internal_string_from_set(mut rng, ascii_chars, len)
}

// fill_buffer_from_set fills the mutable `buf` with random characters from the given `charset`
@[inline]
pub fn (mut rng PRNG) fill_buffer_from_set(charset string, mut buf []u8) {
	internal_fill_buffer_from_set(mut rng, charset, mut buf)
}

// bernoulli returns true with a probability p. Note that 0 <= p <= 1.
pub fn (mut rng PRNG) bernoulli(p f64) !bool {
	if p < 0 || p > 1 {
		return error('${p} is not a valid probability value.')
	}
	return rng.f64() <= p
}

// normal returns a normally distributed pseudorandom f64 with mean `mu` and standard deviation `sigma`.
// By default, `mu` is 0.0 and `sigma` is 1.0.
// NOTE: Use normal_pair() instead if you're generating a lot of normal variates.
pub fn (mut rng PRNG) normal(conf config.NormalConfigStruct) !f64 {
	x, _ := rng.normal_pair(conf)!
	return x
}

// normal_pair returns a pair of normally distributed pseudorandom f64 with mean `mu` and standard deviation `sigma`.
// By default, `mu` is 0.0 and `sigma` is 1.0.
pub fn (mut rng PRNG) normal_pair(conf config.NormalConfigStruct) !(f64, f64) {
	if conf.sigma <= 0 {
		return error('Standard deviation must be positive')
	}
	// This is an implementation of the Marsaglia polar method
	// See: https://doi.org/10.1137%2F1006063
	// Also: https://en.wikipedia.org/wiki/Marsaglia_polar_method
	for {
		u := rng.f64_in_range(-1, 1) or { 0.0 }
		v := rng.f64_in_range(-1, 1) or { 0.0 }

		s := u * u + v * v
		if s >= 1 || s == 0 {
			continue
		}
		t := msqrt(-2 * mlog(s) / s)
		x := conf.mu + conf.sigma * t * u
		y := conf.mu + conf.sigma * t * v
		return x, y
	}
	return error('Implementation error. Please file an issue.')
}

// binomial returns the number of successful trials out of n when the probability of success for each trial is p.
pub fn (mut rng PRNG) binomial(n int, p f64) !int {
	if p < 0 || p > 1 {
		return error('${p} is not a valid probability value.')
	}
	mut count := 0
	for _ in 0 .. n {
		if rng.bernoulli(p)! {
			count++
		}
	}
	return count
}

// exponential returns an exponentially distributed random number with the rate parameter lambda.
// It is expected that lambda is positive.
pub fn (mut rng PRNG) exponential(lambda f64) f64 {
	if lambda <= 0 {
		panic('The rate (lambda) must be positive.')
	}
	// Use the inverse transform sampling method
	return -mlog(rng.f64()) / lambda
}

// shuffle randomly permutates the elements in `a`. The range for shuffling is
// optional and the entire array is shuffled by default. Leave the end as 0 to
// shuffle all elements until the end.
@[direct_array_access]
pub fn (mut rng PRNG) shuffle[T](mut a []T, config_ config.ShuffleConfigStruct) ! {
	config_.validate_for(a)!
	new_end := if config_.end == 0 { a.len } else { config_.end }

	// We implement the Fisher-Yates shuffle:
	// https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm

	for i in config_.start .. new_end - 2 {
		x := rng.int_in_range(i, new_end) or { i }
		// swap
		a_i := a[i]
		a[i] = a[x]
		a[x] = a_i
	}
}

// shuffle_clone returns a random permutation of the elements in `a`.
// The permutation is done on a fresh clone of `a`, so `a` remains unchanged.
pub fn (mut rng PRNG) shuffle_clone[T](a []T, config_ config.ShuffleConfigStruct) ![]T {
	mut res := a.clone()
	rng.shuffle[T](mut res, config_)!
	return res
}

// choose samples k elements from the array without replacement.
// This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array.
// Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn (mut rng PRNG) choose[T](array []T, k int) ![]T {
	n := array.len
	if k > n {
		return error('Cannot choose ${k} elements without replacement from a ${n}-element array.')
	}
	mut results := []T{len: k}
	mut indices := []int{len: n, init: index}
	rng.shuffle[int](mut indices)!
	for i in 0 .. k {
		results[i] = array[indices[i]]
	}
	return results
}

// element returns a random element from the given array.
// Note that all the positions in the array have an equal chance of being selected. This means that if the array has repeating elements, then the probability of selecting a particular element is not uniform.
pub fn (mut rng PRNG) element[T](array []T) !T {
	if array.len == 0 {
		return error('Cannot choose an element from an empty array.')
	}
	return array[rng.intn(array.len)!]
}

// sample samples k elements from the array with replacement.
// This means the elements can repeat and the size of the sample may exceed the size of the array.
pub fn (mut rng PRNG) sample[T](array []T, k int) []T {
	mut results := []T{len: k}
	for i in 0 .. k {
		results[i] = array[rng.intn(array.len) or { 0 }]
	}
	return results
}

__global default_rng &PRNG

// new_default returns a new instance of the default RNG. If the seed is not provided, the current time will be used to seed the instance.
@[manualfree]
pub fn new_default(config_ config.PRNGConfigStruct) &PRNG {
	mut rng := &wyrand.WyRandRNG{}
	rng.seed(config_.seed_)
	unsafe { config_.seed_.free() }
	return &PRNG(rng)
}

// get_current_rng returns the PRNG instance currently in use. If it is not changed, it will be an instance of wyrand.WyRandRNG.
pub fn get_current_rng() &PRNG {
	return default_rng
}

// set_rng changes the default RNG from wyrand.WyRandRNG (or whatever the last RNG was).
// Note that this new RNG must be seeded manually with a constant seed or the
// `seed.time_seed_array()` method. Also, it is recommended to store the old RNG in a variable and
// should be restored if work with the custom RNG is complete. It is not necessary to restore if the
// program terminates soon afterwards.
pub fn set_rng(rng &PRNG) {
	default_rng = unsafe { rng }
}

// seed sets the given array of `u32` values as the seed for the `default_rng`.
// The default_rng is an instance of WyRandRNG which takes 2 u32 values. When using a custom RNG, make sure to use
// the correct number of u32s.
pub fn seed(seed []u32) {
	default_rng.seed(seed)
}

// u8 returns a uniformly distributed pseudorandom 8-bit unsigned positive `u8`.
pub fn u8() u8 {
	return default_rng.u8()
}

// u16 returns a uniformly distributed pseudorandom 16-bit unsigned positive `u16`.
pub fn u16() u16 {
	return default_rng.u16()
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
pub fn u32n(max u32) !u32 {
	return default_rng.u32n(max)
}

// u64n returns a uniformly distributed pseudorandom 64-bit signed positive `u64` in range `[0, max)`.
pub fn u64n(max u64) !u64 {
	return default_rng.u64n(max)
}

// u32_in_range returns a uniformly distributed pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
pub fn u32_in_range(min u32, max u32) !u32 {
	return default_rng.u32_in_range(min, max)
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
pub fn u64_in_range(min u64, max u64) !u64 {
	return default_rng.u64_in_range(min, max)
}

// i16 returns a uniformly distributed pseudorandom 16-bit signed (possibly negative) `i16`.
pub fn i16() i16 {
	return default_rng.i16()
}

// i32 returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) `i32`.
pub fn i32() i32 {
	return default_rng.i32()
}

// int returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) `int`.
pub fn int() int {
	return default_rng.int()
}

// i32n returns a uniformly distributed pseudorandom 32-bit signed positive `i32` in range `[0, max)`.
pub fn i32n(max i32) !i32 {
	return default_rng.i32n(max)
}

// intn returns a uniformly distributed pseudorandom 32-bit signed positive `int` in range `[0, max)`.
pub fn intn(max int) !int {
	return default_rng.intn(max)
}

// int_in_range returns a uniformly distributed pseudorandom  32-bit signed int in range `[min, max)`.
// Both `min` and `max` can be negative, but we must have `min < max`.
pub fn int_in_range(min int, max int) !int {
	return default_rng.int_in_range(min, max)
}

// int_in_range returns a uniformly distributed pseudorandom  32-bit signed int in range `[min, max)`.
// Both `min` and `max` can be negative, but we must have `min < max`.
pub fn i32_in_range(min i32, max i32) !i32 {
	return default_rng.i32_in_range(min, max)
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
pub fn i64n(max i64) !i64 {
	return default_rng.i64n(max)
}

// i64_in_range returns a uniformly distributed pseudorandom 64-bit signed `i64` in range `[min, max)`.
pub fn i64_in_range(min i64, max i64) !i64 {
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

// f32cp returns a uniformly distributed 32-bit floating point in range `[0, 1)` with full precision mantissa.
pub fn f32cp() f32 {
	return default_rng.f32cp()
}

// f64 returns a uniformly distributed 64-bit floating point in range `[0, 1)`.
pub fn f64() f64 {
	return default_rng.f64()
}

// f64 returns a uniformly distributed 64-bit floating point in range `[0, 1)` with full precision mantissa.
pub fn f64cp() f64 {
	return default_rng.f64cp()
}

// f32n returns a uniformly distributed 32-bit floating point in range `[0, max)`.
pub fn f32n(max f32) !f32 {
	return default_rng.f32n(max)
}

// f64n returns a uniformly distributed 64-bit floating point in range `[0, max)`.
pub fn f64n(max f64) !f64 {
	return default_rng.f64n(max)
}

// f32_in_range returns a uniformly distributed 32-bit floating point in range `[min, max)`.
pub fn f32_in_range(min f32, max f32) !f32 {
	return default_rng.f32_in_range(min, max)
}

// f64_in_range returns a uniformly distributed 64-bit floating point in range `[min, max)`.
pub fn f64_in_range(min f64, max f64) !f64 {
	return default_rng.f64_in_range(min, max)
}

// bytes returns a buffer of `bytes_needed` random bytes.
pub fn bytes(bytes_needed int) ![]u8 {
	return default_rng.bytes(bytes_needed)
}

// read fills in `buf` a maximum of `buf.len` random bytes.
pub fn read(mut buf []u8) {
	read_internal(mut default_rng, mut buf)
}

const english_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
const hex_chars = '0123456789abcdef'
const ascii_chars = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~'

// ulid generates an unique lexicographically sortable identifier.
// See https://github.com/ulid/spec .
// Note: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return default_rng.ulid()
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	return default_rng.ulid_at_millisecond(unix_time_milli)
}

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`.
pub fn string_from_set(charset string, len int) string {
	return default_rng.string_from_set(charset, len)
}

// fill_buffer_from_set fills the array `buf` with random characters sampled from the given `charset`
@[inline]
pub fn fill_buffer_from_set(charset string, mut buf []u8) {
	default_rng.fill_buffer_from_set(charset, mut buf)
}

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn string(len int) string {
	return string_from_set(english_letters, len)
}

// hex returns a hexadecimal number of length `len` containing random characters in range `[a-f0-9]`.
pub fn hex(len int) string {
	return string_from_set(hex_chars, len)
}

// ascii returns a random string of the printable ASCII characters with length `len`.
pub fn ascii(len int) string {
	return string_from_set(ascii_chars, len)
}

// shuffle randomly permutates the elements in `a`.
// The range for shuffling is optional and the entire array is shuffled by default.
// Leave the end as 0 to shuffle all elements until the end.
pub fn shuffle[T](mut a []T, config_ config.ShuffleConfigStruct) ! {
	default_rng.shuffle[T](mut a, config_)!
}

// shuffle_clone returns a random permutation of the elements in `a`.
// The permutation is done on a fresh clone of `a`, so `a` remains unchanged.
pub fn shuffle_clone[T](a []T, config_ config.ShuffleConfigStruct) ![]T {
	return default_rng.shuffle_clone[T](a, config_)
}

// choose samples k elements from the array without replacement.
// This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array.
// Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn choose[T](array []T, k int) ![]T {
	return default_rng.choose[T](array, k)
}

// element returns a random element from the given array.
// Note that all the positions in the array have an equal chance of being selected. This means that if the array has repeating elements, then the probability of selecting a particular element is not uniform.
pub fn element[T](array []T) !T {
	return default_rng.element[T](array)
}

// sample samples k elements from the array with replacement.
// This means the elements can repeat and the size of the sample may exceed the size of the array.
pub fn sample[T](array []T, k int) []T {
	return default_rng.sample[T](array, k)
}

// bernoulli returns true with a probability p. Note that 0 <= p <= 1.
pub fn bernoulli(p f64) !bool {
	return default_rng.bernoulli(p)
}

// normal returns a normally distributed pseudorandom f64 with mean `mu` and standard deviation `sigma`.
// By default, `mu` is 0.0 and `sigma` is 1.0.
// NOTE: Use normal_pair() instead if you're generating a lot of normal variates.
pub fn normal(config_ config.NormalConfigStruct) !f64 {
	return default_rng.normal(config_)
}

// normal_pair returns a pair of normally distributed pseudorandom f64 with mean `mu` and standard deviation `sigma`.
// By default, `mu` is 0.0 and `sigma` is 1.0.
pub fn normal_pair(config_ config.NormalConfigStruct) !(f64, f64) {
	return default_rng.normal_pair(config_)
}

// binomial returns the number of successful trials out of n when the probability of success for each trial is p.
pub fn binomial(n int, p f64) !int {
	return default_rng.binomial(n, p)
}

// exponential returns an exponentially distributed random number with the rate parameter `lambda`.
// It is expected that lambda is positive.
pub fn exponential(lambda f64) f64 {
	return default_rng.exponential(lambda)
}
