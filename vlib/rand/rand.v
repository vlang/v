// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
[has_globals]
module rand

import math.bits
import rand.config
import rand.constants
import rand.wyrand
import time

// PRNG is a common interface for all PRNGs that can be used seamlessly with the rand
// modules's API. It defines all the methods that a PRNG (in the vlib or custom made) must
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
[inline]
pub fn (mut rng PRNG) bytes(bytes_needed int) ?[]u8 {
	if bytes_needed < 0 {
		return error('can not read < 0 random bytes')
	}

	mut buffer := []u8{len: bytes_needed}
	read_internal(mut rng, mut buffer)

	return buffer
}

// read fills in `buf` with a maximum of `buf.len` random bytes
pub fn (mut rng PRNG) read(mut buf []u8) {
	read_internal(mut rng, mut buf)
}

// u32n returns a uniformly distributed pseudorandom 32-bit signed positive `u32` in range `[0, max)`.
[inline]
pub fn (mut rng PRNG) u32n(max u32) ?u32 {
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
	if bit_len == 32 {
		for {
			value := rng.u32()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u32(1) << (bit_len + 1)) - 1
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
[inline]
pub fn (mut rng PRNG) u64n(max u64) ?u64 {
	if max == 0 {
		return error('max must be positive integer')
	}
	bit_len := bits.len_64(max)
	if bit_len == 64 {
		for {
			value := rng.u64()
			if value < max {
				return value
			}
		}
	} else {
		mask := (u64(1) << (bit_len + 1)) - 1
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
[inline]
pub fn (mut rng PRNG) u32_in_range(min u32, max u32) ?u32 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.u32n(max - min)?
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
[inline]
pub fn (mut rng PRNG) u64_in_range(min u64, max u64) ?u64 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.u64n(max - min)?
}

// i8 returns a (possibly negative) pseudorandom 8-bit `i8`.
[inline]
pub fn (mut rng PRNG) i8() i8 {
	return i8(rng.u8())
}

// i16 returns a (possibly negative) pseudorandom 16-bit `i16`.
[inline]
pub fn (mut rng PRNG) i16() i16 {
	return i16(rng.u16())
}

// int returns a (possibly negative) pseudorandom 32-bit `int`.
[inline]
pub fn (mut rng PRNG) int() int {
	return int(rng.u32())
}

// i64 returns a (possibly negative) pseudorandom 64-bit `i64`.
[inline]
pub fn (mut rng PRNG) i64() i64 {
	return i64(rng.u64())
}

// int31 returns a positive pseudorandom 31-bit `int`.
[inline]
pub fn (mut rng PRNG) int31() int {
	return int(rng.u32() & constants.u31_mask) // Set the 32nd bit to 0.
}

// int63 returns a positive pseudorandom 63-bit `i64`.
[inline]
pub fn (mut rng PRNG) int63() i64 {
	return i64(rng.u64() & constants.u63_mask) // Set the 64th bit to 0.
}

// intn returns a pseudorandom `int` in range `[0, max)`.
[inline]
pub fn (mut rng PRNG) intn(max int) ?int {
	if max <= 0 {
		return error('max has to be positive.')
	}
	return int(rng.u32n(u32(max))?)
}

// i64n returns a pseudorandom int that lies in `[0, max)`.
[inline]
pub fn (mut rng PRNG) i64n(max i64) ?i64 {
	if max <= 0 {
		return error('max has to be positive.')
	}
	return i64(rng.u64n(u64(max))?)
}

// int_in_range returns a pseudorandom `int` in range `[min, max)`.
[inline]
pub fn (mut rng PRNG) int_in_range(min int, max int) ?int {
	if max <= min {
		return error('max must be greater than min')
	}
	// This supports negative ranges like [-10, -5) because the difference is positive
	return min + rng.intn(max - min)?
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max)`.
[inline]
pub fn (mut rng PRNG) i64_in_range(min i64, max i64) ?i64 {
	if max <= min {
		return error('max must be greater than min')
	}
	return min + rng.i64n(max - min)?
}

// f32 returns a pseudorandom `f32` value in range `[0, 1)`.
[inline]
pub fn (mut rng PRNG) f32() f32 {
	return f32(rng.u32()) / constants.max_u32_as_f32
}

// f64 returns a pseudorandom `f64` value in range `[0, 1)`.
[inline]
pub fn (mut rng PRNG) f64() f64 {
	return f64(rng.u64()) / constants.max_u64_as_f64
}

// f32n returns a pseudorandom `f32` value in range `[0, max]`.
[inline]
pub fn (mut rng PRNG) f32n(max f32) ?f32 {
	if max < 0 {
		return error('max has to be non-negative.')
	}
	return rng.f32() * max
}

// f64n returns a pseudorandom `f64` value in range `[0, max]`.
[inline]
pub fn (mut rng PRNG) f64n(max f64) ?f64 {
	if max < 0 {
		return error('max has to be non-negative.')
	}
	return rng.f64() * max
}

// f32_in_range returns a pseudorandom `f32` in range `[min, max]`.
[inline]
pub fn (mut rng PRNG) f32_in_range(min f32, max f32) ?f32 {
	if max < min {
		return error('max must be greater than or equal to min')
	}
	return min + rng.f32n(max - min)?
}

// i64_in_range returns a pseudorandom `i64` in range `[min, max]`.
[inline]
pub fn (mut rng PRNG) f64_in_range(min f64, max f64) ?f64 {
	if max < min {
		return error('max must be greater than or equal to min')
	}
	return min + rng.f64n(max - min)?
}

// ulid generates an Unique Lexicographically sortable IDentifier.
// See https://github.com/ulid/spec .
// Note: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn (mut rng PRNG) ulid() string {
	return internal_ulid_at_millisecond(mut rng, u64(time.utc().unix_time_milli()))
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_time_milli`.
pub fn (mut rng PRNG) ulid_at_millisecond(unix_time_milli u64) string {
	return internal_ulid_at_millisecond(mut rng, unix_time_milli)
}

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`
pub fn (mut rng PRNG) string_from_set(charset string, len int) string {
	return internal_string_from_set(mut rng, charset, len)
}

// string returns a string of length `len` containing random characters in range `[a-zA-Z]`.
pub fn (mut rng PRNG) string(len int) string {
	return internal_string_from_set(mut rng, rand.english_letters, len)
}

// hex returns a hexadecimal number of length `len` containing random characters in range `[a-f0-9]`.
pub fn (mut rng PRNG) hex(len int) string {
	return internal_string_from_set(mut rng, rand.hex_chars, len)
}

// ascii returns a random string of the printable ASCII characters with length `len`.
pub fn (mut rng PRNG) ascii(len int) string {
	return internal_string_from_set(mut rng, rand.ascii_chars, len)
}

// bernoulli returns true with a probability p. Note that 0 <= p <= 1.
pub fn (mut rng PRNG) bernoulli(p f64) ?bool {
	if p < 0 || p > 1 {
		return error('$p is not a valid probability value.')
	}
	return rng.f64() <= p
}

// normal returns a normally distributed pseudorandom f64 in range `[0, 1)`.
// NOTE: Use normal_pair() instead if you're generating a lot of normal variates.
pub fn (mut rng PRNG) normal(conf config.NormalConfigStruct) ?f64 {
	x, _ := rng.normal_pair(conf)?
	return x
}

// normal_pair returns a pair of normally distributed pseudorandom f64 in range `[0, 1)`.
pub fn (mut rng PRNG) normal_pair(conf config.NormalConfigStruct) ?(f64, f64) {
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

// binomial returns the number of successful trials out of n when the
// probability of success for each trial is p.
pub fn (mut rng PRNG) binomial(n int, p f64) ?int {
	if p < 0 || p > 1 {
		return error('$p is not a valid probability value.')
	}
	mut count := 0
	for _ in 0 .. n {
		if rng.bernoulli(p)! {
			count++
		}
	}
	return count
}

// exponential returns an exponentially distributed random number with the rate paremeter
// lambda. It is expected that lambda is positive.
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
[direct_array_access]
pub fn (mut rng PRNG) shuffle<T>(mut a []T, config config.ShuffleConfigStruct) ? {
	config.validate_for(a)?
	new_end := if config.end == 0 { a.len } else { config.end }

	// We implement the Fisher-Yates shuffle:
	// https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle#The_modern_algorithm

	for i in config.start .. new_end - 2 {
		x := rng.int_in_range(i, new_end) or { i }
		// swap
		a_i := a[i]
		a[i] = a[x]
		a[x] = a_i
	}
}

// shuffle_clone returns a random permutation of the elements in `a`.
// The permutation is done on a fresh clone of `a`, so `a` remains unchanged.
pub fn (mut rng PRNG) shuffle_clone<T>(a []T, config config.ShuffleConfigStruct) ?[]T {
	mut res := a.clone()
	rng.shuffle<T>(mut res, config)?
	return res
}

// choose samples k elements from the array without replacement.
// This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array.
// Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn (mut rng PRNG) choose<T>(array []T, k int) ?[]T {
	n := array.len
	if k > n {
		return error('Cannot choose $k elements without replacement from a $n-element array.')
	}
	mut results := []T{len: k}
	mut indices := []int{len: n, init: it}
	rng.shuffle<int>(mut indices)?
	for i in 0 .. k {
		results[i] = array[indices[i]]
	}
	return results
}

// sample samples k elements from the array with replacement.
// This means the elements can repeat and the size of the sample may exceed the size of the array.
pub fn (mut rng PRNG) sample<T>(array []T, k int) []T {
	mut results := []T{len: k}
	for i in 0 .. k {
		results[i] = array[rng.intn(array.len) or { 0 }]
	}
	return results
}

__global default_rng &PRNG

// new_default returns a new instance of the default RNG. If the seed is not provided, the current time will be used to seed the instance.
[manualfree]
pub fn new_default(config config.PRNGConfigStruct) &PRNG {
	mut rng := &wyrand.WyRandRNG{}
	rng.seed(config.seed_)
	unsafe { config.seed_.free() }
	return &PRNG(rng)
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
	default_rng = unsafe { rng }
}

// seed sets the given array of `u32` values as the seed for the `default_rng`. The default_rng is
// an instance of WyRandRNG which takes 2 u32 values. When using a custom RNG, make sure to use
// the correct number of u32s.
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
pub fn u32n(max u32) ?u32 {
	return default_rng.u32n(max)
}

// u64n returns a uniformly distributed pseudorandom 64-bit signed positive `u64` in range `[0, max)`.
pub fn u64n(max u64) ?u64 {
	return default_rng.u64n(max)
}

// u32_in_range returns a uniformly distributed pseudorandom 32-bit unsigned `u32` in range `[min, max)`.
pub fn u32_in_range(min u32, max u32) ?u32 {
	return default_rng.u32_in_range(min, max)
}

// u64_in_range returns a uniformly distributed pseudorandom 64-bit unsigned `u64` in range `[min, max)`.
pub fn u64_in_range(min u64, max u64) ?u64 {
	return default_rng.u64_in_range(min, max)
}

// i16 returns a uniformly distributed pseudorandom 16-bit signed (possibly negative) `i16`.
pub fn i16() i16 {
	return default_rng.i16()
}

// int returns a uniformly distributed pseudorandom 32-bit signed (possibly negative) `int`.
pub fn int() int {
	return default_rng.int()
}

// intn returns a uniformly distributed pseudorandom 32-bit signed positive `int` in range `[0, max)`.
pub fn intn(max int) ?int {
	return default_rng.intn(max)
}

// byte returns a uniformly distributed pseudorandom 8-bit unsigned positive `byte`.
pub fn u8() u8 {
	return default_rng.u8()
}

// int_in_range returns a uniformly distributed pseudorandom  32-bit signed int in range `[min, max)`.
// Both `min` and `max` can be negative, but we must have `min < max`.
pub fn int_in_range(min int, max int) ?int {
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
pub fn i64n(max i64) ?i64 {
	return default_rng.i64n(max)
}

// i64_in_range returns a uniformly distributed pseudorandom 64-bit signed `i64` in range `[min, max)`.
pub fn i64_in_range(min i64, max i64) ?i64 {
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
pub fn f32n(max f32) ?f32 {
	return default_rng.f32n(max)
}

// f64n returns a uniformly distributed 64-bit floating point in range `[0, max)`.
pub fn f64n(max f64) ?f64 {
	return default_rng.f64n(max)
}

// f32_in_range returns a uniformly distributed 32-bit floating point in range `[min, max)`.
pub fn f32_in_range(min f32, max f32) ?f32 {
	return default_rng.f32_in_range(min, max)
}

// f64_in_range returns a uniformly distributed 64-bit floating point in range `[min, max)`.
pub fn f64_in_range(min f64, max f64) ?f64 {
	return default_rng.f64_in_range(min, max)
}

// bytes returns a buffer of `bytes_needed` random bytes
pub fn bytes(bytes_needed int) ?[]u8 {
	return default_rng.bytes(bytes_needed)
}

// read fills in `buf` a maximum of `buf.len` random bytes
pub fn read(mut buf []u8) {
	read_internal(mut default_rng, mut buf)
}

const (
	english_letters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
	hex_chars       = 'abcdef0123456789'
	ascii_chars     = '!"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\^_`abcdefghijklmnopqrstuvwxyz{|}~'
)

// ulid generates an Unique Lexicographically sortable IDentifier.
// See https://github.com/ulid/spec .
// Note: ULIDs can leak timing information, if you make them public, because
// you can infer the rate at which some resource is being created, like
// users or business transactions.
// (https://news.ycombinator.com/item?id=14526173)
pub fn ulid() string {
	return default_rng.ulid()
}

// ulid_at_millisecond does the same as `ulid` but takes a custom Unix millisecond timestamp via `unix_time_milli`.
pub fn ulid_at_millisecond(unix_time_milli u64) string {
	return default_rng.ulid_at_millisecond(unix_time_milli)
}

// string_from_set returns a string of length `len` containing random characters sampled from the given `charset`
pub fn string_from_set(charset string, len int) string {
	return default_rng.string_from_set(charset, len)
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

// shuffle randomly permutates the elements in `a`. The range for shuffling is
// optional and the entire array is shuffled by default. Leave the end as 0 to
// shuffle all elements until the end.
pub fn shuffle<T>(mut a []T, config config.ShuffleConfigStruct) ? {
	default_rng.shuffle<T>(mut a, config)?
}

// shuffle_clone returns a random permutation of the elements in `a`.
// The permutation is done on a fresh clone of `a`, so `a` remains unchanged.
pub fn shuffle_clone<T>(a []T, config config.ShuffleConfigStruct) ?[]T {
	return default_rng.shuffle_clone<T>(a, config)
}

// choose samples k elements from the array without replacement.
// This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array.
// Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn choose<T>(array []T, k int) ?[]T {
	return default_rng.choose<T>(array, k)
}

// sample samples k elements from the array with replacement.
// This means the elements can repeat and the size of the sample may exceed the size of the array.
pub fn sample<T>(array []T, k int) []T {
	return default_rng.sample<T>(array, k)
}

// bernoulli returns true with a probability p. Note that 0 <= p <= 1.
pub fn bernoulli(p f64) ?bool {
	return default_rng.bernoulli(p)
}

// normal returns a normally distributed pseudorandom f64 in range `[0, 1)`.
// NOTE: Use normal_pair() instead if you're generating a lot of normal variates.
pub fn normal(conf config.NormalConfigStruct) ?f64 {
	return default_rng.normal(conf)
}

// normal_pair returns a pair of normally distributed pseudorandom f64 in range `[0, 1)`.
pub fn normal_pair(conf config.NormalConfigStruct) ?(f64, f64) {
	return default_rng.normal_pair(conf)
}

// binomial returns the number of successful trials out of n when the
// probability of success for each trial is p.
pub fn binomial(n int, p f64) ?int {
	return default_rng.binomial(n, p)
}

// exponential returns an exponentially distributed random number with the rate paremeter
// lambda. It is expected that lambda is positive.
pub fn exponential(lambda f64) f64 {
	return default_rng.exponential(lambda)
}
