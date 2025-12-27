module hash

//#flag -I @VEXEROOT/thirdparty/wyhash
//#include "wyhash.h"
fn C.wyhash(&u8, u64, u64, &u64) u64

fn C.wyhash64(u64, u64) u64
fn C.wyrand(&u64) u64
fn C.wy2u01(u64) f64
fn C.wy2gau(u64) f64

fn C.make_secret(u64, &u64)

// wyhash_c returns a hash given a byte string `key`, its `len`, and a `seed`.
@[inline]
pub fn wyhash(key &u8, len u64, seed u64, secret &u64) u64 {
	return C.wyhash(key, len, seed, secret)
}

// wyhash – convenience version using default secret
@[inline]
pub fn wyhash_c(key &u8, len u64, seed u64) u64 {
	return C.wyhash(key, len, seed, &u64(voidptr(C._wyp)))
}

// wyhash64_c returns a hash given two u64 values `a` and `b`.
@[inline]
pub fn wyhash64_c(a u64, b u64) u64 {
	return C.wyhash64(a, b)
}

// wyrand – fast PRNG (updates seed in place)
@[inline]
pub fn wyrand(seed &u64) u64 {
	return C.wyrand(seed)
}

// wy2u01 – uniform double in [0,1)
@[inline]
pub fn wy2u01(r u64) f64 {
	return C.wy2u01(r)
}

// wy2gau – approximate Gaussian (mean 0, stddev ~1)
@[inline]
pub fn wy2gau(r u64) f64 {
	return C.wy2gau(r)
}

// sum64_string returns a hash given a V string `key` and a `seed`.
@[inline]
pub fn sum64_string(key string, seed u64) u64 {
	return wyhash_c(key.str, u64(key.len), seed)
}

// sum64 returns a hash given a byte array `key` and a `seed`.
@[inline]
pub fn sum64(key []u8, seed u64) u64 {
	return wyhash_c(&u8(key.data), u64(key.len), seed)
}

// make_secret – generate good secret parameters
pub fn make_secret(seed u64) [4]u64 {
	mut secret := [u64(0), 0, 0, 0]!
	unsafe {
		C.make_secret(seed, &secret[0])
	}
	return secret
}
