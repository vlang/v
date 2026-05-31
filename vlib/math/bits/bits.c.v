// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

fn C.__builtin_clz(x u32) i32
fn C.__builtin_clzll(x u64) i32
fn C.__lzcnt(x u32) i32
fn C.__lzcnt64(x u64) i32

// --- LeadingZeros ---

// leading_zeros_8 returns the number of leading zero bits in x; the result is 8 for x == 0.
@[inline]
pub fn leading_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	$if msvc {
		return C.__lzcnt(x) - 24
	} $else $if !tinyc {
		return C.__builtin_clz(x) - 24
	}
	return leading_zeros_8_default(x)
}

// leading_zeros_16 returns the number of leading zero bits in x; the result is 16 for x == 0.
@[inline]
pub fn leading_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	$if msvc {
		return C.__lzcnt(x) - 16
	} $else $if !tinyc {
		return C.__builtin_clz(x) - 16
	}
	return leading_zeros_16_default(x)
}

// leading_zeros_32 returns the number of leading zero bits in x; the result is 32 for x == 0.
@[inline]
pub fn leading_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	$if msvc {
		return C.__lzcnt(x)
	} $else $if !tinyc {
		return C.__builtin_clz(x)
	}
	return leading_zeros_32_default(x)
}

// leading_zeros_64 returns the number of leading zero bits in x; the result is 64 for x == 0.
@[inline]
pub fn leading_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	$if msvc {
		return C.__lzcnt64(x)
	} $else $if !tinyc {
		return C.__builtin_clzll(x)
	}
	return leading_zeros_64_default(x)
}

fn C.__builtin_ctz(x u32) i32
fn C.__builtin_ctzll(x u64) i32
fn C._BitScanForward(pos &int, x u32) u8
fn C._BitScanForward64(pos &int, x u64) u8

// --- TrailingZeros ---

// trailing_zeros_8 returns the number of trailing zero bits in x; the result is 8 for x == 0.
@[inline]
pub fn trailing_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else $if !tinyc {
		return C.__builtin_ctz(x)
	}
	return trailing_zeros_8_default(x)
}

// trailing_zeros_16 returns the number of trailing zero bits in x; the result is 16 for x == 0.
@[inline]
pub fn trailing_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else $if !tinyc {
		return C.__builtin_ctz(x)
	}
	return trailing_zeros_16_default(x)
}

// trailing_zeros_32 returns the number of trailing zero bits in x; the result is 32 for x == 0.
@[inline]
pub fn trailing_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else $if !tinyc {
		return C.__builtin_ctz(x)
	}
	return trailing_zeros_32_default(x)
}

// trailing_zeros_64 returns the number of trailing zero bits in x; the result is 64 for x == 0.
@[inline]
pub fn trailing_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward64(&pos, x)
		return pos
	} $else $if !tinyc {
		return C.__builtin_ctzll(x)
	}
	return trailing_zeros_64_default(x)
}

fn C.__builtin_popcount(x u32) i32
fn C.__builtin_popcountll(x u64) i32
fn C.__popcnt(x u32) i32
fn C.__popcnt64(x u64) i32
fn C.__builtin_bswap16(x u16) u16
fn C.__builtin_bswap32(x u32) u32
fn C.__builtin_bswap64(x u64) u64
fn C._byteswap_ushort(x u16) u16
fn C._byteswap_ulong(x u32) u32
fn C._byteswap_uint64(x u64) u64

// --- OnesCount ---

// ones_count_8 returns the number of one bits ("population count") in x.
@[inline]
pub fn ones_count_8(x u8) int {
	$if msvc {
		return C.__popcnt(x)
	} $else $if !tinyc {
		return C.__builtin_popcount(x)
	}
	return ones_count_8_default(x)
}

// ones_count_16 returns the number of one bits ("population count") in x.
@[inline]
pub fn ones_count_16(x u16) int {
	$if msvc {
		return C.__popcnt(x)
	} $else $if !tinyc {
		return C.__builtin_popcount(x)
	}
	return ones_count_16_default(x)
}

// ones_count_32 returns the number of one bits ("population count") in x.
@[inline]
pub fn ones_count_32(x u32) int {
	$if msvc {
		return C.__popcnt(x)
	} $else $if !tinyc {
		return C.__builtin_popcount(x)
	}
	return ones_count_32_default(x)
}

// ones_count_64 returns the number of one bits ("population count") in x.
@[inline]
pub fn ones_count_64(x u64) int {
	$if msvc {
		return C.__popcnt64(x)
	} $else $if !tinyc {
		return C.__builtin_popcountll(x)
	}
	return ones_count_64_default(x)
}

// reverse_bytes_16 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_16(x u16) u16 {
	$if msvc {
		return C._byteswap_ushort(x)
	} $else $if !tinyc {
		return C.__builtin_bswap16(x)
	}
	return reverse_bytes_16_default(x)
}

// reverse_bytes_32 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_32(x u32) u32 {
	$if msvc {
		return C._byteswap_ulong(x)
	} $else $if !tinyc {
		return C.__builtin_bswap32(x)
	}
	return reverse_bytes_32_default(x)
}

// reverse_bytes_64 returns the value of x with its bytes in reversed order.
//
// This function's execution time does not depend on the inputs.
@[inline]
pub fn reverse_bytes_64(x u64) u64 {
	$if msvc {
		return C._byteswap_uint64(x)
	} $else $if !tinyc {
		return C.__builtin_bswap64(x)
	}
	return reverse_bytes_64_default(x)
}
