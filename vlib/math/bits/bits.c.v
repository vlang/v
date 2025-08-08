// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module bits

fn C.__builtin_clz(x u32) int
fn C.__builtin_clzll(x u64) int
fn C.__lzcnt(x u32) int
fn C.__lzcnt64(x u64) int

@[inline]
pub fn leading_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	$if msvc {
		return C.__lzcnt(x) - 24
	} $else {
		return C.__builtin_clz(x) - 24
	}
}

@[inline]
pub fn leading_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	$if msvc {
		return C.__lzcnt(x) - 16
	} $else {
		return C.__builtin_clz(x) - 16
	}
}

@[inline]
pub fn leading_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	$if msvc {
		return C.__lzcnt(x)
	} $else {
		return C.__builtin_clz(x)
	}
}

@[inline]
pub fn leading_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	$if msvc {
		return C.__lzcnt64(x)
	} $else {
		return C.__builtin_clzll(x)
	}
}

fn C.__builtin_ctz(x u32) int
fn C.__builtin_ctzll(x u64) int
fn C._BitScanForward(pos &int, x u32) u8
fn C._BitScanForward64(pos &int, x u64) u8

@[inline]
pub fn trailing_zeros_8(x u8) int {
	if x == 0 {
		return 8
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else {
		return C.__builtin_ctz(x)
	}
}

@[inline]
pub fn trailing_zeros_16(x u16) int {
	if x == 0 {
		return 16
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else {
		return C.__builtin_ctz(x)
	}
}

@[inline]
pub fn trailing_zeros_32(x u32) int {
	if x == 0 {
		return 32
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else {
		return C.__builtin_ctz(x)
	}
}

@[inline]
pub fn trailing_zeros_64(x u64) int {
	if x == 0 {
		return 64
	}
	$if msvc {
		mut pos := 0
		_ := C._BitScanForward(&pos, x)
		return pos
	} $else {
		return C.__builtin_ctzll(x)
	}
}

fn C.__builtin_popcount(x u32) int
fn C.__builtin_popcountll(x u64) int
fn C.__popcnt(x u32) int
fn C.__popcnt64(x u64) int

@[inline]
pub fn ones_count_8(x u8) int {
	$if msvc {
		return C.__popcnt(x)
	} $else {
		return C.__builtin_popcount(x)
	}
}

@[inline]
pub fn ones_count_16(x u16) int {
	$if msvc {
		return C.__popcnt(x)
	} $else {
		return C.__builtin_popcount(x)
	}
}

@[inline]
pub fn ones_count_32(x u32) int {
	$if msvc {
		return C.__popcnt(x)
	} $else {
		return C.__builtin_popcount(x)
	}
}

@[inline]
pub fn ones_count_64(x u64) int {
	$if msvc {
		return C.__popcnt64(x)
	} $else {
		return C.__builtin_popcountll(x)
	}
}
