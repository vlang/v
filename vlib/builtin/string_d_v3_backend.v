// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// The 128-bit integer types exist only in V3. V3 always selects this file for
// builtin with its internal `v3_backend` define. The portable bootstrap omits this file.

// u128 returns the value of the string as a `u128`, reading decimal digits.
// A string that is not a decimal number gives `u128(0)`, the same as `string.u64()`.
// Example: assert '12345'.u128() == u128(12345)
pub fn (s string) u128() u128 {
	mut i := 0
	if i < s.len && s[i] == `+` {
		i++
	}
	mut result := u128(0)
	for i < s.len {
		c := s[i]
		if c < `0` || c > `9` {
			return u128(0)
		}
		result = result * u128(10) + u128(c - `0`)
		i++
	}
	return result
}

// i128 returns the value of the string as an `i128`, reading an optional sign and
// decimal digits. A string that is not a decimal number gives `i128(0)`.
// Example: assert '-12345'.i128() == i128(-12345)
pub fn (s string) i128() i128 {
	mut i := 0
	mut negative := false
	if i < s.len && (s[i] == `-` || s[i] == `+`) {
		negative = s[i] == `-`
		i++
	}
	mut result := i128(0)
	for i < s.len {
		c := s[i]
		if c < `0` || c > `9` {
			return i128(0)
		}
		result = result * i128(10) + i128(c - `0`)
		i++
	}
	if negative {
		return -result
	}
	return result
}

// at_i128 returns the byte at the i128 index, with the same bounds checking
// as the narrower at_* helpers. It lives in this v3_backend-guarded file
// (rather than string.v) because the 0.5.2 compatibility compiler, which
// checks the current vlib, has no i128/u128 types.
@[markused]
fn (s string) at_i128(idx i128) u8 {
	$if !no_bounds_checking {
		if idx < 0 || idx >= i128(s.len) {
			panic('string index out of range(idx,s.len): ' + idx.str() + ', ' +
				impl_i64_to_string(s.len))
		}
	}
	return unsafe { s.str[int(idx)] }
}

// at_u128 returns the byte at the u128 index, with the same bounds checking
// as the narrower at_* helpers. It lives in this v3_backend-guarded file
// (rather than string.v) because the 0.5.2 compatibility compiler, which
// checks the current vlib, has no i128/u128 types.
@[markused]
fn (s string) at_u128(idx u128) u8 {
	$if !no_bounds_checking {
		if idx >= u128(s.len) {
			panic('string index out of range(idx,s.len): ' + idx.str() + ', ' +
				impl_i64_to_string(s.len))
		}
	}
	return unsafe { s.str[int(idx)] }
}
