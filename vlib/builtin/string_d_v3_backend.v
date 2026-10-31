// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

// The 128-bit integer types exist only in V3. V3 always selects this file for
// builtin, with its internal `v3_backend` define. The compatibility compiler, which
// still checks some legacy fixtures against this vlib, does not know these types.

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
