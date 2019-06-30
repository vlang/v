// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module bits

// Returns base-2 logarithm for common bit counts (8, 16, 32, 64), as
// a constant expression.
pub fn bits_log2(a int) int {
	switch a {
		case 8: return 3
		case 16: return 4
		case 32: return 5
		case 64: return 6
	}	
}

// Returns the power-of-two number closest to the provided value in i32.
pub fn closest_pow2_i32(a i32) i32 {
	next := next_pow2_i32(a)
	prev := i32(next >> i32(1))
	
	if a - prev < next - a {
		return prev	
	}

	return next
}

// Returns the power-of-two number closest to the provided value.
pub fn closest_pow2(a int) int {
	next := next_pow2(a)
	prev := next >> 1
	
	if a - prev < next - a {
		return prev	
	}

	return next
}

// Determines whether the number is power-of-two or not.
pub fn is_pow2(a int) bool {
	return (a & (a - 1)) == 0	
}

// Returns the power-of-two number greater or equal to the provided value in i32. 
pub fn next_pow2_i32(a i32) i32 {
	a = i32(a - i32(1))
	a |= i32(a >> i32(16))
	a |= i32(a >> i32(8))
	a |= i32(a >> i32(4))
	a |= i32(a >> i32(2))
	a |= i32(a >> i32(1)) 
	a = i32(a + i32(1))
	return a	
}

// Returns the power-of-two number greater or equal to the provided value.
pub fn next_pow2(a int) int {
	a = a - 1
	a |= a >> 16
	a |= a >> 8
	a |= a >> 4
	a |= a >> 2
	a |= a >> 1 
	a = a + 1
	return a	
}
