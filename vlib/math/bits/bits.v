module bits

// --- RotateLeft ---

// rotate_left_8 returns the value of x rotated left by (k mod 8) bits.
// To rotate x right by k bits, call rotate_left_8(x, -k).
//
// This function's execution time does not depend on the inputs.
pub fn rotate_left_8(x u8, k int) u8 {
	n := u8(8)
	s := u8(k) & u8(n - u8(1))
	return u8((x<<s) | (x>>(n-s)))
}

// rotate_left_16 returns the value of x rotated left by (k mod 16) bits.
// To rotate x right by k bits, call rotate_left_16(x, -k).
//
// This function's execution time does not depend on the inputs.
pub fn rotate_left_16(x u16, k int) u16 {
	n := u16(16)
	s := u16(k) & (n - u16(1))
	return u16((x<<s) | (x>>(n-s)))
}

// rotate_left_32 returns the value of x rotated left by (k mod 32) bits.
// To rotate x right by k bits, call rotate_left_32(x, -k).
//
// This function's execution time does not depend on the inputs.
pub fn rotate_left_32(x u32, k int) u32 {
	n := u32(32)
	s := u32(k) & (n - u32(1))
	return u32(u32(x<<s) | u32(x>>(n-s)))
}

// rotate_left_64 returns the value of x rotated left by (k mod 64) bits.
// To rotate x right by k bits, call rotate_left_64(x, -k).
//
// This function's execution time does not depend on the inputs.
pub fn rotate_left_64(x u64, k int) u64 {
	n := u64(64)
	s := u64(k) & (n - u64(1))
	return u64(u64(x<<s) | u64(x>>(n-s)))
}