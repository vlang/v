module bits

// reverse_bytes_16(0xAABB) == 0xBBAA and the function is an involution: applying it
// twice returns the original value, and only byte-palindromes are fixed points.
fn test_reverse_bytes_16() {
	assert reverse_bytes_16(0xAABB) == 0xBBAA
	assert reverse_bytes_16(0x1234) == 0x3412
	assert reverse_bytes_16(0xFF00) == 0x00FF
	assert reverse_bytes_16(0x00FF) == 0xFF00
	// Fixed points are the byte palindromes, where both halves are equal.
	assert reverse_bytes_16(0x0000) == 0x0000
	assert reverse_bytes_16(0xFFFF) == 0xFFFF
	assert reverse_bytes_16(0x12) == 0x1200
	assert reverse_bytes_16(0x1200) == 0x12
	for x in [u16(0x0000), 0x0001, 0x00FF, 0x0100, 0x1234, 0x7F80, 0x8001, 0xFFFE, 0xFFFF] {
		assert reverse_bytes_16(reverse_bytes_16(x)) == x
	}
}

fn test_reverse_bytes_32() {
	assert reverse_bytes_32(0x11223344) == 0x44332211
	assert reverse_bytes_32(0x01020304) == 0x04030201
	assert reverse_bytes_32(0xFF000000) == 0x000000FF
	assert reverse_bytes_32(0x000000FF) == 0xFF000000
	// A byte-palindromic word is its own byte reverse.
	assert reverse_bytes_32(0x00000000) == 0x00000000
	assert reverse_bytes_32(0xFFFFFFFF) == 0xFFFFFFFF
	assert reverse_bytes_32(0x12345678) == 0x78563412
	for x in [u32(0x00000000), 0x00000001, 0x000000FF, 0x0000FF00, 0x12345678, 0x7FFFFFFF, 0x80000000,
		0xFFFFFFFE, 0xFFFFFFFF] {
		assert reverse_bytes_32(reverse_bytes_32(x)) == x
	}
}

fn test_reverse_bytes_64() {
	assert reverse_bytes_64(0x1122334455667788) == u64(0x8877665544332211)
	assert reverse_bytes_64(0x0102030405060708) == u64(0x0807060504030201)
	assert reverse_bytes_64(0xFF00000000000000) == u64(0x00000000000000FF)
	assert reverse_bytes_64(0x00000000000000FF) == u64(0xFF00000000000000)
	assert reverse_bytes_64(0x0123456789ABCDEF) == u64(0xEFCDAB8967452301)
	// Byte palindromes are unchanged.
	assert reverse_bytes_64(u64(0)) == u64(0)
	assert reverse_bytes_64(max_u64) == max_u64
	for x in [u64(0), 0x0000000000000001, 0x00000000000000FF, 0x0000000100000000, 0x0123456789ABCDEF,
		0x7FFFFFFFFFFFFFFF, 0x8000000000000000, 0xFFFFFFFFFFFFFFFE, max_u64] {
		assert reverse_bytes_64(reverse_bytes_64(x)) == x
	}
}

// reverse_bytes_* reverses *bytes*, not bits: the reverse of a byte-palindrome is itself,
// whereas reverse_* (the bit reversal) would only fix all-zero and all-one words.
fn test_reverse_bytes_is_not_bit_reversal() {
	assert reverse_bytes_16(0x00FF) == 0xFF00
	assert reverse_16(0x00FF) == u16(0xFF00)
	assert reverse_bytes_16(0x0FF0) == 0xF00F
	assert reverse_16(0x0FF0) != reverse_bytes_16(0x0FF0)
	assert reverse_bytes_32(0x0F0F0F0F) == u32(0x0F0F0F0F)
	assert reverse_32(0x0F0F0F0F) != u32(0x0F0F0F0F)
}

// len_* counts the minimum number of bits needed to represent x, so 0 needs no bits at
// all and a single set bit at position k needs k+1 bits.
fn test_len_8() {
	assert len_8(0) == 0
	assert len_8(1) == 1
	assert len_8(2) == 2
	assert len_8(3) == 2
	assert len_8(0x0F) == 4
	assert len_8(0x10) == 5
	assert len_8(0x7F) == 7
	assert len_8(0x80) == 8
	assert len_8(0xFF) == 8
	for i in 0 .. 8 {
		assert len_8(u8(1) << u8(i)) == i + 1
	}
}

fn test_len_16() {
	assert len_16(0) == 0
	assert len_16(1) == 1
	assert len_16(0x00FF) == 8
	assert len_16(0x0100) == 9
	assert len_16(0x1234) == 13
	assert len_16(0x7FFF) == 15
	assert len_16(0x8000) == 16
	assert len_16(0xFFFF) == 16
	for i in 0 .. 16 {
		assert len_16(u16(1) << u16(i)) == i + 1
	}
}

fn test_len_32() {
	assert len_32(0) == 0
	assert len_32(1) == 1
	assert len_32(0x00FFFFFF) == 24
	assert len_32(0x00010000) == 17
	assert len_32(0x0000FFFF) == 16
	assert len_32(0x01000000) == 25
	assert len_32(0x01234567) == 25
	assert len_32(0x7FFFFFFF) == 31
	assert len_32(0x80000000) == 32
	assert len_32(0xFFFFFFFF) == 32
	for i in 0 .. 32 {
		assert len_32(u32(1) << u32(i)) == i + 1
	}
}

fn test_len_64() {
	assert len_64(0) == 0
	assert len_64(1) == 1
	assert len_64(0x00000000000000FF) == 8
	assert len_64(0x00000000FFFFFFFF) == 32
	assert len_64(0x0000000100000000) == 33
	assert len_64(0x0000000000001234) == 13
	assert len_64(0x7FFFFFFFFFFFFFFF) == 63
	assert len_64(0x8000000000000000) == 64
	assert len_64(max_u64) == 64
	for i in 0 .. 64 {
		assert len_64(u64(1) << u64(i)) == i + 1
	}
}

// len_* agrees with leading_zeros_* and ones_count_*: the complement of the leading
// zeros is the bit length, and every bit below it belongs to the value.
fn test_len_agrees_with_leading_zeros() {
	for x in [u8(0), 1, 0x7F, 0x80, 0xFF] {
		assert leading_zeros_8(x) == 8 - len_8(x)
	}
	for x in [u16(0), 1, 0x00FF, 0x8000, 0xFFFF] {
		assert leading_zeros_16(x) == 16 - len_16(x)
	}
	for x in [u32(0), 1, 0x0000FFFF, 0x80000000, 0xFFFFFFFF] {
		assert leading_zeros_32(x) == 32 - len_32(x)
	}
	for x in [u64(0), 1, 0x00000000FFFFFFFF, 0x8000000000000000, max_u64] {
		assert leading_zeros_64(x) == 64 - len_64(x)
	}
}

// The smallest and largest values sharing a bit length n are 2**(n-1) and 2**n - 1, so
// both must report the same length, and one more bit must report n+1.
fn test_len_covers_every_bit_below_it() {
	for n in 1 .. 9 {
		lowest := u8(1) << u8(n - 1)
		highest := u8((u16(1) << u8(n)) - 1)
		assert len_8(lowest) == n
		assert len_8(highest) == n
		if n < 8 {
			assert len_8(highest + 1) == n + 1
		}
	}
	for n in 1 .. 33 {
		lowest := u32(1) << u32(n - 1)
		highest := u32((u64(1) << u32(n)) - 1)
		assert len_32(lowest) == n
		assert len_32(highest) == n
		if n < 32 {
			assert len_32(highest + 1) == n + 1
		}
	}
	for n in 1 .. 65 {
		lowest := u64(1) << u64(n - 1)
		highest := (u64(1) << u64(n)) - 1
		assert len_64(lowest) == n
		assert len_64(highest) == n
		if n < 64 {
			assert len_64(highest + 1) == n + 1
		}
	}
}
