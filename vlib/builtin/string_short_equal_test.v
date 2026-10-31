module builtin

fn test_short_byte_comparisons_are_bounded_and_accept_every_alignment() {
	for size in 0 .. 42 {
		for offset in 0 .. 17 {
			left := []u8{len: offset + size + 1, init: u8(index * 73)}
			mut right := left.clone()
			a := unsafe { &u8(left.data) + offset }
			b := unsafe { &u8(right.data) + offset }
			assert string_bytes_equal(a, b, size)
			for i in 0 .. size {
				right[offset + i] ^= 0x80
				assert !string_bytes_equal(a, b, size), 'size=${size}, offset=${offset}, byte=${i}'
				right[offset + i] ^= 0x80
			}
			// The byte immediately after a borrowed view is outside the comparison.
			right[offset + size] ^= 0x80
			assert string_bytes_equal(a, b, size)
		}
	}
}
