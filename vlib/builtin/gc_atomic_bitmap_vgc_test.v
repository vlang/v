// vtest vflags: -gc vgc
// vtest build: !race?
// The atomic mark-bit test-and-set must name exactly the bit that the
// byte-based bitmap accessors read, on every endian: bit `index` lives in
// byte `index>>3`, bit `index&7`. A naive `1u << (index & 31)` word mask
// marks the wrong object on big-endian targets (index 0 would hit object 24).
module builtin

fn test_vgc_atomic_bitmap_matches_byte_bitmap() {
	$if vgc ? {
		for idx in 0 .. 1024 {
			mut bits := []u8{len: 128}
			assert C.vgc_bitmap_get(&u8(bits.data), u32(idx)) == 0
			assert C.vgc_bitmap_atomic_test_set(&u8(bits.data), u32(idx)) == 0
			assert C.vgc_bitmap_get(&u8(bits.data), u32(idx)) != 0
			assert C.vgc_bitmap_atomic_test_set(&u8(bits.data), u32(idx)) == 1
			if idx > 0 {
				assert C.vgc_bitmap_get(&u8(bits.data), u32(idx - 1)) == 0
			}
			if idx < 1023 {
				assert C.vgc_bitmap_get(&u8(bits.data), u32(idx + 1)) == 0
			}
		}
	}
}
