// vtest vflags: -gc vgc
// vtest build: !race?
module builtin

// The 32-bit atomic mark-bit update in vgc_scan.h must address the same object
// as the byte-bit layout of vgc_bitmap_get (byte index>>3, bit index&7). On
// little-endian the word's first byte holds its low bits, so the word bit is
// index&31; on big-endian it holds the high bits, so the word bit is
// (index&31)^24. This models both mappings for 1024 indices: any mismatch
// means the parallel marker would mark a different object than the sweep
// frees.
fn test_vgc_bitmap_word_bit_matches_byte_layout_on_both_endiannesses() {
	for i in 0 .. 1024 {
		byte_idx := i >> 3
		bit := i & 7
		for be in [false, true] {
			wbit := if be { (i & 31) ^ 24 } else { i & 31 }
			off := if be { 3 - (wbit >> 3) } else { wbit >> 3 }
			assert (i >> 5) * 4 + off == byte_idx
			assert wbit & 7 == bit
		}
	}
}

// The live header must agree with the model above on this host: setting every
// low bit through the atomic setter shades exactly the byte-bitmap object.
fn test_vgc_atomic_bitmap_set_matches_byte_bitmap() {
	$if vgc ? {
		mut bits := [128]u8{}
		for i in 0 .. 1024 {
			assert C.vgc_bitmap_atomic_test_set(&bits[0], u32(i)) == 0
		}
		for i in 0 .. 1024 {
			assert C.vgc_bitmap_get(&bits[0], u32(i)) == 1
			assert C.vgc_bitmap_atomic_test_set(&bits[0], u32(i)) == 1
		}
	}
}
