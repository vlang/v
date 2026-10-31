module builtin

fn test_memcpy_preserves_unaligned_spans_and_surrounding_bytes() {
	for size in 0 .. 66 {
		for src_offset in 0 .. 16 {
			for dst_offset in 0 .. 16 {
				source := []u8{len: size + src_offset, init: u8(index * 37 + 1)}
				mut target := []u8{len: size + dst_offset + 16, init: 0xa5}
				unsafe {
					dst := &u8(target.data) + dst_offset
					src := &u8(source.data) + src_offset
					assert vmemcpy(dst, src, size) == dst
				}
				for i, value in target {
					assert value == if i >= dst_offset && i < dst_offset + size {
						source[src_offset + i - dst_offset]
					} else {
						u8(0xa5)
					}
				}
			}
		}
	}
}

fn test_memmove_preserves_overlapping_spans_and_surrounding_bytes() {
	for size in 0 .. 66 {
		for src_offset in 0 .. 66 {
			for dst_offset in 0 .. 66 {
				mut target := []u8{len: 132, init: u8(index * 37 + 1)}
				original := target.clone()
				unsafe {
					dst := &u8(target.data) + dst_offset
					src := &u8(target.data) + src_offset
					assert vmemmove(dst, src, size) == dst
				}
				for i, value in target {
					assert value == if i >= dst_offset && i < dst_offset + size {
						original[src_offset + i - dst_offset]
					} else {
						original[i]
					}
				}
			}
		}
	}
}
