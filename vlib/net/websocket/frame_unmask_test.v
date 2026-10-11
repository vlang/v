module websocket

fn test_frame_unmask_all_alignments_lengths_and_masks() {
	mut seed := u64(47631)
	for alignment in 0 .. 16 {
		for size in 0 .. 257 {
			mut mask := []u8{len: 4}
			for i in 0 .. 4 {
				seed = seed * u64(6364136223846793005) + 1
				mask[i] = u8(seed >> 32)
			}
			mut buffer := []u8{len: alignment + size + 16, init: u8(index % 256)}
			original := buffer.clone()
			mut expected := buffer[alignment..alignment + size].clone()
			for i in 0 .. size { expected[i] ^= mask[i % 4] }
			mut payload := unsafe { buffer[alignment..alignment + size] }
			frame_unmask(mut payload, mask)
			assert payload == expected
			assert buffer[..alignment] == original[..alignment]
			assert buffer[alignment + size..] == original[alignment + size..]
			frame_unmask(mut payload, mask)
			assert buffer == original
		}
	}
}

fn test_frame_unmask_large_boundaries_and_known_masks() {
	for mask in [[u8(0), 0, 0, 0], [u8(255), 255, 255, 255], [u8(0), 255, 0, 255],
		[u8(0x37), 0xfa, 0x21, 0x3d]] {
		for alignment in 0 .. 8 {
			for size in [511, 512, 513, 1023, 1024, 1025, 4095, 4096, 4097, 65535, 65536, 65537] {
				mut buffer := []u8{len: alignment + size + 16, init: u8(index % 251)}
				original := buffer.clone()
				mut expected := buffer[alignment..alignment + size].clone()
				for i in 0 .. size { expected[i] ^= mask[i % 4] }
				mut payload := unsafe { buffer[alignment..alignment + size] }
				frame_unmask(mut payload, mask)
				assert payload == expected
				assert buffer[..alignment] == original[..alignment]
				assert buffer[alignment + size..] == original[alignment + size..]
				frame_unmask(mut payload, mask)
				assert buffer == original
			}
		}
	}
}
