module websocket

import encoding.utf8

fn test_frame_utf8_every_alignment_length_and_byte_matches_validator() {
	// One non-ASCII byte surrounded by ASCII cannot form a valid multibyte rune.
	// The validator's result for the byte alone applies at every position.
	mut expected := [256]bool{}
	for value in 0 .. expected.len {
		byte_value := u8(value)
		expected[value] = utf8.validate(&byte_value, 1)
	}
	for alignment in 0 .. 16 {
		for length in 0 .. 257 {
			mut storage := []u8{len: alignment + length + 16, init: u8(0x5a)}
			mut payload := unsafe { storage[alignment..alignment + length] }
			assert frame_text_valid(payload)
			if length > 0 {
				// Exercise the beginning, an interior word and the scalar tail.
				positions := [0, length / 2, length - 1]!
				for value, valid in expected {
					for position in positions {
						payload[position] = u8(value)
						assert frame_text_valid(payload) == valid
						payload[position] = 0x5a
					}
				}
			}
			assert storage.all(it == 0x5a)
		}
	}
}

fn test_frame_utf8_unicode_truncations_and_random_inputs_match_validator() {
	for prefix in 0 .. 32 {
		for text in ['مرحبا 👋 café', '中文', '\u0000', 'a😀z'] {
			bytes := ('a'.repeat(prefix) + text).bytes()
			for cut in 0 .. bytes.len + 1 {
				payload := bytes[..cut]
				assert frame_text_valid(payload) == utf8.validate(payload.data, payload.len)
			}
		}
	}
	mut random := u32(19378541)
	for iteration in 0 .. 20000 {
		length := iteration % 513
		mask := if iteration % 2 == 0 { u8(0xff) } else { u8(0x7f) }
		mut payload := []u8{len: length}
		for i in 0 .. length {
			random ^= random << 13
			random ^= random >> 17
			random ^= random << 5
			payload[i] = u8(random) & mask
		}
		assert frame_text_valid(payload) == utf8.validate(payload.data, payload.len)
	}
}

fn test_frame_utf8_word_boundaries_with_no_trailing_allocation() {
	for alignment in 0 .. 16 {
		for length in [0, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 65, 127, 128, 129, 511, 512,
			513, 1023, 1024, 1025, 16383, 16384, 16385] {
			// No readable suffix after the slice: ASan catches reads beyond its end.
			mut storage := []u8{len: alignment + length, cap: alignment + length, init: u8(0)}
			mut payload := unsafe { storage[alignment..] }
			assert frame_text_valid(payload)
			if length > 0 {
				payload[length - 1] = 0x7f
				assert frame_text_valid(payload)
				payload[length - 1] = 0x80
				assert !frame_text_valid(payload)
				payload[length - 1] = 0
			}
			assert storage.all(it == 0)
		}
	}
}

fn test_frame_utf8_rejects_invalid_sequences_after_ascii_prefixes() {
	invalid := [
		[u8(0x80)],
		[u8(0xc0), 0xaf],
		[u8(0xc2)],
		[u8(0xe0), 0x80, 0xaf],
		[u8(0xed), 0xa0, 0x80],
		[u8(0xef), 0xbf],
		[u8(0xf0), 0x80, 0x80, 0xaf],
		[u8(0xf4), 0x90, 0x80, 0x80],
		[u8(0xf5), 0x80, 0x80, 0x80],
		[u8(0xf0), 0x9f, 0x98],
		[u8(0xff)],
	]
	for prefix in 0 .. 33 {
		for sequence in invalid {
			mut payload := []u8{len: prefix, init: u8(`a`)}
			payload << sequence
			assert !utf8.validate(payload.data, payload.len)
			assert !frame_text_valid(payload)
			payload << `z`
			assert !frame_text_valid(payload)
		}
	}
}
