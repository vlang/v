module term

fn test_utf8_len_counts_the_leading_zero_bits() {
	assert utf8_len(0x00) == 7
	assert utf8_len(0x01) == 7
	assert utf8_len(0x02) == 6
	assert utf8_len(0x04) == 5
	assert utf8_len(0x08) == 4
	assert utf8_len(0x10) == 3
	assert utf8_len(0x20) == 2
	assert utf8_len(0x40) == 1
	assert utf8_len(0x80) == 0
	assert utf8_len(0xC0) == 0
	assert utf8_len(0xFF) == 0
}

fn test_utf8_len_agrees_with_a_leading_zero_count_for_every_byte() {
	for b in 0 .. 256 {
		mut expected := 0
		for shift := 7; shift >= 0; shift-- {
			if (b & (1 << shift)) != 0 {
				break
			}
			expected++
		}
		// an all zero byte has eight leading zeros, but the count saturates
		if expected > 7 {
			expected = 7
		}
		assert utf8_len(u8(b)) == expected, 'byte ${b}'
	}
}

fn test_utf8_len_of_the_complemented_lead_byte_gives_the_sequence_length() {
	// utf8_getchar passes u8(~c), so the leading zero bits of the complement
	// are the leading one bits of the lead byte.
	assert utf8_len(u8(~0x00)) == 0
	assert utf8_len(u8(~0x7F)) == 0
	assert utf8_len(u8(~0x80)) == 1
	assert utf8_len(u8(~0xC0)) == 2
	assert utf8_len(u8(~0xDF)) == 2
	assert utf8_len(u8(~0xE0)) == 3
	assert utf8_len(u8(~0xEF)) == 3
	assert utf8_len(u8(~0xF0)) == 4
	assert utf8_len(u8(~0xF7)) == 4
	assert utf8_len(u8(~0xFF)) == 7
}

fn test_utf8_len_separates_every_class_of_lead_byte() {
	for b in 0 .. 256 {
		len := utf8_len(u8(~b))
		assert len <= 7, 'byte ${b}'
		if b < 0x80 {
			assert len == 0, 'plain byte ${b}'
		} else if b < 0xC0 {
			assert len == 1, 'continuation byte ${b}'
		} else if b < 0xE0 {
			assert len == 2, 'two byte lead ${b}'
		} else if b < 0xF0 {
			assert len == 3, 'three byte lead ${b}'
		} else {
			assert len >= 4, 'four byte lead ${b}'
		}
	}
}
