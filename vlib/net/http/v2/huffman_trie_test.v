module v2

// test_huffman_trie_all_256_bytes tests that all possible byte values
// can be encoded and decoded correctly via the Huffman trie
fn test_huffman_trie_all_256_bytes() {
	mut data := []u8{len: 256}
	for i in 0 .. 256 {
		data[i] = u8(i)
	}

	encoded := encode_huffman(data)
	assert encoded.len > 0, 'Encoded data should not be empty'

	decoded := decode_huffman(encoded) or {
		assert false, 'Failed to decode all 256 bytes: ${err}'
		return
	}

	assert decoded.len == 256, 'Decoded length mismatch: expected 256, got ${decoded.len}'
	for i in 0 .. 256 {
		assert decoded[i] == u8(i), 'Byte mismatch at index ${i}: expected ${u8(i)}, got ${decoded[i]}'
	}
	println('✓ All 256 bytes Huffman trie round-trip test passed')
}

// test_huffman_trie_invalid_padding tests that invalid padding is rejected
fn test_huffman_trie_invalid_padding() {
	// 'a' = symbol 97, code 0x3 (00011), 5 bits.
	// Correct single-byte encoding: 00011_111 = 0x1F (3 bits of all-1 padding)
	// Invalid: 00011_010 = 0x1A (padding is not all-1s)
	invalid := [u8(0x1A)]
	decode_huffman(invalid) or {
		// Expected: decoding should fail with invalid padding
		println('✓ Invalid padding correctly rejected: ${err}')
		return
	}
	assert false, 'Should have rejected invalid Huffman padding for input 0x1A'
}

// test_huffman_trie_too_many_padding_bits tests that > 7 padding bits is rejected
fn test_huffman_trie_too_many_padding_bits() {
	// 'a' (5 bits: 00011) packed into first byte with correct EOS padding: 0x1F
	// Then an extra 0xFF byte means 8 more remainder bits, totalling 11 bits
	// after the last symbol — exceeds the max 7-bit padding limit.
	too_much_padding := [u8(0x1F), 0xFF]
	decode_huffman(too_much_padding) or {
		println('✓ Too many padding bits correctly rejected: ${err}')
		return
	}
	assert false, 'Should have rejected input with > 7 padding bits'
}

// test_huffman_trie_empty_input tests that empty input decodes to empty output
fn test_huffman_trie_empty_input() {
	decoded := decode_huffman([]) or {
		assert false, 'Empty input decode should not error: ${err}'
		return
	}
	assert decoded.len == 0, 'Empty input should decode to empty output'
	println('✓ Empty input Huffman trie test passed')
}

// test_huffman_trie_single_bytes tests individual byte round-trips
fn test_huffman_trie_single_bytes() {
	// Test common ASCII bytes individually — covers multiple code lengths
	test_bytes := [u8(0), 32, 48, 57, 65, 90, 97, 122, 127, 255]
	for b in test_bytes {
		encoded := encode_huffman([b])
		decoded := decode_huffman(encoded) or {
			assert false, 'Failed to decode single byte ${b}: ${err}'
			return
		}
		assert decoded.len == 1, 'Expected 1 decoded byte for input ${b}, got ${decoded.len}'
		assert decoded[0] == b, 'Single byte mismatch: expected ${b}, got ${decoded[0]}'
	}
	println('✓ Single byte Huffman trie round-trip tests passed')
}
