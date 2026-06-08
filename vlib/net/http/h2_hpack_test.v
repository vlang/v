module http

// Tests for the HPACK implementation (RFC 7541). The byte sequences below are
// the worked examples from RFC 7541 Appendix C; decoding them is the canonical
// correctness check for an HPACK decoder.

// hexb parses a hex string (spaces allowed) into bytes.
fn hexb(s string) []u8 {
	clean := s.replace(' ', '').replace('\n', '')
	mut out := []u8{cap: clean.len / 2}
	for i := 0; i + 1 < clean.len; i += 2 {
		hi := hex_nibble(clean[i])
		lo := hex_nibble(clean[i + 1])
		out << u8((hi << 4) | lo)
	}
	return out
}

fn hex_nibble(c u8) u8 {
	return match c {
		`0`...`9` { c - `0` }
		`a`...`f` { c - `a` + 10 }
		`A`...`F` { c - `A` + 10 }
		else { 0 }
	}
}

fn assert_fields(got []H2HeaderField, want [][]string) {
	assert got.len == want.len, 'field count: got ${got.len}, want ${want.len}'
	for i, w in want {
		assert got[i].name == w[0], 'field ${i} name: got "${got[i].name}", want "${w[0]}"'
		assert got[i].value == w[1], 'field ${i} value: got "${got[i].value}", want "${w[1]}"'
	}
}

// --- Integer representation (RFC 7541 Section 5.1) ---

fn test_hpack_integer_examples() {
	// C.1.1: encode 10 with a 5-bit prefix -> single byte 0x0a.
	mut b := []u8{}
	h2_hpack_write_int(mut b, 10, 5, 0)
	assert b == [u8(0x0a)]
	mut r := H2HpackReader{
		buf: b
	}
	assert r.read_int(5)! == 10

	// C.1.2: encode 1337 with a 5-bit prefix -> 0x1f 0x9a 0x0a.
	b = []u8{}
	h2_hpack_write_int(mut b, 1337, 5, 0)
	assert b == [u8(0x1f), 0x9a, 0x0a]
	r = H2HpackReader{
		buf: b
	}
	assert r.read_int(5)! == 1337

	// C.1.3: encode 42 with an 8-bit prefix -> single byte 0x2a.
	b = []u8{}
	h2_hpack_write_int(mut b, 42, 8, 0)
	assert b == [u8(0x2a)]
	r = H2HpackReader{
		buf: b
	}
	assert r.read_int(8)! == 42
}

fn test_hpack_integer_overflow_rejected() {
	// A run of continuation bytes with the high bit always set must not loop
	// forever or overflow; it should error.
	bad := [u8(0x1f), 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80]
	mut r := H2HpackReader{
		buf: bad
	}
	r.read_int(5) or { return }
	assert false, 'expected integer overflow error'
}

// --- Huffman coding (RFC 7541 Section 5.2) ---

fn test_huffman_roundtrip() {
	samples := ['', 'www.example.com', 'no-cache', 'custom-value', '/sample/path',
		'Mon, 21 Oct 2013 20:13:21 GMT', 'https://www.example.com', 'private']
	for s in samples {
		enc := h2_huffman_encode(s.bytes())
		dec := h2_huffman_decode(enc)!
		assert dec.bytestr() == s, 'huffman roundtrip failed for "${s}"'
	}
}

fn test_huffman_known_vector() {
	// "www.example.com" Huffman-encoded, from RFC 7541 C.4.1.
	enc := hexb('f1e3c2e5f23a6ba0ab90f4ff')
	dec := h2_huffman_decode(enc)!
	assert dec.bytestr() == 'www.example.com'
}

fn test_huffman_codes_rebuilt_from_lengths() {
	// The HPACK table now ships only the bit lengths; the canonical codes are
	// rebuilt at startup via hash.huffman. Pin a few known codes from RFC 7541
	// Appendix B so a bad rebuild (or a future builder change) is caught.
	assert h2_huffman_table.codes.len == 257
	assert h2_huffman_table.lengths.len == 257
	// symbol 0 (NUL): 0x1ff8 / 13 bits
	assert h2_huffman_table.codes[0] == 0x1ff8
	assert h2_huffman_table.lengths[0] == 13
	// '0' (0x30): 0x0 / 5 bits, '1': 0x1 / 5 bits, 'a' (0x61): 0x3 / 5 bits
	assert h2_huffman_table.codes[0x30] == 0x0
	assert h2_huffman_table.lengths[0x30] == 5
	assert h2_huffman_table.codes[0x31] == 0x1
	assert h2_huffman_table.codes[0x61] == 0x3
	// EOS (256): 0x3fffffff / 30 bits
	assert h2_huffman_table.codes[256] == 0x3fffffff
	assert h2_huffman_table.lengths[256] == 30
}

fn test_huffman_rejects_padding_not_all_ones() {
	// Valid encoding of "0" is 5 bits (00000); pad the rest of the byte with
	// zeros instead of ones -> invalid per RFC 7541 Section 5.2.
	bad := [u8(0x00)] // '0' code is 00000, then 000 padding (not all ones)
	h2_huffman_decode(bad) or { return }
	assert false, 'expected invalid huffman padding error'
}

// --- C.2: Header field representations ---

fn test_hpack_c_2_1_literal_incremental() {
	mut d := H2HpackDecoder{}
	fields := d.decode(hexb('400a 6375 7374 6f6d 2d6b 6579 0d63 7573 746f 6d2d 6865 6164 6572'))!
	assert_fields(fields, [['custom-key', 'custom-header']])
	// Added to the dynamic table: size = 10 + 13 + 32 = 55.
	assert d.dyn_table.entries.len == 1
	assert d.dyn_table.cur_size == 55
	assert d.dyn_table.entries[0].name == 'custom-key'
}

fn test_hpack_c_2_2_literal_without_indexing() {
	mut d := H2HpackDecoder{}
	fields := d.decode(hexb('040c 2f73 616d 706c 652f 7061 7468'))!
	assert_fields(fields, [[':path', '/sample/path']])
	assert d.dyn_table.entries.len == 0
}

fn test_hpack_c_2_3_never_indexed() {
	mut d := H2HpackDecoder{}
	fields := d.decode(hexb('1008 7061 7373 776f 7264 0673 6563 7265 74'))!
	assert_fields(fields, [['password', 'secret']])
	assert d.dyn_table.entries.len == 0
}

fn test_hpack_c_2_4_indexed() {
	mut d := H2HpackDecoder{}
	fields := d.decode(hexb('82'))!
	assert_fields(fields, [[':method', 'GET']])
}

// --- C.3: Request sequence without Huffman, shared decoder ---

fn test_hpack_c_3_request_sequence() {
	mut d := H2HpackDecoder{}

	f1 := d.decode(hexb('8286 8441 0f77 7777 2e65 7861 6d70 6c65 2e63 6f6d'))!
	assert_fields(f1, [[':method', 'GET'], [':scheme', 'http'],
		[':path', '/'], [':authority', 'www.example.com']])
	assert d.dyn_table.cur_size == 57

	f2 := d.decode(hexb('8286 84be 5808 6e6f 2d63 6163 6865'))!
	assert_fields(f2, [[':method', 'GET'], [':scheme', 'http'],
		[':path', '/'], [':authority', 'www.example.com'], ['cache-control', 'no-cache']])

	f3 :=
		d.decode(hexb('8287 85bf 400a 6375 7374 6f6d 2d6b 6579 0c63 7573 746f 6d2d 7661 6c75 65'))!
	assert_fields(f3, [[':method', 'GET'], [':scheme', 'https'],
		[':path', '/index.html'], [':authority', 'www.example.com'],
		['custom-key', 'custom-value']])
}

// --- C.4: Request sequence with Huffman, shared decoder ---

fn test_hpack_c_4_request_sequence_huffman() {
	mut d := H2HpackDecoder{}

	f1 := d.decode(hexb('8286 8441 8cf1 e3c2 e5f2 3a6b a0ab 90f4 ff'))!
	assert_fields(f1, [[':method', 'GET'], [':scheme', 'http'],
		[':path', '/'], [':authority', 'www.example.com']])

	f2 := d.decode(hexb('8286 84be 5886 a8eb 1064 9cbf'))!
	assert_fields(f2, [[':method', 'GET'], [':scheme', 'http'],
		[':path', '/'], [':authority', 'www.example.com'], ['cache-control', 'no-cache']])

	f3 := d.decode(hexb('8287 85bf 4088 25a8 49e9 5ba9 7d7f 8925 a849 e95b b8e8 b4bf'))!
	assert_fields(f3, [[':method', 'GET'], [':scheme', 'https'],
		[':path', '/index.html'], [':authority', 'www.example.com'],
		['custom-key', 'custom-value']])
}

// --- Dynamic table eviction (RFC 7541 Sections 4.3, 4.4) ---

fn test_dyn_table_eviction_on_add() {
	// Mirrors python-hpack's eviction test: a 66-byte table holds only one of
	// these two entries at a time.
	mut t := H2DynTable{
		max_size: 66
	}
	t.add('a', 'b') // size = 1 + 1 + 32 = 34
	assert t.entries.len == 1
	assert t.cur_size == 34
	t.add('long-custom-header', 'longish value') // size = 18 + 13 + 32 = 63
	assert t.entries.len == 1
	assert t.entries[0].name == 'long-custom-header'
	assert t.cur_size == 63
}

fn test_dyn_table_oversized_entry_empties_table() {
	mut t := H2DynTable{
		max_size: 64
	}
	t.add('a', 'b')
	assert t.entries.len == 1
	// An entry larger than the whole table empties it and is not added.
	t.add('x'.repeat(100), '')
	assert t.entries.len == 0
	assert t.cur_size == 0
}

fn test_dyn_table_resize_evicts() {
	mut t := H2DynTable{}
	t.add('a', 'b')
	t.add('c', 'd')
	assert t.entries.len == 2
	t.set_max_size(34) // room for exactly one 34-byte entry (the newest)
	assert t.entries.len == 1
	assert t.entries[0].name == 'c'
	t.set_max_size(0)
	assert t.entries.len == 0
	assert t.cur_size == 0
}

// A "size update then re-add" sequence exercised through the decoder: an
// indexed reference to an evicted entry must fail.
fn test_decoder_dynamic_indexing_and_eviction() {
	mut d := H2HpackDecoder{}
	// Literal incremental indexing of custom-key: custom-header (size 55).
	_ := d.decode(hexb('400a 6375 7374 6f6d 2d6b 6579 0d63 7573 746f 6d2d 6865 6164 6572'))!
	assert d.dyn_table.entries.len == 1
	// Index 62 now refers to that entry.
	f := d.decode([u8(0xbe)])!
	assert_fields(f, [['custom-key', 'custom-header']])
	// Shrinking the table to 0 evicts it; index 62 is then out of range.
	d.decode([u8(0x20)])! // dynamic table size update to 0
	assert d.dyn_table.entries.len == 0
	d.decode([u8(0xbe)]) or { return }
	assert false, 'expected out-of-range error after eviction'
}

// --- Encoder + round-trip ---

fn test_hpack_encode_indexed_static() {
	mut e := H2HpackEncoder{}
	// :method GET is static index 2 -> single byte 0x82.
	out := e.encode([H2HeaderField{':method', 'GET'}])
	assert out == [u8(0x82)]
}

fn test_hpack_roundtrip() {
	fields := [
		H2HeaderField{':method', 'GET'},
		H2HeaderField{':scheme', 'https'},
		H2HeaderField{':authority', 'example.com'},
		H2HeaderField{':path', '/index.html'},
		H2HeaderField{'user-agent', 'v.http/0.1'},
		H2HeaderField{'accept', '*/*'},
		H2HeaderField{'cookie', 'session=abc123'},
	]
	mut e := H2HpackEncoder{}
	mut d := H2HpackDecoder{}
	encoded := e.encode(fields)
	decoded := d.decode(encoded)!
	assert_fields(decoded, [[':method', 'GET'], [':scheme', 'https'],
		[':authority', 'example.com'], [':path', '/index.html'],
		['user-agent', 'v.http/0.1'], ['accept', '*/*'], ['cookie', 'session=abc123']])
}

// --- Decoder error handling ---

fn test_hpack_rejects_zero_index() {
	mut d := H2HpackDecoder{}
	d.decode([u8(0x80)]) or { return } // indexed header field, index 0
	assert false, 'expected error for index 0'
}

fn test_hpack_rejects_out_of_range_index() {
	mut d := H2HpackDecoder{}
	d.decode([u8(0xff), 0x00]) or { return } // index 62, dynamic table empty
	assert false, 'expected error for out-of-range index'
}

fn test_hpack_rejects_size_update_after_field() {
	mut d := H2HpackDecoder{}
	// Indexed field (0x82) followed by a dynamic table size update (0x20).
	d.decode([u8(0x82), 0x20]) or { return }
	assert false, 'expected error for size update after field'
}

fn test_hpack_rejects_size_update_over_limit() {
	mut d := H2HpackDecoder{}
	d.set_max_dynamic_size(4096)
	// 0x3f e0 0f = dynamic table size update to 4096+... well over 4096.
	// Dynamic table size update to 8192 (> 4096 limit).
	d.decode([u8(0x3f), 0xe1, 0x3f]) or { return }
	assert false, 'expected error for size update over limit'
}

fn test_hpack_rejects_truncating_index() {
	mut d := H2HpackDecoder{}
	// Insert one dynamic entry, so dynamic index 1 (HPACK index 62) is valid.
	_ := d.decode(hexb('400a 6375 7374 6f6d 2d6b 6579 0d63 7573 746f 6d2d 6865 6164 6572'))!
	// Indexed representation with idx = 2^32 + 62: it truncates to 62 (a valid
	// dynamic index) when narrowed to a 32-bit int, but must be rejected.
	mut block := []u8{}
	h2_hpack_write_int(mut block, u64(0x1_0000_0000) + 62, 7, 0x80)
	d.decode(block) or { return }
	assert false, 'expected out-of-range error for truncating index'
}

fn test_hpack_rejects_truncating_string_length() {
	mut d := H2HpackDecoder{}
	mut block := []u8{}
	block << 0x00 // literal without indexing, name index 0
	block << 0x00 // empty name (H=0, length 0)
	// Value string length = 2^32 + 5, which truncates to 5 in a 32-bit int.
	h2_hpack_write_int(mut block, u64(0x1_0000_0000) + 5, 7, 0x00)
	// No value bytes follow; the oversized length must be rejected cleanly.
	d.decode(block) or { return }
	assert false, 'expected length-exceeds-buffer error for truncating string length'
}
