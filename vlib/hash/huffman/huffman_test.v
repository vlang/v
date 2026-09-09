module huffman

// The worked example from RFC 1951 §3.2.2: symbols A..H with these lengths
// produce these exact canonical (MSB-first) codes.
fn test_rfc1951_canonical_example() {
	lengths := [3, 3, 3, 3, 3, 2, 4, 4] // A B C D E F G H
	t := build(lengths: lengths, max_bits: 4, bit_order: .msb_first)!
	expected := [u32(0b010), 0b011, 0b100, 0b101, 0b110, 0b00, 0b1110, 0b1111]
	assert t.codes == expected
	assert t.lengths == lengths
	assert t.max_bits == 4
}

fn test_lsb_first_reverses_each_code() {
	lengths := [3, 3, 3, 3, 3, 2, 4, 4]
	msb := build(lengths: lengths, max_bits: 4, bit_order: .msb_first)!
	lsb := build(lengths: lengths, max_bits: 4, bit_order: .lsb_first)!
	// Each LSB code is the MSB code bit-reversed within its length.
	for sym, l in lengths {
		assert lsb.codes[sym] == bit_reverse(msb.codes[sym], l)
	}
	// e.g. F (len 2, code 00) is unchanged; A (len 3, 010) -> 010 reversed.
	assert lsb.codes[5] == 0b00
	assert lsb.codes[0] == 0b010 // 010 reversed is still 010
	assert lsb.codes[6] == bit_reverse(u32(0b1110), 4) // 1110 -> 0111
}

fn test_flat_table_round_trips_lsb() {
	lengths := [3, 3, 3, 3, 3, 2, 4, 4]
	t := build(lengths: lengths, max_bits: 4, bit_order: .lsb_first)!
	table := flat_table(lengths: lengths, max_bits: 4, bit_order: .lsb_first)!
	assert table.len == 1 << 4
	// Every symbol must decode back from its code in every don't-care variant.
	for sym, l in lengths {
		step := 1 << l
		mut idx := int(t.codes[sym])
		for idx < table.len {
			entry := table[idx]
			assert entry != flat_invalid_entry
			assert int(entry & ((u32(1) << flat_length_bits) - 1)) == l
			assert int(entry >> flat_length_bits) == sym
			idx += step
		}
	}
}

fn test_flat_table_round_trips_msb() {
	// MSB-first flat table: a code of length l fills the contiguous block whose
	// high l bits equal the code (the low max_bits-l bits are don't-cares).
	lengths := [3, 3, 3, 3, 3, 2, 4, 4]
	t := build(lengths: lengths, max_bits: 4, bit_order: .msb_first)!
	table := flat_table(lengths: lengths, max_bits: 4, bit_order: .msb_first)!
	assert table.len == 1 << 4
	for sym, l in lengths {
		block := 1 << (t.max_bits - l)
		base := int(t.codes[sym]) * block
		for k in 0 .. block {
			entry := table[base + k]
			assert entry != flat_invalid_entry
			assert int(entry & ((u32(1) << flat_length_bits) - 1)) == l
			assert int(entry >> flat_length_bits) == sym
		}
	}
}

fn test_flat_table_incomplete_marks_gaps() {
	// A single length-1 code under-subscribes a 2-bit table: half the indices
	// belong to no code and must read back as flat_invalid_entry. This is the
	// path the complete-code fast path must NOT take.
	table := flat_table(lengths: [1], max_bits: 2, bit_order: .lsb_first)!
	assert table.len == 4
	// code 0, len 1, lsb stride 2 -> indices 0 and 2 are the symbol; 1 and 3 gaps.
	assert int(table[0] >> flat_length_bits) == 0
	assert int(table[0] & ((u32(1) << flat_length_bits) - 1)) == 1
	assert table[2] == table[0]
	assert table[1] == flat_invalid_entry
	assert table[3] == flat_invalid_entry
}

fn test_decode_map_msb() {
	lengths := [3, 3, 3, 3, 3, 2, 4, 4]
	t := build(lengths: lengths, max_bits: 4, bit_order: .msb_first)!
	m := t.decode_map()!
	for sym, l in lengths {
		key := (u64(l) << 32) | u64(t.codes[sym])
		assert m[key] == sym
	}
}

fn test_decode_map_rejects_lsb() {
	t := build(lengths: [1, 1], max_bits: 1, bit_order: .lsb_first)!
	if _ := t.decode_map() {
		assert false, 'decode_map should reject lsb_first tables'
	}
}

fn test_unused_symbols_get_zero_code() {
	// A length-0 symbol is unused; it must not consume a code.
	t := build(lengths: [1, 0, 1], max_bits: 1, bit_order: .msb_first)!
	assert t.codes[1] == 0
	assert t.codes[0] == 0
	assert t.codes[2] == 1
}

fn test_error_length_exceeds_max_bits() {
	if _ := build(lengths: [5], max_bits: 4, bit_order: .msb_first) {
		assert false, 'length > max_bits must error'
	}
}

fn test_error_negative_length() {
	if _ := build(lengths: [-1], max_bits: 4, bit_order: .msb_first) {
		assert false, 'negative length must error'
	}
}

fn test_error_max_bits_too_small() {
	if _ := build(lengths: [1], max_bits: 0, bit_order: .msb_first) {
		assert false, 'max_bits < 1 must error'
	}
}

fn test_error_over_subscribed() {
	// Three length-1 codes cannot coexist (only two 1-bit codes exist).
	if _ := build(lengths: [1, 1, 1], max_bits: 1, bit_order: .msb_first) {
		assert false, 'over-subscribed code must error'
	}
}

fn test_incomplete_code_is_allowed() {
	// A single length-2 code under-subscribes the space; that is permitted.
	t := build(lengths: [2], max_bits: 2, bit_order: .msb_first)!
	assert t.codes[0] == 0
}

fn test_flat_table_rejects_wide_codes() {
	if _ := flat_table(
		lengths:   [max_flat_bits + 1]
		max_bits:  max_flat_bits + 1
		bit_order: .lsb_first
	)
	{
		assert false, 'flat table must reject max_bits > max_flat_bits'
	}
}
