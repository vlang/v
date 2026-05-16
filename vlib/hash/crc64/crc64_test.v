import hash.crc64

fn test_crc64_basic() {
	b1 := 'testing crc64'.bytes()
	sum1 := crc64.sum(b1)
	// Verify it's not zero for non-empty input
	assert sum1 != u64(0)

	c := crc64.new(crc64.ecma)
	b2 := 'testing crc64 again'.bytes()
	sum2 := c.checksum(b2)
	// Different input should yield different checksum (with high probability)
	assert sum2 != sum1
}

fn test_crc64_empty() {
	empty := ''.bytes()
	assert crc64.sum(empty) == u64(0)

	c := crc64.new(crc64.ecma)
	assert c.checksum(empty) == u64(0)
}

fn test_crc64_single_byte() {
	a := 'a'.bytes()
	c := crc64.new(crc64.ecma)
	sum_a := c.checksum(a)
	assert sum_a != u64(0)

	b := 'b'.bytes()
	sum_b := c.checksum(b)
	assert sum_b != sum_a
}

fn test_crc64_roundtrip_known_vector() {
	// Standard test vector: "123456789"
	data := '123456789'.bytes()
	c := crc64.new(crc64.ecma)
	result := c.checksum(data)
	// CRC-64-ECMA-182 check value
	assert result == u64(0x6c40df5f0b497347)

	// Verify consistency
	assert crc64.sum(data) == result
	assert crc64.sum_with_poly(crc64.ecma, data) == result
}

fn test_crc64_binary_input() {
	// Test with all byte values
	data := [u8(0), 1, 2, 255, 128, 42, 0x00, 0xff]
	c := crc64.new(crc64.ecma)
	result := c.checksum(data)
	assert result != u64(0)
}

fn test_crc64_update() {
	data := '123456789'.bytes()
	part1 := data[..4]
	part2 := data[4..]

	c := crc64.new(crc64.ecma)
	mut acc := u64(0)
	acc = c.update(acc, part1)
	acc = c.update(acc, part2)

	assert acc == c.checksum(data)
}

fn test_crc64_streaming_chunk_sizes() {
	data := ('streaming data block '.repeat(64)).bytes()
	c := crc64.new(crc64.ecma)
	expected := c.checksum(data)

	for chunk_size in [1, 2, 3, 5, 7, 16, 31, 64, 128] {
		mut state := u64(0)
		mut start := 0
		for start < data.len {
			end := if start + chunk_size < data.len { start + chunk_size } else { data.len }
			state = c.update_state(state, data[start..end])
			start = end
		}
		assert state == expected
	}
}

fn test_crc64_update_state() {
	data := 'stateful streaming'.bytes()
	part1 := data[..5]
	part2 := data[5..]
	c := crc64.new(crc64.ecma)

	mut state := u64(0)
	state = c.update_state(state, part1)
	state = c.update_state(state, part2)

	assert state == c.checksum(data)
}

fn test_crc64_sum_with_poly() {
	data := 'variant helper'.bytes()
	c := crc64.new(crc64.ecma)
	assert c.checksum(data) == crc64.sum_with_poly(crc64.ecma, data)
	assert crc64.sum(data) == crc64.sum_with_poly(crc64.ecma, data)
}

fn test_crc64_sum_with_poly_custom() {
	data := 'custom poly checksum'.bytes()
	// Use a different polynomial to verify custom path
	poly := u64(0xa6fd4db2ef0b0da9)

	assert crc64.sum_with_poly(poly, data) == crc64.new(poly).checksum(data)
}

fn test_crc64_large_input() {
	// Test with large repetitive data
	large_data := ('large repetitive input '.repeat(1000)).bytes()
	c := crc64.new(crc64.ecma)
	result := c.checksum(large_data)
	assert result != u64(0)

	// Verify consistency with streaming
	mut state := u64(0)
	for i := 0; i < large_data.len; i += 512 {
		end := if i + 512 < large_data.len { i + 512 } else { large_data.len }
		state = c.update_state(state, large_data[i..end])
	}
	assert state == result
}

fn test_crc64_all_bytes() {
	// Create data with all possible byte values
	mut all_bytes := []u8{}
	for b in 0 .. 256 {
		all_bytes << u8(b)
	}
	c := crc64.new(crc64.ecma)
	result := c.checksum(all_bytes)
	assert result != u64(0)

	// Verify it's consistent
	assert crc64.sum(all_bytes) == result
}

fn test_crc64_deterministic() {
	data := 'deterministic test'.bytes()
	c := crc64.new(crc64.ecma)

	result1 := c.checksum(data)
	result2 := c.checksum(data)
	result3 := crc64.sum(data)

	assert result1 == result2
	assert result1 == result3
}

fn test_crc64_prefix_sensitivity() {
	// Verify that different prefixes produce different results
	base := 'test'.bytes()
	c := crc64.new(crc64.ecma)

	sum_base := c.checksum(base)
	sum_t := c.checksum('test_extended'.bytes())
	sum_prefix := c.checksum('prefix_test'.bytes())

	assert sum_base != sum_t
	assert sum_base != sum_prefix
	assert sum_t != sum_prefix
}

fn test_crc64_consistency_across_polys() {
	// If only one poly is defined, at least verify the path works
	data := 'poly path test'.bytes()
	direct := crc64.sum(data)
	via_poly := crc64.sum_with_poly(crc64.ecma, data)
	via_new := crc64.new(crc64.ecma).checksum(data)

	assert direct == via_poly
	assert direct == via_new
}
