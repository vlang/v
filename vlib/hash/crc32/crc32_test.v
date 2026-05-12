import hash.crc32

const reflected_test_polys = [crc32.ieee, crc32.crc32c, crc32.crc32k, crc32.crc32q_reflected]

fn sum_for_reflected_poly(poly u32, data []u8) u32 {
	return match poly {
		crc32.ieee { crc32.sum(data) }
		crc32.crc32c { crc32.sum_crc32c(data) }
		crc32.crc32k { crc32.sum_crc32k(data) }
		crc32.crc32q_reflected { crc32.sum_with_poly(crc32.crc32q_reflected, data) }
		else { panic('unexpected polynomial in test') }
	}
}

fn expected_reflected_crc_123456789(poly u32) u32 {
	return match poly {
		crc32.ieee { u32(0xcbf43926) }
		crc32.crc32c { u32(0xe3069283) }
		crc32.crc32k { u32(0x2d3dd0ae) }
		crc32.crc32q_reflected { u32(0xa9cc8179) }
		else { panic('unexpected polynomial in test') }
	}
}

fn expected_reflected_crc_a(poly u32) u32 {
	return match poly {
		crc32.ieee { u32(0xe8b7be43) }
		crc32.crc32c { u32(0xc1d04330) }
		crc32.crc32k { u32(0x0da2aa8a) }
		crc32.crc32q_reflected { u32(0x248ca0a3) }
		else { panic('unexpected polynomial in test') }
	}
}

fn assert_reflected_poly_paths_match(poly u32, data []u8) {
	c := crc32.new(poly)
	by_new := c.checksum(data)
	assert by_new == crc32.sum_with_poly(poly, data)
	assert by_new == sum_for_reflected_poly(poly, data)
}

fn test_hash_crc32() {
	b1 := 'testing crc32'.bytes()
	sum1 := crc32.sum(b1)
	assert sum1 == u32(1212124400)
	assert sum1.hex() == '483f8cf0'

	c := crc32.new(crc32.ieee)
	b2 := 'testing crc32 again'.bytes()
	sum2 := c.checksum(b2)
	assert sum2 == u32(1420327025)
	assert sum2.hex() == '54a87871'
}

fn test_hash_crc32_variants() {
	data := '123456789'.bytes()
	for poly in reflected_test_polys {
		expected := expected_reflected_crc_123456789(poly)
		assert sum_for_reflected_poly(poly, data) == expected
		assert_reflected_poly_paths_match(poly, data)
	}
}

fn test_hash_crc32q_standard() {
	data := '123456789'.bytes()
	assert crc32.sum_crc32q(data) == u32(0x3010bf7f)
	assert crc32.sum_with_poly(crc32.crc32q, data) == u32(0x3010bf7f)
	assert crc32.sum_crc32q('a'.bytes()) == u32(0xd1112b6b)
}

fn test_hash_crc32_update() {
	data := '123456789'.bytes()
	part1 := data[..4]
	part2 := data[4..]

	c := crc32.new(crc32.ieee)
	mut acc := u32(0)
	acc = c.update(acc, part1)
	acc = c.update(acc, part2)

	assert acc == c.checksum(data)
	assert acc.hex() == 'cbf43926'
}

fn test_hash_crc32_edge_cases() {
	empty := ''.bytes()
	one := 'a'.bytes()
	for poly in reflected_test_polys {
		assert sum_for_reflected_poly(poly, empty) == u32(0)
		assert sum_for_reflected_poly(poly, one) == expected_reflected_crc_a(poly)
	}
	assert crc32.sum_crc32q(empty) == u32(0)
}

fn test_hash_crc32_sum_with_poly() {
	data := 'variant helper'.bytes()
	for poly in reflected_test_polys {
		assert_reflected_poly_paths_match(poly, data)
	}
	assert crc32.sum_with_poly(crc32.crc32q, data) == crc32.sum_crc32q(data)
}

fn test_hash_crc32_sum_with_poly_custom() {
	data := 'custom poly checksum'.bytes()
	poly := u32(0xa833982b)

	assert crc32.sum_with_poly(poly, data) == crc32.new(poly).checksum(data)
}

fn test_hash_crc32_all_polys_consistent() {
	data := 'all polys consistent'.bytes()
	part1 := data[..7]
	part2 := data[7..]

	for poly in reflected_test_polys {
		c := crc32.new(poly)
		full := c.checksum(data)

		mut split := u32(0)
		split = c.update(split, part1)
		split = c.update(split, part2)

		assert full == split
		assert full == sum_for_reflected_poly(poly, data)
	}
}

fn test_hash_crc32_streaming_chunk_sizes() {
	data := ('streaming data block '.repeat(64)).bytes()
	for poly in reflected_test_polys {
		c := crc32.new(poly)
		expected := c.checksum(data)
		for chunk_size in [1, 2, 3, 5, 7, 16, 31, 64, 128] {
			mut state := ~u32(0)
			mut start := 0
			for start < data.len {
				end := if start + chunk_size < data.len { start + chunk_size } else { data.len }
				state = c.update_state(state, data[start..end])
				start = end
			}
			assert ~state == expected
		}
	}
}

fn test_hash_crc32_update_state() {
	data := 'stateful streaming'.bytes()
	part1 := data[..5]
	part2 := data[5..]
	c := crc32.new(crc32.crc32c)

	mut state := ~u32(0)
	state = c.update_state(state, part1)
	state = c.update_state(state, part2)

	assert ~state == c.checksum(data)
}
