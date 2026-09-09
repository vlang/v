import hash.adler32

fn test_adler32_rfc_vectors() {
	assert adler32.sum([]u8{}) == u32(0x00000001)
	assert adler32.sum('123456789'.bytes()) == u32(0x091e01de)
	assert adler32.sum('Wikipedia'.bytes()) == u32(0x11e60398)
}

fn test_adler32_basic_aliases() {
	data := 'Hello world!'.bytes()
	expected := u32(0x1d09045e)
	assert adler32.checksum(data) == expected
	assert adler32.sum(data) == expected
}

fn test_adler32_update_matches_full_sum() {
	data := 'streaming adler32 data'.bytes()
	part1 := data[..8]
	part2 := data[8..14]
	part3 := data[14..]

	mut state := u32(1)
	state = adler32.update(state, part1)
	state = adler32.update(state, part2)
	state = adler32.update(state, part3)

	assert state == adler32.sum(data)
}

fn test_adler32_update_state_matches_update() {
	data := ('chunked data '.repeat(80)).bytes()
	mut via_state := u32(1)
	mut via_update := u32(1)
	for chunk_size in [1, 2, 3, 5, 8, 16, 64, 257] {
		mut state_a := u32(1)
		mut state_b := u32(1)
		mut i := 0
		for i < data.len {
			end := if i + chunk_size < data.len { i + chunk_size } else { data.len }
			chunk := data[i..end]
			state_a = adler32.update_state(state_a, chunk)
			state_b = adler32.update(state_b, chunk)
			i = end
		}
		assert state_a == adler32.sum(data)
		assert state_b == adler32.sum(data)
		via_state = state_a
		via_update = state_b
	}
	assert via_state == via_update
}

fn test_adler32_all_bytes() {
	mut all_bytes := []u8{len: 256}
	for i in 0 .. 256 {
		all_bytes[i] = u8(i)
	}
	assert adler32.sum(all_bytes) == u32(0xadf67f81)
}

fn test_adler32_large_input() {
	data := 'a'.repeat(7000).bytes()
	assert adler32.sum(data) == u32(0x1a305cef)
}
