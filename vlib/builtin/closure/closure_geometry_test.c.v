module closure

fn test_arm64_closure_literal_loads_address_metadata_for_supported_page_sizes() {
	for page_size in [4096, 16384, 65536] {
		for instruction in 0 .. 2 {
			word := closure_arm64_template_word(page_size, instruction)
			// Decode the signed 19-bit literal displacement independently of its
			// encoder. PC advances four bytes; metadata advances eight bytes.
			displacement := (i32(word << 8) >> 13) * 4
			pc := page_size + instruction * 4
			assert pc + displacement == instruction * 8
			assert word & 0xff00001f == if instruction == 0 {
				u32(0x5c000011)
			} else {
				u32(0x58000010)
			}
		}
	}
	assert closure_arm64_template_word(16384, 0) == 0x5cfe0011
	assert closure_arm64_template_word(16384, 1) == 0x58fe0030
}
